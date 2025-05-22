# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

### Stage 1: estimate selection
estimate_selection<-function(leedata,form=NULL,selection_function=NULL,selection_function_name="glmnet",variables_for_selection=NULL,names_to_include=c(),
                             treat_name="treat+",yname="selection",myweights=NULL,...) {
  ### read in data
  d<-leedata$treat
  s<-leedata$selection
  sy<-leedata$outcome


  ### set variables for regression
  if (is.null(variables_for_selection)) {
    variables_for_selection<-setdiff(colnames(leedata),c("outcome","X.Intercept.","(Intercept)"))
  } else {
    variables_for_selection<-unique(c("treat","selection",setdiff(variables_for_selection,c("outcome","X.Intercept.","(Intercept)"))))

  }

  if (is.null(myweights)) {
    myweights<-rep(1,dim(leedata)[1])
  }

  ## TODO: Replace with glmnet(..., alpha=1, family="binomial")
  if (selection_function_name=="rlassologit") {
      glm.fit<-rlassologit(form, leedata[,variables_for_selection],family="binomial",...)
      # select non-zero coefficients
      non_zero_coefs<-glm.fit$coefficients[glm.fit$coefficients!=0]
      # names whose coefs are non-zero
      selected_names<-setdiff(names(non_zero_coefs),c("(Intercept)","treat"))
      # add manually selected features
      selected_names<-unique(c(selected_names,names_to_include))
      ## (optional): add raw variables after interactions
      #grep("treat:",selected_names,value=TRUE)

      # if treatment was dropped, make sure to re-run low-dim analysis with treatment
      if (length(selected_names)>0) {
        form<-as.formula(paste0(yname,"~",treat_name,paste0(selected_names,collapse="+")))
      } else {
        form<-as.formula(paste0(yname,"~",treat_name))
      }
  } else if (selection_function_name=="glmnet") {
      glm.fit<-glmnet(form, leedata[,variables_for_selection],family="binomial",...)
      # select non-zero coefficients
      non_zero_coefs<-glm.fit$coefficients[glm.fit$coefficients!=0]
      # names whose coefs are non-zero
      selected_names<-setdiff(names(non_zero_coefs),c("(Intercept)","treat"))
      # add manually selected features
      selected_names<-unique(c(selected_names,names_to_include))
      ## (optional): add raw variables after interactions
      #grep("treat:",selected_names,value=TRUE)

      # if treatment was dropped, make sure to re-run low-dim analysis with treatment
      if (length(selected_names)>0) {
        form<-as.formula(paste0(yname,"~",treat_name,paste0(selected_names,collapse="+")))
      } else {
        form<-as.formula(paste0(yname,"~",treat_name))
      }
  }

  ### final stage is always logistic with low-dim covariates
  leedata$myweights<-myweights
  glm.postlasso<-glm( form,data=leedata[,c("myweights",variables_for_selection)],family="binomial",weights = myweights)


  return(glm.postlasso)
}

predict_selection<-function(fit,leedata,...) {
  leedata_0treat<-leedata
  leedata_0treat$treat<-0

  leedata_1treat<-leedata
  leedata_1treat$treat<-1

  s.0.hat<-predict( fit,leedata_0treat,type="response")
  s.1.hat<-predict( fit,leedata_1treat,type="response")

  return(list(s.0.hat=s.0.hat,s.1.hat=s.1.hat))
}

### Stage 1: estimate conditional quantile


estimate_quantile_regression<-function(training_data,test_data,variables_for_outcome,distribution_functionname="rq",quantile_grid_size,myweights=NULL,...) {
  print(variables_for_outcome)
  variables_for_outcome<-unique(c("outcome",setdiff(variables_for_outcome,c("treat","selection"))))
  #print(variables_for_outcome)
  #p<-length()
  taus=seq(0,1,quantile_grid_size)
  estimated_quantiles<-matrix(0,dim(test_data)[1],length(taus))

  if (is.null(myweights)) {
    myweights<-rep(1,dim(training_data)[1])
  }
  training_data$myweights<-myweights
  ## everything else requires distribution regression
  if (distribution_functionname=="rq") {
    for (i in 1:(length(taus))) {

      tau<-taus[i]
      q_model<-quantreg::rq(outcome~.,data=training_data[,variables_for_outcome],tau=tau,weights=myweights )
      estimated_quantiles[,i]<-predict(q_model,test_data[,variables_for_outcome])
    }
  } else {
    stop("Other quantile regression functions not currently supported")
    #     variables_for_outcome<-unique(setdiff(variables_for_outcome,c("treat","selection","outcome")))
    #     #stop ("Unsupported estimator of quantile regression")
    #     for (i in 1:(length(taus))) {

    #       tau<-taus[i]
    #       q_model<-rqPen::rq.lasso.fit(x=as.matrix(training_data[,variables_for_outcome]),y=training_data$outcome,tau=tau,lambda=0.0001)
    #       estimated_quantiles[,i]<-predict(q_model,as.matrix(test_data[,variables_for_outcome]))
    #     }
  }

  return(estimated_quantiles)
}

evaluate_quantile<-function(quantile_table,p.0.hat,quantile_grid_size,...) {
  taus=seq(0,1,quantile_grid_size)
  y.p0.hat<-rep(0,dim(quantile_table)[1])
  for (i in 1:length(taus)) {
    # print(i)
    tau<-taus[i]
    inds<-abs(p.0.hat-tau)<=quantile_grid_size
    y.p0.hat[inds]<-quantile_table[inds,i]
  }



  return(y.p0.hat)
}

evaluate_quantile_p_1_p<-function(quantile_table,p.0.hat,min_wage=NULL,max_wage=NULL,...) {
  ### sort quantile table

  for (obs in 1:dim(quantile_table)[1]) {
    quantile_table[obs,]<-sort(quantile_table[obs,])

  }
  #
  y.p0.hat<-evaluate_quantile(quantile_table,p.0.hat,...)
  y.1.p0.hat<-evaluate_quantile(quantile_table,1-p.0.hat,...)
  if (!is.null(min_wage)) {
    y.p0.hat<-sapply(y.p0.hat,max,min_wage)
    y.1.p0.hat<-sapply(y.1.p0.hat,max,min_wage)
  }
  if (!is.null(max_wage)) {
    y.p0.hat<-sapply(y.p0.hat,min,max_wage)
    y.1.p0.hat<-sapply(y.1.p0.hat,min,max_wage)

  }

  return(data.frame(y.p0.hat=y.p0.hat, y.1.p0.hat= y.1.p0.hat))
}

standardize<-function(vec) {
  if (sd(vec)>0) {
    vec<-(vec-mean(vec))/sd(vec)
  }
  return(vec)
}

main_bb<-function(mydata,N_rep=10,function_name,...) {

  ATE_bb<-matrix(0,N_rep,2)
  sample_size<-dim(mydata)[1]
  for (b in 1:N_rep) {
    set.seed(b)
    #print(b)
    inds<-sample(1:sample_size,sample_size,replace=TRUE)
    mydatab<-mydata[inds,]
    resultb = try(function_name  (mydatab,...))
    ATE_bb[b,]<-c(resultb$lower_bound, resultb$upper_bound)

  }
  return( ATE_bb)
}



compute_confidence_region<-function(ATE_boot, ATE_est, ci_alpha=0.05,tol=1e-5) {
  Omega.hat<-matrix(0,2,2)
  if (sum(is.na(ATE_boot))+sum(is.na(ATE_est))>0) {
    return(c(lower_bound = NA, upper_bound=NA))
  }
  ATE_boot_centered<-matrix(0,dim(ATE_boot)[1],2)
  ## Centered draws of lower bound
  ATE_boot_centered[,1]<-ATE_boot[,1]-ATE_est[1]
  ## Centered draws of upper bound
  ATE_boot_centered[,2]<-ATE_boot[,2]-ATE_est[2]

  Omega.hat[1,1]<-var( ATE_boot_centered[,1])
  Omega.hat[2,2]<-var(ATE_boot_centered[,2])
  Omega.hat[1,2]<-cov(ATE_boot_centered[,1],ATE_boot_centered[,2])
  Omega.hat[2,1]<-Omega.hat[1,2]

  crit.val<-sqrtm(Omega.hat)%*% c(-qnorm(sqrt(1-ci_alpha)), qnorm(sqrt(1-ci_alpha)) )
  if (max(abs(Im(sqrtm(Omega.hat))))>tol) {
    stop ("Non-trivial imaginary part!")
  } else {
    crit.val<-sapply( crit.val,Re)

  }
  lower_bound<-ATE_est[1]+ crit.val[1]
  upper_bound<-ATE_est[2] +crit.val[2]
  ## double checking
  # sum( ATE_boot_centered[,1]>crit.val[1] & ATE_boot_centered[,2]< crit.val[2])
  #return ( c( sd(ATE[,1]), sd(ATE[,2])))

  return(c(lower_bound = lower_bound, upper_bound=upper_bound))
}


weighted_bb<-function(mydata,B,function_name,...) {
  ATE_bb<-matrix(0,2,B)
  sample_size<-dim(mydata)[1]
  set.seed(1)
  # exp (1) weights
  weights<-matrix(rexp(sample_size*B),nrow=sample_size,ncol=B)
  # norm(1) weights
  #weights<-matrix(rnorm(sample_size*B,mean=1,sd=1),nrow=sample_size,ncol=B)

  for (b in 1:B) {
    #print(b)
    weights[,b]<-weights[,b]/mean(weights[,b])
    resultb<-try(function_name(mydata,weights=weights[,b],...))

    ATE_bb[,b]<-GetBounds(resultb)
  }


  return(ATE_bb)


}


summary_stat_nonmonotone<-function(leedata_cov,p.0.star) {
  inds_helps<-(p.0.star<=1)
  inds_hurts<-(p.0.star>1)

  participation_rate_X0_treat<-mean(leedata_cov$selection[leedata_cov$treat==1 & inds_helps ])
  participation_rate_X0_control<-mean(leedata_cov$selection[leedata_cov$treat==0 & inds_helps  ])

  delta_X0 = participation_rate_X0_treat-participation_rate_X0_control

  participation_rate_X1_treat<-mean(leedata_cov$selection[leedata_cov$treat==1 & inds_hurts])
  participation_rate_X1_control<-mean(leedata_cov$selection[leedata_cov$treat==0 & inds_hurts  ])

  delta_X1=participation_rate_X1_treat-participation_rate_X1_control
  stats<-list(delta_X0=delta_X0, delta_X1=delta_X1, prop_treat_helps=mean(inds_helps))
  stats<-lapply(stats,round,3)
  return(stats)
}

solve_im<-function(x,IM,ci_alpha) {
  return ( pnorm(x+IM)-pnorm(-x)-(1-ci_alpha))
}

imbens_manski<-function(ATE_boot, ATE_est, ci_alpha=0.05) {
  Omega.hat<-matrix(0,2,2)
  if (sum(is.na(ATE_boot))+sum(is.na(ATE_est))>0) {
    return(c(lower_bound = NA, upper_bound=NA))
  }
  ATE_boot_centered<-matrix(0,dim(ATE_boot)[1],2)
  ## Centered draws of lower bound
  ATE_boot_centered[,1]<-ATE_boot[,1]-ATE_est[1]
  ## Centered draws of upper bound
  ATE_boot_centered[,2]<-ATE_boot[,2]-ATE_est[2]

  Omega.hat[1,1]<-var( ATE_boot_centered[,1])
  Omega.hat[2,2]<-var(ATE_boot_centered[,2])
  Omega.hat[1,2]<-cov(ATE_boot_centered[,1],ATE_boot_centered[,2])
  Omega.hat[2,1]<-Omega.hat[1,2]

  width=ATE_est[2]-ATE_est[1]
  IM=width/max(sqrt(Omega.hat[1,1]),sqrt(Omega.hat[2,2]))
  C<-nleqslv::nleqslv(0,solve_im,IM=IM,ci_alpha=ci_alpha)

  lower_bound<-ATE_est[1]- C$x*sqrt(Omega.hat[1,1])
  upper_bound<-ATE_est[2] +C$x*sqrt(Omega.hat[2,2])


  return(c(lower_bound=lower_bound,upper_bound=upper_bound))

}

print_table<-function(estimates,sd,im=NULL,digs=3) {

  estimates<-apply(estimates,2,round,digs)
  sd<-apply(sd,2,round,digs)
  if (! is.null(im)) {
    im<-apply(im,2,round,digs)
    M<-matrix(NA,dim(estimates)[1]*3,dim(estimates)[2]/2)
    for (j in 1:dim(estimates)[1]) {
      for (k in 1:(dim(estimates)[2]/2)) {
        print(k)
        M[3*j-2,k]<-paste0("[", estimates[j,2*k-1],", " ,estimates[j,2*k],"]")
        M[3*j-1,k]<-paste0("(", sd[j,2*k-1],", " ,sd[j,2*k],")")
        M[3*j,k]<-paste0("(", im[j,2*k-1],", " ,im[j,2*k],")")
      }
    }
  } else {
    M<-matrix(NA,dim(estimates)[1]*2,dim(estimates)[2]/2)
    for (j in 1:dim(estimates)[1]) {
      for (k in 1:(dim(estimates)[2]/2)) {
        print(k)
        M[2*j-1,k]<-paste0("[", estimates[j,2*k-1],", " ,estimates[j,2*k],"]")
        M[2*j,k]<-paste0("(", sd[j,2*k-1],", " ,sd[j,2*k],")")
      }
    }
  }


  return(M)
}
