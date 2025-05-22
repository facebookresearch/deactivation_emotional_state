# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

ortho_leebounds <- function(leedata_cov,s.hat=NULL,s_min=0.001,...) {

    covars <- setdiff(colnames(leedata_cov),c("treat", "selection", "outcome", "weights"))

    form_nonmonotone <- as.formula(paste0("selection ~ treat *(",paste0(covars,collapse="+"), ")"))
    glm.fit <- glm(
        form=form_nonmonotone,
        data=leedata_cov[, setdiff(colnames(leedata_cov),"outcome")],
        family="binomial"
    )

    s.hat <- predict_selection(glm.fit, leedata_cov[,c("treat","selection",covars)])
    s.hat <- data.frame(s.0.hat=s.hat$s.0.hat, s.1.hat=s.hat$s.1.hat)

    fs <- first_stage_wrapper(
        leedata_cov,
        variables_for_outcome=covars,
        s.hat=s.hat,
        ...
    )
    s.hat <- fs$s.hat
    s.hat$s.0.hat <- sapply(s.hat$s.0.hat,max,s_min)
    s.hat$s.1.hat <- sapply(s.hat$s.1.hat,max,s_min)

    res <- second_stage_wrapper(
        leedata=leedata_cov,
        inds_helps=fs$inds_helps,
        y.hat=fs$y.hat,
        s.hat=s.hat,
        ...
    )
    res$inds_helps <- fs$inds_helps
    res$y.hat <- fs$y.hat
    res$s.hat <- s.hat
    return (res)

}



### wrapper functions to estimate first and second stage
### first stage
first_stage_wrapper <- function(leedata_cov,
                              variables_for_outcome,
                              s.hat=NULL,
                              weights=NULL,
                              p0_cutoff=0.94,
                              quantile_grid_size=0.01,
                              sort_quantiles=TRUE,
                              ...) {
  sample_size <- dim(leedata_cov)[1]
  if (is.null(weights)) {
    weights <- rep(1,sample_size)
  }

  if (sum(is.na(weights))>0) {
    stop ("NA weights!")
  }
  p = dim(leedata_cov)[2]-3


  if (!is.null(s.hat)) {
    s.0.hat <- s.hat$s.0.hat
    s.1.hat <- s.hat$s.1.hat
  } else {
    glm.fit <- estimate_selection(leedata_cov,...)
    res_selection <- predict_selection(glm.fit,leedata_cov,...)
    s.0.hat <- res_selection$s.0.hat
    s.1.hat <- res_selection$s.1.hat
    s.hat <- data.frame(s.0.hat=s.0.hat,s.1.hat=s.1.hat)
    p.0.star <- s.0.hat/s.1.hat
  }

  p.0.star <- (s.0.hat/s.1.hat)
  inds_helps <- (p.0.star<=1)
  inds_hurts <- (p.0.star>1)

  if (mean(inds_helps)>=p0_cutoff) {
    p.0.star <- sapply(p.0.star,min,0.9999)
  }

  if (mean(p.0.star>1)>=p0_cutoff) {
    print("cutoff")
    p.0.star <- sapply(p.0.star,max,1.0001)
  }
  inds_helps <- (p.0.star<=1)
  inds_hurts <- (p.0.star>1)

  y.hat <- data.frame(y.p0.hat=rep(NA,sample_size),y.1.p0.hat=rep(NA,sample_size))
  if (sum(inds_helps)>0) {


    estimated_quantiles_11 <- estimate_quantile_regression(training_data=leedata_cov[leedata_cov$treat==1 & leedata_cov$selection==1,],
                                                         test_data=leedata_cov,
                                                         variables_for_outcome=variables_for_outcome,quantile_grid_size=quantile_grid_size,
                                                         myweights=weights[leedata_cov$treat==1 & leedata_cov$selection==1],...  )
    y.hat.helps=evaluate_quantile_p_1_p(taus=taus,quantile_table=estimated_quantiles_11[inds_helps,],p.0.hat=p.0.star[inds_helps],
                                        quantile_grid_size=quantile_grid_size,sort_quantiles=sort_quantiles,...)
    y.hat$y.p0.hat[inds_helps] <- y.hat.helps$y.p0.hat
    y.hat$y.1.p0.hat[inds_helps] <- y.hat.helps$y.1.p0.hat
  } else {
    estimated_quantiles_11 <- NULL
  }

  if (sum(inds_hurts)>0) {
    estimated_quantiles_10 <- estimate_quantile_regression(training_data=leedata_cov[leedata_cov$treat==0 & leedata_cov$selection==1,],
                                                         test_data=leedata_cov,
                                                         variables_for_outcome=variables_for_outcome,
                                                         quantile_grid_size=quantile_grid_size,
                                                         myweights=weights[leedata_cov$treat==0 & leedata_cov$selection==1],...  )
    y.hat.hurts=evaluate_quantile_p_1_p(taus=taus,quantile_table=estimated_quantiles_10[inds_hurts,],p.0.hat=1/p.0.star[inds_hurts],
                                        quantile_grid_size=quantile_grid_size,...)
    y.hat$y.p0.hat[inds_hurts] <- y.hat.hurts$y.p0.hat
    y.hat$y.1.p0.hat[inds_hurts] <- y.hat.hurts$y.1.p0.hat
  }
  else {
    estimated_quantiles_10 <- NULL
  }

  return(list(inds_helps=inds_helps,
              y.hat=y.hat,
              s.hat=s.hat))
}

### second stage
second_stage_wrapper <- function(leedata,
                                 inds_helps,
                                 y.hat,
                                 s.hat,
                                 weights=NULL,
                                 bounds_fun=ortho_bounds_ss_wt,
                                 tol=1e-5,
                                 ci_alpha=0.05,
                                 props_list=NULL,
                                 ...) {

  flag_props_list_null <- is.null(props_list)
  if (is.null(weights)) {
    weights <- rep(1,dim(leedata)[1])
  }
  weights <- weights/sum(weights)

  sample_size <- dim(leedata)[1]
  inds_hurts <- !inds_helps
  if (sum(inds_helps)>0){
    if (flag_props_list_null) {
      prop0 <- weighted.mean(leedata$treat[inds_helps]==0,weights[inds_helps])
      prop1 <- weighted.mean(leedata$treat[inds_helps]==1,weights[inds_helps])
      prop10 <- weighted.mean(leedata$treat[inds_helps]==0 & leedata$selection[inds_helps] == 1,weights[inds_helps])
      prop11 <- weighted.mean(leedata$treat[inds_helps]==1 & leedata$selection[inds_helps] == 1,weights[inds_helps])
      props_list_helps=list(prop0=prop0,prop1=prop1,prop10=prop10,prop11=prop11)

    } else {
      prop0 <- props_list$prop0[inds_helps]
      prop1 <- props_list$prop1[inds_helps]
      prop10 <- weighted.mean(leedata$treat[inds_helps]==0 & leedata$selection[inds_helps] == 1,weights[inds_helps])
      prop11 <- weighted.mean(leedata$treat[inds_helps]==1 & leedata$selection[inds_helps] == 1,weights[inds_helps])
      props_list_helps=list(prop0=prop0,prop1=prop1,prop10=prop10,prop11=prop11)
    }

    res_helps <- bounds_fun(leedata=leedata[ inds_helps,],
                          treat_helps = TRUE,
                          s.hat=s.hat[inds_helps,],
                          weights=weights[inds_helps],y.hat=y.hat[inds_helps,],props_list=props_list_helps,...)
    estimated_bounds_helps <- GetBounds(lapply(res_helps,weighted.mean,w=weights[inds_helps]))
    bounds <- cbind(res_helps$lower_bound,res_helps$upper_bound)
    wtcov <- cov.wt(bounds,wt=weights[inds_helps])

    cov_helps <- wtcov$cov
  }
  else {
    res_helps <- NULL
    estimated_bounds_helps <- c(0,0)
    cov_helps <- matrix(0,2,2)
  }


  if (sum(inds_hurts)>0){
    if (flag_props_list_null) {
      prop0 <- weighted.mean(leedata$treat[inds_hurts]==0,weights[inds_hurts])
      prop1 <- weighted.mean(leedata$treat[inds_hurts]==1,weights[inds_hurts])
      prop10 <- weighted.mean(leedata$treat[inds_hurts]==0 & leedata$selection[inds_hurts] == 1,weights[inds_hurts])
      prop11 <- weighted.mean(leedata$treat[inds_hurts]==1 & leedata$selection[inds_hurts] == 1,weights[inds_hurts])
       props_list_hurts=list(prop0=prop0,prop1=prop1,prop10=prop10,prop11=prop11)

    }else {
      prop0 <- props_list$prop0[inds_hurts]
      prop1 <- props_list$prop1[inds_hurts]
      prop10 <- weighted.mean(leedata$treat[inds_hurts]==0 & leedata$selection[inds_hurts] == 1,weights[inds_hurts])
      prop11 <- weighted.mean(leedata$treat[inds_hurts]==1 & leedata$selection[inds_hurts] == 1,weights[inds_hurts])
      props_list_hurts=list(prop0=prop0,prop1=prop1,prop10=prop10,prop11=prop11)
    }

    res_hurts <- bounds_fun (leedata=leedata[ inds_hurts,],
                           treat_helps = FALSE,
                           s.hat=s.hat[inds_hurts,],
                           y.hat=y.hat[inds_hurts,],weights=weights[inds_hurts],props_list=props_list_hurts,...)

    estimated_bounds_hurts <- GetBounds(lapply(res_hurts,weighted.mean,w=weights[inds_hurts]))
    bounds <- cbind(res_hurts$lower_bound,res_hurts$upper_bound)
    wtcov <- cov.wt(bounds,wt=weights[inds_hurts])

    cov_hurts <- wtcov$cov

  }else {
    res_hurts <- NULL
    cov_hurts <- matrix(0,2,2)
    estimated_bounds_hurts <- c(0,0)
  }





  bounds <- (sum(weights[inds_helps])*estimated_bounds_helps+sum(weights[inds_hurts])*estimated_bounds_hurts)
  Omega.hat <-  (sum(weights[inds_helps])*cov_helps+sum(weights[inds_hurts])*cov_hurts)

  crit.val <- sqrtm(Omega.hat)%*% c(-qnorm(sqrt(1-ci_alpha)), qnorm(sqrt(1-ci_alpha)) )
  if (max(abs(Im(sqrtm(Omega.hat))))>tol) {
    stop ("Non-trivial imaginary part!")
  } else {
    crit.val <- sapply( crit.val,Re)

  }
  lower_bound_ci <- bounds[1]+ crit.val[1]/sqrt(sample_size)
  upper_bound_ci <- bounds[2] +crit.val[2]/sqrt(sample_size)

  return(list(lower_bound=bounds[1],
              upper_bound=bounds[2],
              estimated_bounds_helps=estimated_bounds_helps,
              estimated_bounds_hurts=estimated_bounds_hurts,
              lower_bound_ci=lower_bound_ci,
              upper_bound_ci=upper_bound_ci))


}



### summary for subjects with positive lower bound


summary_subjects_positive_lower_bound <- function(leedata_cov_total,
                                                weights=NULL,
                                                y.hat=NULL,
                                                s.hat=NULL,eps=0.00001,max_p_hat=0.94,
                                                form_outcome=as.formula("outcome~."),form_selection=NULL,cutoff_positive=0.07,
                                                ...) {


  if (is.null(weights)) {
    weights_total <- rep(1,dim(leedata_cov_total)[1])
  } else {
    weights_total <- weights
  }

  if (sum(is.na(weights))>0) {
    stop ("NA weights!")
  }

  s.hat$s.0.hat <- sapply(s.hat$s.0.hat,max,eps)
  s.hat$s.1.hat <- sapply(s.hat$s.1.hat,max,eps)
  p.0.star <- s.hat$s.0.hat/s.hat$s.1.hat
  inds_helps <- p.0.star<=1
  inds_hurts <- !inds_helps

  if (mean(inds_helps)>max_p_hat) {
    p.0.star <- sapply(p.0.star,min,1)
  }
  inds_helps <- p.0.star<=1
  inds_hurts <- p.0.star>1

  if (mean(inds_helps)<1-max_p_hat) {
    p.0.star <- sapply(p.0.star,max,1.010101)
  }

  inds_helps <- p.0.star<=1
  inds_hurts <- p.0.star>1

  is_positive_total <- rep(NA,dim(leedata_cov_total)[1])
  if (sum(inds_helps)>0){
    leedata_cov <- leedata_cov_total[inds_helps,]
    weights <- weights_total[inds_helps]

    d <- leedata_cov$treat
    s <- leedata_cov$selection
    sy <- leedata_cov$outcome
    leedata_cov$weights <- weights
    leedata_cov$y.p0.hat <- y.hat$y.p0.hat[inds_helps]
    leedata_cov$y.1.p0.hat <- y.hat$y.1.p0.hat[inds_helps]

    prop0 <- stats::weighted.mean(s[d==0]==1,w=weights[d==0])


    lm0.fit <- lm(formula=form_outcome,data=leedata_cov,
                subset=(selection==1 & treat==0),
                weights = weights)
    y0.hat <- predict(lm0.fit,leedata_cov)
    lm1.fit <- lm(formula=form_outcome,
                leedata_cov,
                subset=selection==1  & treat==1 & outcome<=y.p0.hat,
                weights=weights)
    y1.hat <- predict( lm1.fit, leedata_cov)
    treat_effect_lower_bound <- y1.hat-y0.hat
    is_positive <- treat_effect_lower_bound>cutoff_positive
    is_positive_total[inds_helps] <- is_positive
  }
    if (sum(inds_hurts)>0) {


      leedata_cov <- leedata_cov_total[inds_hurts,]
      weights <- weights_total[inds_hurts]
      leedata_cov$weights <- weights
      d <- leedata_cov$treat
      s <- leedata_cov$selection
      sy <- leedata_cov$outcome

      leedata_cov$y.p0.hat <- y.hat$y.p0.hat[inds_hurts]
      leedata_cov$y.1.p0.hat <- y.hat$y.1.p0.hat[inds_hurts]
      leedata_cov$weights <- weights

      prop1 <- stats::weighted.mean(s[d==1]==1,w=weights[d==1])


      lm1.fit <- lm(formula=form_outcome,
                  data=leedata_cov,
                  subset= selection==1 & treat==1,
                  weights=weights)
      y1.hat <- predict(lm1.fit,leedata_cov)

      lm0.fit <- lm(formula=form_outcome,
                  data=leedata_cov,
                  subset=selection==1  & treat==0 & outcome>=y.1.p0.hat,
                  weights=weights)
      y0.hat <- predict( lm0.fit, leedata_cov)
      treat_effect_lower_bound <- y1.hat-y0.hat
      is_positive <- treat_effect_lower_bound>cutoff_positive
      is_positive_total[inds_hurts] <- is_positive
    }
    return(is_positive_total)

}

treatment_effect_lower_bound <- function(leedata_cov_total,
                                                weights=NULL,
                                                y.hat=NULL,
                                                s.hat=NULL,eps=0.00001,max_p_hat=0.94,
                                                form_outcome=as.formula("outcome~."),form_selection=NULL,cutoff_positive=0.07,
                                                ...) {


  if (is.null(weights)) {
    weights_total <- rep(1,dim(leedata_cov_total)[1])
  } else {
    weights_total <- weights
  }

  if (sum(is.na(weights))>0) {
    stop ("NA weights!")
  }

  s.hat$s.0.hat <- sapply(s.hat$s.0.hat,max,eps)
  s.hat$s.1.hat <- sapply(s.hat$s.1.hat,max,eps)
  p.0.star <- s.hat$s.0.hat/s.hat$s.1.hat
  inds_helps <- p.0.star<=1
  inds_hurts <- !inds_helps

  if (mean(inds_helps)>max_p_hat) {
    p.0.star <- sapply(p.0.star,min,1)
  }
  inds_helps <- p.0.star<=1
  inds_hurts <- p.0.star>1

  if (mean(inds_helps)<1-max_p_hat) {
    p.0.star <- sapply(p.0.star,max,1.010101)
  }

  inds_helps <- p.0.star<=1
  inds_hurts <- p.0.star>1

  is_positive_total <- rep(NA,dim(leedata_cov_total)[1])
  treat_effect_lower_bound_total <- rep(NA,dim(leedata_cov_total)[1])
  if (sum(inds_helps)>0){
    leedata_cov <- leedata_cov_total[inds_helps,]
    weights <- weights_total[inds_helps]

    d <- leedata_cov$treat
    s <- leedata_cov$selection
    sy <- leedata_cov$outcome
    leedata_cov$weights <- weights
    leedata_cov$y.p0.hat <- y.hat$y.p0.hat[inds_helps]
    leedata_cov$y.1.p0.hat <- y.hat$y.1.p0.hat[inds_helps]

    prop0 <- stats::weighted.mean(s[d==0]==1,w=weights[d==0])


    lm0.fit <- lm(formula=form_outcome,data=leedata_cov,
                subset=(selection==1 & treat==0),
                weights = weights)
    y0.hat <- predict(lm0.fit,leedata_cov)
    lm1.fit <- lm(formula=form_outcome,
                leedata_cov,
                subset=selection==1  & treat==1 & outcome<=y.p0.hat,
                weights=weights)
    y1.hat <- predict( lm1.fit, leedata_cov)
    treat_effect_lower_bound <- y1.hat-y0.hat
    is_positive <- treat_effect_lower_bound>cutoff_positive
    is_positive_total[inds_helps] <- is_positive
    treat_effect_lower_bound_total[inds_helps] <- treat_effect_lower_bound
  }
  if (sum(inds_hurts)>0) {


    leedata_cov <- leedata_cov_total[inds_hurts,]
    weights <- weights_total[inds_hurts]
    leedata_cov$weights <- weights
    d <- leedata_cov$treat
    s <- leedata_cov$selection
    sy <- leedata_cov$outcome

    leedata_cov$y.p0.hat <- y.hat$y.p0.hat[inds_hurts]
    leedata_cov$y.1.p0.hat <- y.hat$y.1.p0.hat[inds_hurts]
    leedata_cov$weights <- weights

    prop1 <- stats::weighted.mean(s[d==1]==1,w=weights[d==1])


    lm1.fit <- lm(formula=form_outcome,
                data=leedata_cov,
                subset= selection==1 & treat==1,
                weights=weights)
    y1.hat <- predict(lm1.fit,leedata_cov)

    lm0.fit <- lm(formula=form_outcome,
                data=leedata_cov,
                subset=selection==1  & treat==0 & outcome>=y.1.p0.hat,
                weights=weights)
    y0.hat <- predict( lm0.fit, leedata_cov)
    treat_effect_lower_bound <- y1.hat-y0.hat
    is_positive <- treat_effect_lower_bound>cutoff_positive
    is_positive_total[inds_hurts] <- is_positive
    treat_effect_lower_bound_total[inds_hurts] <- treat_effect_lower_bound
  }
  return(list(treat_effect_lower_bound=treat_effect_lower_bound_total))

}


ortho_bounds_nontreated_wage_ss <- function(leedata,treat_helps,weights,ortho=TRUE,...) {

  d <- leedata$treat
  s <- leedata$selection
  sy <- s*leedata$outcome
  weights <- weights/sum(weights)

  if (treat_helps) {
    prop10 <- weighted.mean(d==0 & s == 1,weights)
    moment_upper <- sy*(1-d)/prop10
    moment_lower <- sy*(1-d)/prop10
    trimmed_mean_lower <- sy*(1-d)/prop10
    trimmed_mean_upper <- sy*(1-d)/prop10

   } else {


    if (ortho==TRUE) {
      correction <- orthogonal_correction(leedata=leedata,treat_helps=treat_helps,...)
    } else {
      correction <- list(lower_trim_correction=0,upper_trim_correction=0)
    }

    trimmed_moments <- basic_trimmed_moments(treat_helps=treat_helps,leedata=leedata,...)
    trimmed_mean_lower <- trimmed_moments$trimmed_mean_lower
    trimmed_mean_upper <- trimmed_moments$trimmed_mean_upper

    moment_upper <- trimmed_mean_upper + correction$upper_trim_correction
    moment_lower <- trimmed_mean_lower + correction$lower_trim_correction

  }
  return (list(lower_bound=moment_lower,
               upper_bound=moment_upper,
               trimmed_mean_lower=trimmed_mean_lower,
               trimmed_mean_upper=trimmed_mean_upper))
}



ortho_bounds_ss_wt <- function(leedata,treat_helps,weights,ortho=TRUE,...) {

  d <- leedata$treat
  s <- leedata$selection
  sy <- s*leedata$outcome
  weights <- weights/sum(weights)

  if (ortho==TRUE) {
    correction <- orthogonal_correction(leedata=leedata,treat_helps=treat_helps,...)
  } else {
    correction <- list(lower_trim_correction=0,upper_trim_correction=0)
  }

  trimmed_moments <- basic_trimmed_moments(treat_helps=treat_helps,leedata=leedata,...)
  trimmed_mean_lower <- trimmed_moments$trimmed_mean_lower
  trimmed_mean_upper <- trimmed_moments$trimmed_mean_upper

  if (treat_helps) {
    prop10 <- weighted.mean(d==0 & s == 1,weights)
    moment_upper <- trimmed_mean_upper + correction$upper_trim_correction - sy*(1-d)/prop10
    moment_lower <- trimmed_mean_lower + correction$lower_trim_correction - sy*(1-d)/prop10

  }

   else {
    prop11 <- weighted.mean(d==1 & s == 1,weights)
    moment_upper <- sy*(d)/prop11 - trimmed_mean_lower - correction$lower_trim_correction
    moment_lower <- sy*(d)/prop11  - trimmed_mean_upper - correction$upper_trim_correction

   }

  return (list(lower_bound=moment_lower,
               upper_bound=moment_upper,
               trimmed_mean_lower=trimmed_mean_lower,
               trimmed_mean_upper=trimmed_mean_upper))


}

basic_trimmed_moments <- function(leedata,treat_helps,props_list,y.hat,...) {
  ## args: data
  d <- leedata$treat
  s <- leedata$selection
  sy <- s*leedata$outcome

  ## args: first-stage estimate
  y.p0.hat <- y.hat$y.p0.hat
  y.1.p0.hat <- y.hat$y.1.p0.hat
  ## props
  prop0 <- props_list$prop0
  prop1 <- props_list$prop1
  prop10 <- props_list$prop10
  prop11 <- props_list$prop11


  if (treat_helps) {

    trimmed_mean_upper <- (d*s*sy*(sy>=y.1.p0.hat))*prop0/prop1/prop10
    trimmed_mean_lower <- (d*s*sy*(sy<=y.p0.hat))*prop0/prop1/prop10

  } else {
    trimmed_mean_upper <- ((1-d)*s*sy*(sy>=y.1.p0.hat))*prop1/prop0/prop11
    trimmed_mean_lower <- ((1-d)*s*sy*(sy<=y.p0.hat))*prop1/prop0/prop11


  }
  return(list(trimmed_mean_lower=trimmed_mean_lower,
              trimmed_mean_upper=trimmed_mean_upper))
}

orthogonal_correction <- function(leedata,treat_helps,props_list,y.hat,s.hat,c_quant=1,...) {
  d <- leedata$treat
  s <- leedata$selection
  sy <- s*leedata$outcome


  ## compute second stage estimate based on the first stage

  ## args: first-stage estimate
  y.p0.hat <- y.hat$y.p0.hat
  y.1.p0.hat <- y.hat$y.1.p0.hat
  s.0.hat <- s.hat$s.0.hat
  s.1.hat <- s.hat$s.1.hat

  prop0 <- props_list$prop0
  prop1 <- props_list$prop1
  prop10 <- props_list$prop10
  prop11 <- props_list$prop11


  if (treat_helps) {

    p.0.hat <- s.0.hat/s.1.hat
    p.0.hat <- sapply(p.0.hat,min,0.99999)
    # (-1)* (-1) *(1)
    gamma1x <- y.1.p0.hat*prop0/prop10
    # (-1)* (-1) *(-1)
    gamma2x <-  (-1)*(y.1.p0.hat)*p.0.hat*prop0/prop10
    # (-1)* (-1)
    gamma3x <- (y.1.p0.hat)*s.1.hat*prop0/prop10

    # (1)*(1)*(1)
    gamma4x <- y.p0.hat*prop0/prop10
    # (1)*(1)*(-1)
    gamma5x <- (-1)*(y.p0.hat)*p.0.hat*prop0/prop10
    # (1)*(-1)*
    gamma6x <- (-1)*y.p0.hat*s.1.hat*prop0/prop10

    alpha3x <-  d*s*(as.numeric(sy<=y.1.p0.hat) - (1-p.0.hat))/prop1/s.1.hat
    alpha6x <-   d*s*(as.numeric(sy<=y.p0.hat) - p.0.hat)/prop1/s.1.hat

    alpha1x <- (1-d)*s/prop0-s.0.hat
    alpha2x <- d*s/prop1-s.1.hat

    alpha4x <- alpha1x
    alpha5x <- alpha2x


    A1 <- gamma1x*alpha1x
    A2 <- gamma2x*alpha2x
    A3 <- gamma3x*alpha3x*c_quant


    A4 <- gamma4x*alpha4x
    A5 <- gamma5x*alpha5x
    A6 <- gamma6x*alpha6x*c_quant

  } else {

    p.0.hat <- s.1.hat/s.0.hat
    p.0.hat <- sapply(p.0.hat,min,0.99999)


    # (-1)*(-1)*(-1)
    gamma1x <- (-1)*(y.1.p0.hat)*p.0.hat*prop1/prop11
    # (-1)*(-1)*(1)
    gamma2x <- y.1.p0.hat*prop1/prop11
    # (-1)*(-1)
    gamma3x <- (y.1.p0.hat)*s.0.hat*prop1/prop11


    # (1)*(1)*(-1)
    gamma4x <- (-1)*y.p0.hat*p.0.hat*prop1/prop11
    # (1)*(1)*(1)
    gamma5x <- y.p0.hat*prop1/prop11
    # (-1)
    gamma6x <- (-1)*y.p0.hat*s.0.hat*prop1/prop11

    alpha1x <- (1-d)*s/prop0-s.0.hat
    alpha2x <- d*s/prop1-s.1.hat
    alpha3x <- (1-d)*s*(as.numeric(sy<=y.1.p0.hat)- (1-p.0.hat))/s.0.hat/prop0


    alpha4x <- alpha1x
    alpha5x <- alpha2x
    alpha6x <- (1-d)*s*(as.numeric(sy<=y.p0.hat) - p.0.hat)/s.0.hat/prop0

    A1 <- gamma1x*alpha1x
    A2 <- gamma2x*alpha2x
    A3 <- gamma3x*alpha3x*c_quant


    A4 <- gamma4x*alpha4x
    A5 <- gamma5x*alpha5x
    A6 <- gamma6x*alpha6x*c_quant

  }


  lower_trim_correction <- (A4+A5+A6)
  upper_trim_correction <- (A1+A2+A3)
  return(list(lower_trim_correction=lower_trim_correction,
              upper_trim_correction=upper_trim_correction))
}
