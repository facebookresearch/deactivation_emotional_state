# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

## method to get bounds
GetBounds<-function(x) {
  return(c(x$lower_bound,x$upper_bound))
}
GetFraction<-function(x) {
  return (x$fraction)
}

#' @title Estimating Lee (2009) treatment effect bounds
#'
#' @description \code{leebounds} basic Lee (2009) bounds on treatment effect without covariates. Sign of monotonicity is determined by trimming threshold.
#'
#' @usage leebounds(leedata)
#'
#' @param leedata data frame containing three fields
#' \item{res}{leedata$treat: binary treatment indicator}
#' \item{res}{leedata$selection: selection=1 if the outcome is observed }
#' \item{res}{leedata$outcome:   outcome=selection*outcome}
#' @return A list containing the estimate of lower bound and upper bound
#'

leebounds<-function(leedata,treat_helps=NULL,...) {
  # print(treat_helps)
  d<-leedata$treat
  s<-leedata$selection
  sy<-leedata$outcome

  if ("weights" %in% colnames(leedata)) {
    weights<-leedata$weights
  } else {
    weights<-rep(1,length(d))
  }

  # s: binary (1/0)selection
  # sy: outcome; sy=0 if selection s=0, sy=y otherwise
  # normalize weights; assume weights are positive
  treat_size<-sum(d==1)
  control_size<-sum(d==0)
  prop_control_nonmissing<-weighted.mean(s[d==0]==1,weights[d==0])
  prop_treat_nonmissing<-weighted.mean(s[d==1]==1,weights[d==1])



  p0<-prop_control_nonmissing/prop_treat_nonmissing
  #print(p0)
  if (!is.null(treat_helps)) {
    if (treat_helps==TRUE) {
      p0<-sapply(p0,min,1)
    }
    if (treat_helps==FALSE) {
      p0<-sapply(p0,max,1)
    }

  }

  if (prop_treat_nonmissing==0|is.na(p0)) {
    leebounds_result=list(lower_bound=NA,upper_bound=NA)
    return(leebounds_result)
  }
  if (p0<=1) {
    trim_group_inds<-(d==1)
    nontrim_group_inds<-(d==0)
    p0_star = p0
  } else {
    p0_star = 1/p0
    trim_group_inds<-(d==0)
    nontrim_group_inds<-(d==1)
  }

  flag_NA<-FALSE
  if (sum(trim_group_inds &s==1)==0) {
    # stop("No identification: no observed data in trimmed group")
    flag_NA<-TRUE
  }
  if (sum(nontrim_group_inds  &s==1)==0) {
    flag_NA<-TRUE
    # stop("No identification: no observed data in nontrimmed group")
  }
  if (flag_NA) {
    leebounds_result=list(lower_bound=NA,upper_bound=NA)
    return(leebounds_result)
  } else {
    y_trim<-sy[trim_group_inds &s==1]
    y_nontrim<-sy[nontrim_group_inds & s==1]

    weights_trim<-weights[trim_group_inds &s==1]

    if ("weights" %in% colnames(leedata)) {
      yp0<-Hmisc::wtd.quantile(y_trim,probs=p0_star,weights=weights[trim_group_inds &s==1])
      y1p0<-Hmisc::wtd.quantile(y_trim,probs=1-p0_star,weights=weights[trim_group_inds &s==1])

    } else {
      yp0<-stats::quantile(y_trim,p0_star)
      y1p0<-stats::quantile(y_trim,1-p0_star)

    }



    trimmed_mean_upper<-weighted.mean(y_trim[y_trim>=y1p0],weights_trim[y_trim>=y1p0])
    trimmed_mean_lower<-weighted.mean(y_trim[y_trim<=yp0],weights_trim[ y_trim<=yp0])

    if (p0<=1) {
      upper_bound_effect<- trimmed_mean_upper-weighted.mean(y_nontrim,weights[ nontrim_group_inds & s==1])
      lower_bound_effect<- trimmed_mean_lower-weighted.mean(y_nontrim,weights[ nontrim_group_inds & s==1])
    }
    else {
      upper_bound_effect<- weighted.mean(y_nontrim,weights[ nontrim_group_inds & s==1])-trimmed_mean_lower
      lower_bound_effect<- weighted.mean(y_nontrim,weights[ nontrim_group_inds & s==1])-trimmed_mean_upper
    }

    leebounds_result<-list(lower_bound=lower_bound_effect,upper_bound=upper_bound_effect)
    return(list(lower_bound=lower_bound_effect,upper_bound=upper_bound_effect,p0=prop_control_nonmissing/prop_treat_nonmissing,
                trimmed_mean_upper=trimmed_mean_upper,trimmed_mean_lower=trimmed_mean_lower,
                mean_no_trim=mean(y_nontrim),
                odds=treat_size/control_size,
                yp0=yp0,
                y1p0=y1p0,
                s0=prop_control_nonmissing,
                s1=prop_treat_nonmissing,
                prop0=mean(d==0),
                prop1=mean(d==1)))
  }



}


leebounds_wout_monotonicity<-function(leedata,p.0.star) {
  d<-leedata$treat
  if ("weights" %in% colnames(leedata)) {
    weights<-leedata$weights
  } else {
    weights<-rep(1,length(d))
  }

  inds_helps<-(p.0.star<=1)
  inds_hurts<-(p.0.star>1)
  if (sum(inds_helps)>0) {
    res_helps <-leebounds(leedata[inds_helps,])
    bounds_helps<-GetBounds(res_helps)
  } else {
    bounds_helps<-c(0,0)
  }

  if (sum(inds_hurts)>0) {
    res_hurts<-leebounds(leedata[inds_hurts,])
    bounds_hurts<-GetBounds( res_hurts)
  } else {
    bounds_hurts<-c(0,0)
  }

  bounds<-(sum(weights[inds_helps])*bounds_helps+sum(weights[inds_hurts])*bounds_hurts)/sum(weights)
  leebounds_result<-list(lower_bound=bounds[1],upper_bound=bounds[2])
  return(leebounds_result)

}
