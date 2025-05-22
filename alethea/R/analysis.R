# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

#' Create composite scale using factor analysis
#'
#' Create composite scale from multiple survey items using Principal Component
#' Analysis with varimax rotation.
#'
#' @param svy (data frame) Data frame that contains survey data
#' @param items (character vector) List of variables that will be jointly scaled.
#' @param control_group (character) Value of the \code{treatment_group} variable
#' in the \code{svy} data frame that corresponds to the control group in the
#' study. If \code{NULL}, the composite scale will be computed using all
#' observations in the survey dataset.
#' @param nfactors (numeric) Number of components to be extracted. This is only
#' for exploratory purposes; the function will always return a single score.
#' @param impute (Boolean) If \code{TRUE}, impute missing values using the
#' median.
#' @param impute_function (character) Either 'median' or 'mean'. Function used
#' to replace missing values when imputing.
#' @param missingness_report (Boolean) If \code{TRUE}, returns report with
#' details about missing values in each item and across respondents.
#' @param missingness_baseline (character) Either 'study' to compute proportion
#' missing with respect to entire dataset; or 'W2', 'W3', 'W4', 'W5' to compute
#' it with respect to respondents who completed those waves
#' @param verbose (Boolean) If \code{TRUE} (default), print factor loadings.
#'
#' @export

create_composite_scale <- function(svy, items, control_group=NULL,
                                   nfactors=1, impute=FALSE,
                                   impute_function = 'median',
                                   missingness_report = TRUE,
                                   missingness_baseline = 'study',
                                   verbose=TRUE){
  # selecting only respondents in control group, if required
  if (!is.null(control_group)){
    sbs <- svy[svy$treatment_group == control_group,]
  } else {
    sbs <- svy
  }
  # selecting only respondents in relevant wave, if applicable
  if (missingness_baseline == 'study'){
    sbs <- sbs
  }
  if (missingness_baseline == 'W2'){
    sbs <- sbs[sbs$iswave2complete,]
  }
  if (missingness_baseline == 'W3'){
    sbs <- sbs[sbs$iswave3complete,]
  }
  if (missingness_baseline == 'W4'){
    sbs <- sbs[sbs$iswave4complete,]
  }
  if (missingness_baseline == 'W5'){
    sbs <- sbs[sbs$iswave5complete,]
  }
  # Missingness report
  if (missingness_report){
    message("Item non-response (% of respondents missing for each item)")
    print(100*apply(sbs[,items], 2, function(x) mean(is.na(x))) %>% round(3))
    message("Unit non-response (% of respondents with missing responses)")
    print(100*prop.table(table(apply(sbs[,items], 1, function(x) sum(is.na(x))))) %>%
            round(3))
  }
  # running PCA with varimax rotation
  scale_fit <- psych::principal(sbs[,items], nfactors=nfactors,
                                rotate="varimax", missing=impute, impute=impute_function)
  if (verbose) print(scale_fit$loadings)
  # predict for all respondents
  preds <- predict(scale_fit, svy[,items], missing=impute, impute=impute_function)[,1:nfactors]
  return(preds)
}

#' Test for balance across treatment arms
#'
#' Fit OLS regression (one per treatment arm, with a sample that includes
#' the control and that treatment arm) of the treatment indicators on a set
#' of covariates (dummy variables for blocks will automatically be included)
#' and calculate a heteroskedasticity-robust Wald statistic for the hypothesis
#' that all the coefficients on the covariates (excluding block dummies) are zero.
#'
#' @param data (data frame) Data frame that contains survey data
#' @param control (character) Name of control group
#' @param trt (character) Name of treatment group
#' @param covars (character vector) Names of covariates in survey dataset that
#' will be used for the covariate balance test.
#'
#' @export

balance_f_test <- function(data, control, trt, covars){
  # listwise deletion of missing values
  # (necessary as pre-processing step because otherwise models will have different Ns)
  svy_sbs <-  data %>% select(treatment_group, BLOCK_ID, covars) %>% na.omit()
  deleted <- nrow(data)-nrow(svy_sbs)
  message(deleted, ' observations (', round(100*deleted/nrow(data),2) ,'%) deleted due to missingness.')

  # restricted model - only block dummies
  m0 <- paste0('treatment_group == "', trt, '" ~ factor(BLOCK_ID)')
  m0_output <- lm(formula(m0), data=svy_sbs %>% filter(treatment_group %in% c(control, trt)))

  # unrestricted model - block dummies + covariates
  m1 <- paste0('treatment_group == "', trt, '" ~ factor(BLOCK_ID) + ',
               paste(covars, collapse = ' + '))
  m1_output <- lm(formula(m1), data=svy_sbs %>% filter(treatment_group %in% c(control, trt)))

  # computing heteroskedasticity-robust Wald statistic
  w <- waldtest(m0_output, m1_output, vcov = sandwich::vcovHC(m1_output, type = "HC2"))
  message('F statistic: ', round(w$F[2], 3), ' (p=', round(w$`Pr(>F)`[2], 3), ')')
}

#' Permutation Test of Covariate Balance
#'
#' Similar to \code{balance_f_test()}, but here we use a permutation test
#' (randomization inference) to calculate the p-value associated with the
#' Wald statistic.
#' #'
#' @param data (data frame) Data frame that contains survey data
#' @param control (character) Name of control group
#' @param trt (character) Name of treatment group
#' @param covars (character vector) Names of covariates in survey dataset that
#' will be used for the covariate balance test.
#' @param treatment_var (character) Name of treatment group assignment variable.
#' Default "treatment_group"
#' @param sims (numeric) Number of simulated samples for permutation test
#'
#' @export

permutation_balance_test <- function(data, control, trt, covars,
                                     treatment_var = "treatment_group", sims=1000){
  # adapted from:
  # https://alexandercoppock.com/Green-Lab-SOP/Green_Lab_SOP.html
  # listwise deletion of missing values
  svy_sbs <-  data[ data[,treatment_var] %in% c(control, trt),
                    c(treatment_var, "BLOCK_ID", covars)] %>%
    na.omit()
  deleted <- nrow(data)-nrow(svy_sbs)
  message(deleted, ' observations (', round(100*deleted/nrow(data),2) ,'%) deleted due to missingness.')

  # Regress treatment on covariates
  reg_model <- paste0(treatment_var, ' == "', trt, '" ~ factor(BLOCK_ID) + ',
                      paste(covars, collapse = ' + '))
  fit <- lm(formula(reg_model), data=svy_sbs)

  # computing heteroskedasticity-robust Wald statistic
  Rbeta.hat <- coef(fit)[-1]
  RVR <- vcovHC(fit, type <- 'HC0')[-1,-1]
  W_obs <- as.numeric(Rbeta.hat %*% solve(RVR, Rbeta.hat))

  # Compare to permutation distribution of W
  W_sims <- numeric(sims)
  pb <- txtProgressBar(min = 0, max = sims, style=3)
  set.seed(2020)

  # fixed values for blocked randomization
  sampling_prob <- mean(svy_sbs[,treatment_var]==trt)

  for (i in 1:sims){
    svy_sbs$Z_sim <- block_ra( blocks=svy_sbs$BLOCK_ID,
                               prob=sampling_prob )
    m1 <- paste0('Z_sim ~ factor(BLOCK_ID) + ', paste(covars, collapse = ' + '))
    fit_sim <- lm(formula(m1), data=svy_sbs)

    Rbeta.hat <- coef(fit_sim)[-1]
    RVR <- vcovHC(fit_sim, type <- 'HC0')[-1,-1]
    W_sims[i] <- as.numeric(Rbeta.hat %*% solve(RVR, Rbeta.hat))

    setTxtProgressBar(pb, i)
  }

  # Obtain p-value
  p <- mean(W_sims >= W_obs)
  message('(p=', round(p, 3), ')')

}

#' Create balance tables
#'
#' This function creates a balance table in which covariate proportions are listed in
#' rows and columns correspond to the control and treatment arms.
#' An additional column contains the treatment-control difference and a p-value
#' for a t-test of the difference in means.
#'
#' @param data (data frame) Data frame that contains survey data
#' @param control (character) Name of control group
#' @param trt (character) Name of treatment group
#' @param covars (character vector) Names of covariates in survey dataset that
#' will be used for the covariate balance test.
#' @param output_file (character) Name of file where table will be stored (in
#' latex format)
#'
#' @export

balance_table <- function(data, control, trt, covars, output_file){
  # keeping only relevant data; creating treatment indicator
  svy_sbs <-  data %>% filter(treatment_group %in% c(control, trt)) %>%
    mutate(in_trt = ifelse(treatment_group == trt, TRUE, FALSE))

  out <- list()
  i <- 1
  # loop over covariates
  for (var in covars){
    # find unique values of variable; excluding missing values
    values <- unique(svy_sbs[,var])
    values <- values[!is.na(values)]
    for (value in values){
      # t-test for difference in means
      tt <- t.test(formula(paste0(var, '=="', value, '" ~ in_trt')), data=svy_sbs)
      # storing output
      out[[i]] <- matrix(c(tt$estimate[1], tt$estimate[2],
                           (tt$estimate[1]-tt$estimate[2]), tt$p.value),
                         nrow=1, ncol=4, byrow=TRUE)
      rownames(out[[i]]) <- paste0('% ', var, '=', value)
      i <- i + 1
    }
  }
  # tidying up output table
  out <- do.call(rbind, out)
  colnames(out) <- c("% in Control", "% in Treatment", "% Diff.", "p(diff!=0)")
  print(xtable(out, digits=2), type='latex', file=output_file)
}


#' Implement sharpened FDR adjustment (Benjamini, Krieger, and Yekutieli 2006)
#' to adjust for multiple comparisons.
#'
#' This code generates BKY (2006) sharpened two-stage q-values as described in
#' Anderson (2008), "Multiple Inference and Gender Differences in the Effects
#' of Early Intervention: A Reevaluation of the Abecedarian, Perry Preschool,
#' and Early Training Projects", Journal of the American Statistical
#' Association, 103(484), 1481-1495

#' BKY (2006) sharpened two-stage q-values are introduced in Benjamini, Krieger,
#' and Yekutieli (2006), "Adaptive Linear Step-up Procedures that Control the
#' False Discovery Rate", Biometrika, 93(3), 491-507

# The code is based on the Stata .do file created by Anderson
#'
#'@param pval (numeric vector) Set of p values that need to be adjusted
#'
#' @export

sharpened_fdr <- function(pval){

  # Collect the total number of p-values tested
  totalpvals <- length(pval)

  # Sort the p-values in ascending order and generate a variable that codes each p-value's rank
  df <- data.frame(pval = pval, original_sorting_order = 1:totalpvals)
  df <- df[order(df$pval),]
  df$rank <- 1:totalpvals

  # Set the initial counter to 1
  qval = 1

  # Generate the variable that will contain the BKY (2006) sharpened q-values
  df$bky06_qval <- rep(1, totalpvals)

  # Set up a loop that begins by checking which hypotheses are rejected at
  # q = 1.000, then checks which hypotheses are rejected at q = 0.999, then
  # checks which hypotheses are rejected at q = 0.998, etc.
  # The loop ends by checking which hypotheses are rejected at q = 0.001.

  while (qval > 0) {
    # First Stage
    # Generate the adjusted first stage q level we are testing: q' = q/1+q
    qval_adj <- qval/(1+qval)
    # Generate value q'*r/M
    df$fdr_temp1 <- qval_adj*df$rank/totalpvals
    # Generate binary variable checking condition p(r) <= q'*r/M
    df$reject_temp1 <- (df$fdr_temp1>=df$pval)
    # Generate variable containing p-value ranks for all p-values that meet above condition
    df$reject_rank1 <- df$reject_temp1*df$rank
    # Record the rank of the largest p-value that meets above condition
    total_rejected1 <- max(df$reject_rank1)

    # Second Stage
    # Generate the second stage q level that accounts for hypotheses rejected in
    # first stage: q_2st = q'*(M/m0)
    qval_2st <- qval_adj*(totalpvals/(totalpvals-total_rejected1))
    # Generate value q_2st*r/M
    df$fdr_temp2 <- qval_2st*df$rank/totalpvals
    # Generate binary variable checking condition p(r) <= q_2st*r/M
    df$reject_temp2 <- (df$fdr_temp2>=df$pval)
    # Generate variable containing p-value ranks for all p-values that meet above condition
    df$reject_rank2 <- df$reject_temp2*df$rank
    # Record the rank of the largest p-value that meets above condition
    total_rejected2 <- max(df$reject_rank2)

    # A p-value has been rejected at level q if its rank is less than or equal
    # to the rank of the max p-value that meets the above condition
    df$bky06_qval <- ifelse(df$rank <= total_rejected2, qval, df$bky06_qval)
    # Reduce q by 0.001 and repeat loop
    # drop fdr_temp* reject_temp* reject_rank* total_rejected*
    qval <- qval - .001
  }

  df <- df[order(df$original_sorting_order),]

  # return adjusted p-values
  df$bky06_qval

}

#' Lasso selection of variables
#'
#' A simple function to select variables for baseline_model using Lasso
#'
#' @param d (data frame) Data frame that contains the data
#' @param dv (character) Name of the DV
#' @param lasso_covars (character vector) List of covariates to add to lasso
#' model for selection
#' @param treatment_var (character) Name of treatment group assignment variable.
#' Default "treatment_group"
#' @param trt (character) Name of treatment group
#' @param control (character) Name of control group
#' @param select_factor (logical) If \code{TRUE}, will include the whole factor from LASSO
#' in output, if \code{FALSE} it will output just the selected levels from the factor
#' @param seed (numeric) Seed for random numbers for repeatability.  Default 2020.
#'
#' @export

baseline_lasso <- function(d, dv, lasso_covars, treatment_var,
                           trt, control, weights_var = NULL, select_factor = FALSE, seed=2020) {

    # make all weights equal across obs fed to model
    if(is.null(weights_var)) {
      d$weights_var <- 1
      weights_var   <- "weights_var"
    }

    # select non-zero weight obs only
    d <- d %>% filter(!!as.name(weights_var) > 0)

    # selecting relevant data - listwise deletion
    d <- d %>% filter(!!as.name(treatment_var) %in% c(all_of(trt), all_of(control)))
    d_na <- na.omit(d[, c(dv, weights_var, lasso_covars)])
    d_model <- model.matrix( ~., d_na[, lasso_covars])

    # computing deleted observations
    deleted <- nrow(d)-nrow(d_na)
    message(sum(is.na(d[,dv])), ' missing values in DV.')
    message(deleted, ' observations (',
            round(100*deleted/nrow(d),2) ,'%) dropped by listwise deletion.')

    # setting folds manually so that they are reproducible
    nfolds <- 10; set.seed(seed)
    foldid <- sample(1:nfolds, size = nrow(d_model), replace = TRUE)
    # fitting lasso to select covariates
    lasso_select <- cv.glmnet(x=d_model, y=d_na[,dv],
                              alpha=1, weights=d_na[,weights_var],
                              nfolds=nfolds, foldid=foldid)

    # using some regex magic to extract all selected covariates
    coef_out <- coef(lasso_select, exact=TRUE)
    selected_coefs <- rownames(coef_out)[which(coef_out[,1] != 0)]
    selected_covars <- c()

    if(select_factor) {
      # get factor covar if some factor bin selected
      for (cov in lasso_covars){
        cov_suffix <- gsub("_bin_[A-Za-z0-9_-]*", "", cov)
        if ( any(grepl( paste0('^', cov_suffix), selected_coefs))) {
          selected_covars <- unique(c(selected_covars, cov_suffix))
        }
      }
    } else {
      # get only selected bin factors
      selected_covars <- selected_coefs[selected_coefs != '(Intercept)']
    }

    message('Covariates selected by lasso model: ', paste(selected_covars, collapse=", "))
    return(selected_covars)
}

#' Estimate treatment effects using a baseline model with lasso covariate
#' selection
#'
#' This function follows a two-step procedure. First, it estimates a lasso model
#' where the DV is regressed on its pre-treatment measure (if available), as well
#' as a set of covariates measured prior to treatment. Then, it fits a linear
#' model that includes the treatment indicator, the covariates selected by the
#' lasso model and additional covariates. The function returns both
#' weighted (PATE) and unweighted (SATE) treatment effects.
#'
#' @param d (data frame) Data frame that contains the data
#' @param dv (character) Name of the DV
#' @param dv_pre (character) Name of the pre-treatment DV. \code{NULL} if not
#' available. Default NULL.
#' @param lasso_covars (character vector) List of covariates to add to lasso
#' model for selection
#' @param model_covars (character vector) List of covariates to add to baseline
#' model, regardless of whether they were selected by the lasso or not.
#' @param selected_covars (character vectors) List of covariates selected to be
#' used in main specification (if not NULL, overrides LASSO selection of
#' covariates)
#' @param treatment_var (character) Name of treatment group assignment variable.
#' Default 'treatment_group'
#' @param trt (character) Name of treatment group
#' @param control (character) Name of control group
#' @param dv_label (character) Label for the DV, which can then be used to create
#' tables or graphs. Default is same as \code{dv}.
#' @param model_type (character) Model type(s) to return.  Must be one of 'SATE',
#' 'LATE', 'PLATE', 'PATE', or 'all' (default). You can also select multiple
#' models by adding the requested types as a character vector.
#' @param weights_var (character) Name of the weights variable in the dataset.
#' Must be included if \code{model_type} is 'all', 'PATE', or 'PLATE'.
#' @param iv (character vector) Vector of variables names in the dataset of the endogenous
#' variables defined in "instruments". Must be included if \code{model_type} is 'all' or 'LATE'.
#' @param instruments (character vector) Vector of variable names that constitute the 
#' instrumental variables used in the IV regression.
#' @param select_factor (logical) If \code{TRUE}, will include the whole factor from LASSO
#' in output, if \code{FALSE} it will output just the selected levels from the factor
#' @param convert_to_df (logical) If \code{TRUE}, will check if d is a data frame
#' and convert d to data frame if check returns FALSE
#' @param treated_bin (logical) If \code{TRUE}, will create a binary for the
#' whether obs belongs to treated group or not
#' @param factor_to_bin (logical) If \code{TRUE}, will create binary covariates
#' from factor (or character) covariates (excluding one category per factor var)
#' @param impute (boolean) Whether to use mean imputation for missing values
#' @param deact_impute (boolean) Whether to use mean imputation protocol outlined in
#' deactivation PAP
#' @param proportion (logical) If \code{TRUE}, will multiply the DV by 100 prior
#' to fitting the model. Useful if the DV is a percentage; so that the output
#' is easier to read.
#' @param digits (numeric) Number of decimal digits to display in output.
#' @param se_type (character) Type of standard errors for treatment effect
#' estimates. This is inherited from estimatr. Valid types are: "HC0", "HC1",
#' "HC2", and "HC3". Default is "HC2"
#' @param verbose (boolean) Whether to print messages with estimated effects.
#'
#' @export

baseline_model <- function(d, dv, dv_pre = NULL,
                           lasso_covars, model_covars,
                           selected_covars = NULL,
                           treatment_var = 'treatment_group',
                           trt, control, dv_label = NULL,
                           model_type = "all", weights_var = NULL, iv = NULL,
                           instruments = c("treated"),
                           select_factor = TRUE, convert_to_df = TRUE, treated_bin = TRUE, factor_to_bin = FALSE,
                           impute = TRUE, deact_impute = FALSE, proportion = FALSE,
                           digits = 3, se_type = 'HC2', combine_attrition = FALSE,
                           verbose = FALSE) {

  message('Running `alethea_deact` flavor of `baseline_model`')

  # send message if factor_to_bin is FALSE but select_factor is FALSE
  if(!factor_to_bin & !select_factor) {
    message('factor_to_bin is FALSE but LASSO will only select factor bins (select_factor is FALSE)')
  }

  # adding dv_pre to vector with all lasso covars
  lasso_covars <- unique(c(lasso_covars, dv_pre))

  # convert d to data frame if d is not data frame
  if (convert_to_df) d <- convert_to_df(d)

  # creating treated variable
  if (treated_bin)   d <- make_treated_bin(d = d, treatment_var = treatment_var, trt = trt)

  # creating binary vars for factor vars
  if (factor_to_bin){
      factor_bins     <- make_factor_bins(d, covars = lasso_covars)
      d               <- factor_bins$d
      lasso_covars    <- factor_bins$updated_covars
  }

  # imputation of pre-treament covariates
  if (impute) {
      imputed_covars  <- impute_covars(d, covars = lasso_covars, deact_impute = deact_impute)
      d               <- imputed_covars$d
      lasso_covars    <- imputed_covars$updated_covars
  }

  # make proportion var (dv * 100)
  if (proportion)   d <- proportion_dvs(d, dvs = c(dv))

  # select main specification covariates using LASSO
  if(is.null(selected_covars)) {
      selected_covars <- baseline_lasso(d = d, dv = dv,
                                        lasso_covars = lasso_covars,
                                        treatment_var = treatment_var,
                                        trt = trt, control = control,
                                        select_factor = select_factor)
  }

  if(combine_attrition) {
    d <- combine.attrition(d = d, dv = dv, covars = lasso_covars,
      treatment_var = treatment_var, trt = trt, control = control,
      impute = impute, deact_impute = deact_impute, treated_bin = treated_bin,
      standardize = standardize, proportion = proportion
    )
  }

  ATEs <- NULL
  ATEs <- compute_ATEs(model_type = model_type, d = d, dv = dv,
                       model_covars = model_covars, selected_covars = selected_covars,
                       dv_label = dv_label, weights_var = weights_var, iv = iv, instruments = instruments,
                       digits = digits, se_type = se_type, verbose = verbose)

  return(ATEs)

}


#' Wraps calls to estimate different treatment effects flavors using
#' main specification function.
#'
#' This function is a wrapper to the different ATE flavors:
#' (PATE, LATE, PLATE, SATE)
#'
#' @param d (data frame) Data frame that contains the data
#' @param dv (character) Name of the DV
#' @param dv_pre (character) Name of the pre-treatment DV. \code{NULL} if not
#' available. Default NULL.
#' @param model_covars (character vector) List of covariates to add to baseline
#' model, regardless of whether they were selected by the lasso or not.
#' @param selected_covars (character vector) List of covariates to add to lasso
#' model for selection
#' @param dv_label (character) Label for the DV, which can then be used to create
#' tables or graphs. Default is same as \code{dv}.
#' @param model_type (character) Model type(s) to return.  Must be one of 'SATE',
#' 'LATE', 'PLATE', 'PATE', or 'all' (default). You can also select multiple
#' models by adding the requested types as a character vector.
#' @param weights_var (character) Name of the weights variable in the dataset.
#' Must be included if \code{type} is 'PATE', or 'PLATE'.
#' @param iv (character vector) Vector of variables names in the dataset of the endogenous
#' variables defined in "instruments". Must be included if \code{model_type} is 'all' or 'LATE'.
#' @param instruments (character vector) Vector of variable names that constitute the 
#' instrumental variables used in the IV regression.
#' @param digits (numeric) Number of decimal digits to display in output.
#' @param se_type (character) Type of standard errors for treatment effect
#' estimates. This is inherited from estimatr. Valid types are: "HC0", "HC1",
#' "HC2", and "HC3". Default is "HC2"
#' @param verbose (boolean) Whether to print messages with estimated effects.
#'
#' @export

compute_ATEs <- function(model_type='all', d, dv, model_covars, selected_covars,
                         dv_label=NULL, weights_var=NULL, iv=NULL, instruments = c("treated"),
                         digits=3, se_type = 'HC2', verbose=FALSE, return_model=FALSE) {

  # baseline model - PATE
  pate <- NULL
  if (any(model_type %in% c('all','PATE'))){
    pate  <- main_specification(type = 'PATE', d = d, dv = dv,
                                 model_covars = model_covars, selected_covars = selected_covars,
                                 dv_label = dv_label, weights_var = weights_var, iv = NULL, instruments = instruments,
                                 digits = digits, se_type = se_type, verbose = verbose, return_model = return_model)
  }

  # baseline model - LATE
  late <- NULL
  if (any(model_type %in% c('all','LATE'))){
    late  <- main_specification(type = 'LATE', d = d, dv = dv,
                                 model_covars = model_covars, selected_covars = selected_covars,
                                 dv_label = dv_label, weights_var = NULL, iv = iv, instruments = instruments,
                                 digits = digits, se_type = se_type, return_model = return_model)
  }

  # baseline model - PLATE
  plate <- NULL
  if (any(model_type %in% c('all','PLATE'))){
    plate <- main_specification(type = 'PLATE', d = d, dv = dv,
                                 model_covars = model_covars, selected_covars = selected_covars,
                                 dv_label = dv_label, weights_var = weights_var, iv = iv, instruments = instruments,
                                 digits = digits, se_type = se_type, verbose = verbose, return_model = return_model)
  }

  # baseline model - SATE
  sate <- NULL
  if (any(model_type %in% c('all','SATE'))){
    sate  <- main_specification(type = 'SATE', d = d, dv = dv,
                                 model_covars = model_covars, selected_covars = selected_covars,
                                 dv_label = dv_label, weights_var = NULL, iv = NULL, instruments = instruments,
                                 digits = digits, se_type = se_type, return_model = return_model)
  }

  if (return_model & model_type != 'all') {
    ates_df_model = (Filter(Negate(is.null), list(pate, late, plate, sate)))[[1]]
    return(ates_df_model)
  } else { 
    return(rbind(pate, plate, late, sate))
  }

}


#' Estimate treatment effects using main specification, given a subset of
#' covariates and the type of treatment effects desired.
#'
#' This function performs the main linear model specification,
#' accepting different types of ATE: PATE, LATE, PLATE, SATE.
#' The linear model includes the treatment indicator, the covariates selected
#' for the main specification (should be done by LASSO, see baseline_lasso)
#' and additional covariates.
#'
#' @param d (data frame) Data frame that contains the data
#' @param dv (character) Name of the DV
#' @param dv_pre (character) Name of the pre-treatment DV. \code{NULL} if not
#' available. Default NULL.
#' @param model_covars (character vector) List of covariates to add to baseline
#' model, regardless of whether they were selected by the lasso or not.
#' @param selected_covars (character vector) List of covariates to add to lasso
#' model for selection
#' @param dv_label (character) Label for the DV, which can then be used to create
#' tables or graphs. Default is same as \code{dv}.
#' @param type (character) Model type to return.  Must be one of 'SATE',
#' 'LATE', 'PLATE', or 'PATE'. The function returns outputs for a single type in
#' each call
#' @param weights_var (character) Name of the weights variable in the dataset.
#' Must be included if \code{type} is 'PATE', or 'PLATE'.
#' @param iv (character vector) Vector of variables names in the dataset of the endogenous
#' variables defined in "instruments". Must be included if \code{model_type} is 'all' or 'LATE'.
#' @param instruments (character vector) Vector of variable names that constitute the 
#' instrumental variables used in the IV regression.
#' @param digits (numeric) Number of decimal digits to display in output.
#' @param se_type (character) Type of standard errors for treatment effect
#' estimates. This is inherited from estimatr. Valid types are: "HC0", "HC1",
#' "HC2", and "HC3". Default is "HC2"
#' @param verbose (boolean) Whether to print messages with estimated effects.
#'
#' @export

main_specification <- function(type, d, dv, model_covars, selected_covars=NULL,
                               dv_label=NULL, weights_var=NULL, iv=NULL, instruments,
                               digits=3, se_type = 'HC2', return_model=FALSE, verbose=FALSE) {

  # sort selected covars
  if(!is.null(selected_covars)) {
    selected_covars <- sort(selected_covars)
  }

  # run data checks
  if(!(type %in% c('SATE','LATE','PATE','PLATE'))) {
    stop("type must be at one 'SATE', 'LATE', 'PATE' or 'PLATE'")
  }
  if(type %in% c('PATE', 'PLATE') & is.null(weights_var)) {
    stop('weights_var not provided but PATE or PLATE requested')
  }
  if(type %in% c('LATE', 'PLATE')) {
      if(is.null(iv)) {
        stop('iv not provided but LATE or PLATE requested')
      }
  }
  if(!('treated' %in% colnames(d))) {
    stop('please provide treatment binary variable `treated` with dataframe `d`')
  }

  # create label if needed
  if(is.null(dv_label)) {
    dv_label = dv
  }

  # drop values with dv = NA
  d.unfiltered <- d
  d_na_count <- sum(is.na(d[,dv]))
  d <- d %>% filter(!is.na(!!as.name(dv)))

  # computing deleted observations
  message(sprintf('%s missing values in DV / dropped observations.', d_na_count))

  # run missing values check
  missing_values <- sum(is.na(d[, c(dv, model_covars, selected_covars, iv, instruments)]) > 0)
  if(missing_values > 0) {
    stop('There are missing values on the dataset')
  }
    
  # run factor covariate check
  vars <- colnames(d[, c(dv, model_covars, selected_covars, iv, instruments)])
  factor_vars <- vars[class(d[, c(dv, model_covars, selected_covars, iv, instruments)]) %in% c('character', 'factor')]
  if(length(factor_vars) > 0 ) {
    stop(sprintf('Factor var(s) on the relevant data: %s', paste(factor_vars, collapse = ", ")))
  }
        
  # create ATE var
  ate_var <- unlist(ifelse(type %in% c('LATE', 'PLATE'), list(iv), list(instruments)))

  # specify regression type and weights
  reg_type  <- ifelse(type %in% c('LATE', 'PLATE'), 'ivreg', 'lm')
  weights <- NULL
  if(!is.null(weights_var) & type %in% c('PLATE', 'PATE')) {
    d <- d %>% filter(!!as.name(weights_var) > 0) # filter out zero-weighted obs
    weights <- d[,weights_var]
  }

  # create main specification formula
  f <- build_formula(type = type, dv = dv, iv = iv, instruments = instruments,
                     selected_covars = selected_covars,
                     model_covars = model_covars)
    
  baseline_mod <- do.call(reg_type,
                            list(
                                formula=f,
                                data=d,
                                weights=weights,
                                na.action='na.exclude'
                                )
                          )

  # remove covariates excluded by lm/ivreg
  removed_covars <- names(baseline_mod$coefficients)[is.na(baseline_mod$coefficients)]
  removed_covars <- removed_covars[!str_detect(removed_covars, "D_i|treated")]

  if(length(removed_covars) > 0) {

    # remove redundant covariates from model
    selected_covars <- selected_covars[!(selected_covars %in% removed_covars)]
    model_covars    <- model_covars[!(model_covars %in% removed_covars)]

    message(sprintf('Covariate(s) automatically removed from model: %s',
            paste(removed_covars, collapse = ", ")))

    # re-write formula
    f <- build_formula(type = type, dv = dv, iv = iv, instruments = instruments,
                       selected_covars = selected_covars,
                       model_covars = model_covars)

    # re-run baseline model
    baseline_mod <- do.call(reg_type,
                            list(
                                formula=f,
                                data=d,
                                weights=weights,
                                na.action='na.exclude'
                                )
                          )
  } else {
    message('No covariate automatically removed from model')
  }

  # get VIF for model
  # require package for vif function
  if (!require(car)) {
    install.packages('car')
    library(car)
  }

  model_vif <- car::vif(baseline_mod)
  model_vif <- model_vif[!str_detect(names(model_vif), "block_id|D_i|treated")] # we don't want to remove BLOCK_ID factors per PAP

  while(length(model_vif) > 0 && max(model_vif) >= 500 && length(selected_covars) > 0) {

    # remove covariate with largest VIF
    model_vif <- sort(model_vif, decreasing = TRUE)

    collin_cov <- names(model_vif[1])
    selected_covars <- selected_covars[selected_covars != collin_cov]

    message(sprintf('Covariate removed due to large VIF: %s (VIF: %s)', collin_cov, round(model_vif[1], 2)))

    # re-write formula
    f <- build_formula(type = type, dv = dv, iv = iv, instruments = instruments,
                       selected_covars = selected_covars,
                       model_covars = model_covars)

    # re-run baseline model
    baseline_mod <- do.call(reg_type,
                            list(
                                formula=f,
                                data=d,
                                weights=weights,
                                na.action='na.exclude'
                                )
                          )

    # update VIF
    model_vif <- car::vif(baseline_mod)
    model_vif <- model_vif[!str_detect(names(model_vif), "block_id|D_i|treated")]

  }

  # computing QOIs with HC2 robust standard errors + weights
  b.vcov = vcovHC(baseline_mod, type=se_type)
  if(sum(is.na(b.vcov))==prod(dim(b.vcov))) {
      stop(sprintf("WARNING: cannot cannot calculate HC2 SEs for dv: %s", dv))
      message("WARNING: cannot calculate robust standard errors.  Defaulting to HC1")
      b.vcov = vcovHC(baseline_mod, type="HC1")
  }
  ct <- coeftest(baseline_mod, vcov=b.vcov)
  ate <- ct[ate_var, "Estimate"]
  std_error <- ct[ate_var, "Std. Error"]
  ci_low <- ate - 1.96 * std_error
  ci_hi <- ate + 1.96 * std_error
  pval <- ct[ate_var, "Pr(>|t|)"]
  tstat <- ct[ate_var, "t value"]

  if (verbose) {
      message(sprintf(" === %s === ", type))
      message("Estimate: ", display(ate, digits=digits))
      message("Std. Error: ", display(std_error, digits=digits))
      message("p value ", ifelse(pval < 0.01, '< 0.01', paste0('= ', display(pval, digits=digits))))
      message("95% CI Lower: ", display(ci_low, digits=digits))
      message("95% CI Upper: ", display(ci_hi, digits=digits))
      message("Adj. R2 = ", display(summary(baseline_mod)$adj.r.squared), digits=digits)
  }

  res_df <- data.frame(
    dv_label, 
    est = type, 
    ate,
    ci = paste0("[", display(ci_low, digits=digits), ", ", display(ci_hi, digits=digits), "]"),
    ci_low, ci_hi, std_error, tstat, pval
  )
        
  if (return_model) {
    return(list(mod = baseline_mod, df = res_df))
  } else {
    return(res_df)
  }

}


#' Build main specification formula
#'
#' This function
#'
#' @param dv_label (character) Label for the DV, which can then be used to create
#' tables or graphs. Default is same as \code{dv}.
#' @param type (character) Model type to return.  Must be one of 'SATE',
#' 'LATE', 'PLATE', or 'PATE'. The function returns outputs for a single type in
#' each call
#' @param dv (character) Name of the DV
#' @param iv (character vector) Vector of variables names in the dataset of the endogenous
#' variables defined in "instruments". Must be included if \code{model_type} is 'all' or 'LATE'.
#' @param instruments (character vector) Vector of variable names that constitute the 
#' instrumental variables used in the IV regression.
#' @param selected_covars (character vector) List of covariates to add to lasso
#' model for selection
#' @param model_covars (character vector) List of covariates to add to baseline
#' model, regardless of whether they were selected by the lasso or not.
#'
#' @export

build_formula <- function(type, dv, iv = NULL, instruments, selected_covars, model_covars=NULL, intercept = TRUE) {

  to_formula <- function(x) ifelse(length(x)>0, paste(" + ", paste(x, collapse=" + ")), '')

  f.use.selected = to_formula(selected_covars)
  f.model.covars = to_formula(model_covars)
  f.model.iv = gsub(" \\+  ", "", to_formula(iv))
  f.model.instruments = gsub(" \\+  ", "", to_formula(instruments))

  if(type %in% c('LATE', 'PLATE')) {
    f = paste0(dv, " ~ ", f.model.iv, f.use.selected, f.model.covars,
                  "|", f.model.instruments ,f.use.selected, f.model.covars)
  } else {
    f = paste0(dv, " ~ treated ", f.use.selected, f.model.covars)
  }

  return(f)

}


#' Convert data d into dataframe if needed
#'
#' The function checks if d is data frame, if check returns FALSE,
#' function converts d intro a data frame before returning d
#'
#' @param d (data frame) Data frame that contains survey data
#'
#' @export

convert_to_df <- function(d) {
  if(!is.data.frame(d)) {
    message("Converting input to data frame")
    d <- as.data.frame(d)
  }
  return(d)
}

#' Make binary of whether observation is included in treated group
#'
#' The function creates a binary variable, based on whether the
#' treatment group assignment of the observation corresponds to the treated
#' group (trt).
#'
#' @param d (data frame) Data frame that contains survey data
#' @param treatment_var (character) Name of treatment group assignment variable.
#' Default 'treatment_group'
#' @param trt (character) Name of treatment group#'
#'
#' @export

make_treated_bin <- function(d, treatment_var='treatment_group', trt) {
  d$treated <- 1 * (d[,treatment_var] == trt)
  return(d)
}

#' Makes binary variables out of factors of factor variable(s).
#'
#' The function turns factor covariates into binaries, excluding
#' one of the binaries. The name of the new variables from a factor
#' `factor` with a value `Value One` is `factor_bin_value_one`. The
#' function ouputs a dataframe with all binaries from factor variables
#' found on the input list of variables ($d). It also returns an updates
#' list of covariates that replaces the factor name with the names of
#' the new factor binaries ($updated_covars)
#'
#' @param d (data frame) Data frame that contains survey data
#' @param covars (character) Name(s) of covariates (potential factors)
#'
#' @export

make_factor_bins <- function(d, covars, remove_most_frequent_dummy = TRUE) {
  # create vec of covars with bins
  updated_covars <- covars

  # require package for dummy_cols
  if (!require(fastDummies)) {
    install.packages('fastDummies')
    library(fastDummies)
  }

  for(cov in covars) {
    vartype <- class(d[[cov]])
    if (vartype %in% c('character', 'factor'))  {
        # create binary vars out of factors
        cov_suffix <- sprintf("%s_bin", cov)
        d[cov_suffix] <- d[cov]
        d <- dummy_cols(d, remove_most_frequent_dummy = remove_most_frequent_dummy, ignore_na = TRUE, select_columns = all_of(cov_suffix))
        d <- d %>% select(-c(all_of(cov_suffix)))

        # clean new bin variable names
        bins <- which(grepl(cov_suffix, names(d), fixed = TRUE))
        colnames(d)[bins] <- gsub(" ", "_", colnames(d)[bins])
        colnames(d)[bins] <- gsub("-", "_", colnames(d)[bins])
        colnames(d)[bins] <- gsub(",", "",  colnames(d)[bins])
        colnames(d)[bins] <- tolower(colnames(d)[bins])

        # remove old cov from list and add bins
        updated_covars <- updated_covars[updated_covars!=cov]
        updated_covars <- c(updated_covars, colnames(d)[bins])
    }

  }

  return(list(d = d, updated_covars = updated_covars))
}

#' Imputation procedure as described on PAP
#'
#' The function imputes numerical covariates, including bins of
#' factors, as the mean of non-missing values. For variables with
#' >10% missing values, also create and include in LASSO covars a
#' missingness binary (updates list of covariates to include
#' missingness binary if needed).
#'
#' @param d (data frame) Data frame that contains survey data
#' @param covars (character) Name(s) of covariates to be imputed
#' @param deact_impute (boolean) Whether to use mean imputation protocol outlined in
#' deactivation PAP
#'
#' @export

impute_covars <- function(d, covars, weights_var = NULL, sample = NULL, deact_impute = TRUE) {

  updated_covars <- covars

  if(is.null(weights_var)) {
    d$weights_var <- 1
    weights_var = "weights_var"
  }

  if(is.null(sample)) {
    d$sample <- TRUE
    sample = "sample"
  }

  for(i in 1:length(covars)){
    pct_missing <- mean(is.na(d[,covars[i]]))
    if(pct_missing <= .10) {
        d[,covars[i]] <- mean_impute(x = d[,covars[i]], w = d[,weights_var], in_sample = d[, sample], deact_impute = deact_impute)
    } else {
        message(sprintf('create missing cov for: %s', covars[i]))
        # assign arbitrary constant
        missing <- is.na(d[,covars[i]])
        d[,covars[i]] <- mean_impute(x = d[,covars[i]], w = d[,weights_var], in_sample = d[, sample], deact_impute = deact_impute) # arbitrary constant = mean of var

        # create missingness variable
        suffix          <- gsub("_bin_[A-Za-z0-9_-]*", "", covars[i])
        missing_col     <- sprintf("%s_missing", suffix)
        d[,missing_col] <- ifelse(missing, 1, 0) # creates a single missing binary for factor var
        updated_covars  <- unique(c(updated_covars, missing_col))
    }
  }

  impute_test <- sum(sapply(d %>% select(all_of(covars)), # checking imputation worked
                            function(x) sum(is.na(x))))
  if (impute_test > 0) stop("Some covariates still have missing values.")
  if (impute_test == 0) message("Successfully imputed covariates with missing values.")

  return(list(d = d, updated_covars = updated_covars))
}


#' Multiply variables by 100
#'
#' The function multiplies the variables with var_names on
#' dvs vector by 100 and returns a dataframe with the transformed
#' variables
#'
#' @param d (data frame) Data frame that contains survey data
#' @param dvs (character) Name(s) of covariates to be transformed
#'
#' @export

proportion_dvs <- function(d, dvs) {
  for (dv in dvs) {
    d[,dv] <- d[,dv] * 100
  }
  return(d)
}


#' Estimate treatment effects using a Lin (2013) regression model with lasso
#' covariate selection
#'
#' This function follows a two-step procedure. First, it estimates a lasso model
#' where the DV is regressed on its pre-treatment measure (if available), as well
#' as a set of covariates measured prior to treatment. Then, it fits a Lin (2013)
#' model that includes the treatment indicator, the covariates selected by the
#' lasso model and additional covariates. The function returns both
#' weighted (PATE) and unweighted (SATE) treatment effects.
#'
#' @param d (data frame) Data frame that contains the data
#' @param dv (character) Name of the DV
#' @param dv_pre (character) Name of the pre-treatment DV. \code{NULL} if not
#' available
#' @param lasso_covars (character vector) List of covariates to add to lasso
#' model for selection
#' @param model_covars (character vector) List of covariates to add to baseline
#' model, regardless of whether they were selected by the lasso or not.
#' @param trt (character) Name of treatment group
#' @param control (character) Name of control group
#' @param dv_label (character) Label for the DV, which can then be used to create
#' tables or graphs
#' @param weights_var (character) Name of the weights variable in the dataset
#' @param standardize (logical) If \code{TRUE}, the DV will be standardized
#' prior to fit the models, and the returning treatment effect estimate will be
#' a standardized coefficient.
#' @param proportion (logical) If \code{TRUE}, will multiply the DV by 100 prior
#' to fitting the model. Useful if the DV is a percentage; so that the output
#' is easier to read.
#' @param digits (numeric) Number of decimal digits to display in output.
#' @param se_type (character) Type of standard errors for treatment effect
#' estimates. This is inherited from estimatr. Valid types are: "HC0", "HC1",
#' "HC2", and "HC3". Default is "HC2"
#' @param impute (boolean) Whether to use mean imputation for missing values
#' of pretreatment covariates. Default is set to TRUE.
#'
#'
#' @export

lin_model <- function(d, dv, dv_pre=NULL, lasso_covars, model_covars, trt,
                      control, dv_label, weights_var,
                      standardize=TRUE, proportion=FALSE,
                      digits=3, se_type="HC2", impute=TRUE){

  # creating treated variable
  d$treated <- 1 * (d$treatment_group==trt)

  # imputation of pre-treament covariates
  if (impute){
    to_impute <- c(lasso_covars, dv_pre)
    for(i in 1:length(to_impute)){
      d[,to_impute[i]] <- mean_impute(d[,to_impute[i]])
    }
    impute_test <- sum(sapply(d %>% select(all_of(to_impute)), # checking imputation worked
                              function(x) sum(is.na(x))))
    if (impute_test > 0) stop("Some covariates still have missing values.")
    if (impute_test == 0) message("Successfully imputed covariates with missing values.")
  }

  # standardizing DV
  if (standardize) d[,dv] <- ( d[,dv] - mean(d[d$treatment_group==control,dv], na.rm=TRUE) )/
      sd(d[d$treatment_group==control,dv], na.rm=TRUE)
  if (proportion) d[,dv] <- 100 * d[,dv]

  # selecting relevant data - listwise deletion
  d <- d[d$treatment_group %in% c(trt, control),]
  d_na <- na.omit(d[, c(dv, dv_pre, lasso_covars)])
  d_model <- model.matrix( ~., d_na[, c(dv_pre, lasso_covars)])
  # computing deleted observations
  deleted <- nrow(d)-nrow(d_na)
  message(sum(is.na(d[,dv])), ' missing values in DV.')
  message(deleted, ' observations (', round(100*deleted/nrow(d),2) ,'%) dropped by listwise deletion.')

  # setting folds manually so that they are reproducible
  nfolds <- 10; set.seed(2020)
  foldid <- sample(1:nfolds, size = nrow(d_model), replace = TRUE)
  # fitting lasso to select covariates
  lasso_select <- cv.glmnet(x=d_model, y=d_na[,dv],
                            alpha=1, nfolds=nfolds, foldid=foldid)

  # using some regex magic to extract all selected covariates
  coef_out <- coef(lasso_select, exact=TRUE)
  selected_coefs <- rownames(coef_out)[which(coef_out[,1] != 0)]
  selected_covars <- c()
  for (cov in c(dv_pre, lasso_covars)){
    if ( any(grepl( paste0('^', cov), selected_coefs))) {
      selected_covars <- c(selected_covars, cov)
    }
  }
  message('Covariates selected by lasso model: ', paste(selected_covars, collapse=", "))

  # baseline model - PATE
  if (length(selected_covars)>0){
    lin_reg <- do.call("lm_lin",
                       list(formula=formula(paste0(dv, " ~ treated")),
                            covariates=formula(paste0(" ~ ", paste0(selected_covars, collapse=" + "),
                                                      " + ", paste0(model_covars, collapse=" + "))),
                            data=d,
                            weights=d[,weights_var],
                            se_type=se_type
                       ))
  }
  if (length(selected_covars)==0){
    lin_reg <- do.call("lm_lin",
                       list(formula=formula(paste0(dv, " ~ treated")),
                            covariates=formula(paste0(" ~ ", paste0(model_covars, collapse=" + "))),
                            data=d,
                            weights=d[,weights_var],
                            se_type=se_type
                       ))
  }

  # computing QOIs with HC2 robust standard errors + weights
  ct <- coeftest(lin_reg)
  ate <- ct["treated", "Estimate"]
  std_error <- ct["treated", "Std. Error"]
  ci_low <- ate - 1.96 * std_error
  ci_hi <- ate + 1.96 * std_error
  pval <- ct["treated", "Pr(>|t|)"]

  message(" === PATE === ")
  message("Estimate: ", display(ate, digits=digits))
  message("Std. Error: ", display(std_error, digits=digits))
  message("p value ", ifelse(pval < 0.01, '< 0.01', paste0('= ', display(pval, digits=digits))))
  message("95% CI Lower: ", display(ci_low, digits=digits))
  message("95% CI Upper: ", display(ci_hi, digits=digits))
  message("Adj. R2 = ", display(summary(lin_reg)$adj.r.squared), digits=digits)

  pate <- data.frame(dv_label, est='PATE', ate,
                     ci = paste0("[", display(ci_low, digits=digits), ", ",
                                  display(ci_hi, digits=digits), "]"),
                     std_error, pval)

  # baseline model - SATE
  if (length(selected_covars)>0){
    lin_reg <- lm_lin(formula=formula(paste0(dv, " ~ treated")),
                      covariates=formula(paste0(" ~ ", paste0(selected_covars, collapse=" + "),
                                                " + ", paste0(model_covars, collapse=" + "))),
                      data=d,
                      se_type=se_type)
  }
  if (length(selected_covars)==0){
    lin_reg <- lm_lin(formula=formula(paste0(dv, " ~ treated")),
                      covariates=formula(paste0(" ~ ", paste0(model_covars, collapse=" + "))),
                      data=d,
                      se_type=se_type)
  }

  # computing QOIs with HC2 robust standard errors
  ct <- coeftest(lin_reg)
  ate <- ct["treated", "Estimate"]
  std_error <- ct["treated", "Std. Error"]
  ci_low <- ate - 1.96 * std_error
  ci_hi <- ate + 1.96 * std_error
  pval <- ct["treated", "Pr(>|t|)"]

  message(" === SATE === ")
  message("Estimate: ", display(ate, digits=digits))
  message("Std. Error: ", display(std_error, digits=digits))
  message("p value ", ifelse(pval < 0.01, '< 0.01', paste0('= ', display(pval, digits=digits))))
  message("95% CI Lower: ", display(ci_low, digits=digits))
  message("95% CI Upper: ", display(ci_hi, digits=digits))
  message("Adj. R2 = ", display(summary(lin_reg)$adj.r.squared), digits=digits)

  sate <- data.frame(dv_label, est='SATE', ate,
                     ci = paste0("[", display(ci_low, digits=digits), ", ",
                                  display(ci_hi, digits=digits), "]"),
                     std_error, pval)

  return(rbind(pate, sate))

}

#' Winsorize raw count at a given percentile threshold, using 
#' only main sample observations for percentile value calculation
#' if main sample binary vector is specified
#' @param x (numeric) Number to winsorize
#' @param pct (numeric) Percentile at which to winsorize
#' @param main_sample (binary) Vector of same size of x, such 
#' that it is TRUE when obs from x is in main analysis sample and
#' FALSE otherwise
#' @export

winsorize <- function(x, pct=.99, main_sample = NULL) {
  if(!is.null(main_sample)) {
    include = main_sample    
  } else {
    include = rep(TRUE, length(x))
  }
  thr <- quantile(x[include], probs=pct, na.rm=TRUE)
  x <- ifelse(x > thr, thr, x)
  return(x)
}

#' Single imputation of missing values
#'
#' Function to impute missing values in a covariate to the mode (categorical) or
#' mean (continuous).
#'
#' @param x (numeric or character) Vector that contains data with missing values
#' that need to be imputed
#' @param seed (integer) Random number generator state
#' @param deact_impute (boolean) Whether to use mean imputation protocol outlined in
#' deactivation PAP
#' @export

mean_impute <- function(x, w = NULL, in_sample = NULL, seed=2020, deact_impute = TRUE){

  # make all weights equal across obs fed to model
  if(is.null(w)) {
    w <- rep(1, length(x))
  }

  if(is.null(sample)) {
    in_sample <- rep(TRUE, length(x))
  }

  # test for zero weights
  if (sum(w[in_sample] == 0) > 0) {
    message('ATTENTION: Zero weights included in weight vector of mean_impute')
  }

  set.seed(seed)
  # assess missingness
  nas <- sum(is.na(x))

  if (nas > 0){ # Impute when there is missing values
    vartype <- class(x)
    if (vartype %in% c('character', 'factor')){ # Assign levels proportionally
      pvec <- round(prop.table(table(x)), 2)
      levs <- names(pvec)
      impvals <- sample(size = nas, levs, prob = pvec, replace = TRUE)
      #imputing values
      x[is.na(x)] <- impvals
    }
    # original alethea imputation of numerical
    if (!deact_impute) {
      if (vartype == 'numeric'){ # Imputing sample mean
        mu <- round(mean(x, na.rm=TRUE), 3)
        x[is.na(x)] <- mu
      }
      if (vartype == 'integer'){ # Imputing sample mean
        mu <- round(mean(x, na.rm=TRUE), 0)
        x[is.na(x)] <- mu
      }
    # deactivation imputation of numerical
    } else if (vartype %in% c('numeric', 'integer', 'logical')){ # Imputing sample mean
      x <- as.numeric(x)
      mu <- weighted.mean(x[w>0 & in_sample], w[w>0 & in_sample], na.rm=TRUE)
      x[is.na(x)] <- mu
    }
  }

  return(x)
}


#' Latex saving functions
#' Code adapted from Digital Addiction (AGS 2022)
#'
#' Function to save scalars
#'
#' @param scalar_list (numeric or character) named list of scalars 
#' @param scalar_names (character) list of alternative names for scalars
#' @param digits (integer) total decimals output should include
#' @param filename (integer) name of file, including path
#' @export

save_tex <- function(scalar_list, scalar_names = NULL, digits = 2, filename){
    tex_string <- ""
    for(x in names(scalar_list)){
      name <- x
      if (x %in% names(scalar_names)){
        name <- scalar_names[[x]]
      }

      if(is.character(scalar_list[[x]])) {
        digit_spec <- "%s"
      } else {
        digit_spec <- paste0("%.", digits, "f")
      } 

      scalar_formatted <- sprintf(digit_spec, scalar_list[[x]])
      
      new_line   <- sprintf("\\newcommand{\\%s}{%s}", name, scalar_formatted)
      tex_string <- sprintf("%s\n%s", tex_string, new_line)
    }
    
    # saves it as a .tex file 
    sink(file=filename)
    cat(tex_string)
    sink(NULL)
}
