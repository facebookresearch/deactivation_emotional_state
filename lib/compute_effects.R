# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

###################################
###   lib - compute_effects.R   ###
###################################

devtools::load_all("~/alethea")

library(stringr)

select_covars_lasso <- function(
  df,
  weights_var,
  dv,
  dv_pre_list,
  lasso_covars = NULL,
  verbose = FALSE
) {
  #' Runs lasso regression to select covariates for a given outcome variable.
  #'
  #' @param df A data frame containing the main sample data.
  #' @param weights_var A string indicating the variable to use for weights.
  #' @param dv A string indicating the outcome variable to estimate.
  #' @param dv_pre_list A list of strings indicating the corresponding baseline outcome variables.
  #' @param lasso_covars A vector of covariates to include in the lasso regression. Default is NULL.
  #' @param verbose A boolean indicating whether to print progress messages. Default is FALSE.
  #'
  #' @return A list of selected covariates for the given outcome variable.
  #'
  if (dv_pre_list[[dv]] == "") {
    dv_pre <- NULL
  } else {
    dv_pre <- dv_pre_list[[dv]]
  }
  lasso_covars <- c(lasso_covars, dv_pre)
  selected_covars <- alethea::baseline_lasso(
    d = df,
    dv = dv,
    lasso_covars = unique(lasso_covars),
    treatment_var = "experiment_group",
    trt = "test",
    control = "control",
    weights_var = weights_var,
    select_factor = FALSE
  )
  covars_dict <- list()
  covars_dict[[dv]] <- selected_covars
  return(covars_dict)
}

run_outcome_wrapper <- function(
  dv_dict,
  sample,
  selected_covars,
  model_covars,
  iv,
  weights_var,
  model_type = "PLATE",
  verbose = FALSE,
  return_model = FALSE
) {
  #' Estimates treatment effects for a given outcome, wrapping `run_outcome`.
  #' Allows for packaging dv and dv_label into a dictionary, for lapply calls.
  #'
  #' @param sample A data frame containing the main sample data.
  #' @param selected_covars A list of lasso-selected covariates for each outcome.
  #' @param model_covars A vector of model block-id covariates.
  #' @param iv A string indicating the instrumental variable to use.
  #' @param weights_var A string indicating the variable to use for weights.
  #' @param model_type A string indicating the estimate to recover (e.g., 'PLATE'). Default is 'PLATE'.
  #' @param verbose A boolean indicating whether to print progress messages. Default is FALSE.
  #' @param return_model A boolean indicating whether to return the model object. Default is FALSE.
  #'
  #' @return A dataframe containing the estimated treatment effect for desired outcome.
  #'
  dv <- dv_dict[[1]]
  dv_label <- dv_dict[[2]]
  if (is.list(selected_covars)) {
    selected_covars <- selected_covars[[dv]]
  }
  res <- run_outcome(
    df = sample,
    model_covars = model_covars,
    selected_covars = selected_covars,
    dv = dv,
    dv_label = dv_label,
    model_type = model_type,
    weights_var = weights_var,
    iv = iv,
    verbose = verbose,
    return_model = return_model
  )
  return(res)
}

run_outcome <- function(
  df,
  model_covars,
  selected_covars,
  iv,
  instruments = c("treated"),
  weights_var,
  dv,
  dv_label,
  model_type = "PLATE",
  se_type = "HC2",
  verbose = FALSE,
  return_model = FALSE
) {
  #' Estimates treatment effects for a given outcome, wrapping `alethea::compute_ATEs`.
  #' Removes singleton nodes (relative to model covars) from the data frame before estimation.
  #' Note: compute_ATEs handles missing data in DV and drops covariates with zero variance.
  #'
  #' @param df A data frame containing the main sample data.
  #' @param model_covars A vector of model block-id covariates.
  #' @param selected_covars A list of lasso-selected covariates for each outcome.
  #' @param iv A string indicating the instrumental variable to use.
  #' @param instruments A vector of strings indicating variable to instrument. Default is 'treated'.
  #' @param weights_var A string indicating the variable to use for weights.
  #' @param dv A string indicating the outcome variable to estimate.
  #' @param dv_label A string indicating the label for the outcome variable.
  #' @param model_type A string indicating the estimate to recover (e.g., 'PLATE'). Default is 'PLATE'.
  #' @param se_type A string indicating the type of standard errors to use. Default is 'HC2'.
  #' @param verbose A boolean indicating whether to print progress messages. Default is FALSE.
  #' @param return_model A boolean indicating whether to return the model object. Default is FALSE.
  #'
  #' @return A dataframe containing the estimated treatment effect for desired outcome.
  #'  If return_model is TRUE, returns a list containing the model object and the dataframe.
  #'
  # ensure df valid for specification (i.e., remove any singletons)
  df <- validate_df(df, model_covars)
  res <- alethea::compute_ATEs(
    d = df,
    dv = dv,
    dv_label = dv_label,
    model_covars = model_covars,
    selected_covars = selected_covars,
    iv = iv,
    instruments = instruments,
    weights_var = weights_var,
    model_type = model_type,
    verbose = verbose,
    return_model = return_model
  )
  return(res)
}

validate_df <- function(df, model_covars, selected_covars = NULL) {
  #' Ensures dataframe is valid for model specification, i.e., dropping singleton nodes
  #' (which interfere with robust SE estimation). In theory, could also drop NAs, and
  #' zero-variance columns, but this is handled internally in alethea::compute_ATEs.
  #'
  #' @param df A data frame containing the main sample data.
  #' @param model_covars A vector of model block-id covariates.
  #' @param selected_covars A list of lasso-selected covariates for each outcome.
  #'
  #' @return An updated dataframe to be passed to alethea::compute_ATEs.
  #'
  n <- nrow(df)
  model_covars_sums <- df %>% select(all_of(model_covars)) %>% colSums()
  # check if sum = 1 (i.e., all zero but one) or = n -1 (i.e., all one but one zero)
  model_covars_singleton <- model_covars[(model_covars_sums == 1) | (model_covars_sums == (n - 1))]
  df <- drop_singletons(df, model_covars_singleton)
  return(df)
}

drop_singletons <- function(df, covars) {
  #' Helper funciton to drop singleton nodes from dataframe, i.e., columns
  #' that are all 0s but one 1, or all 1s but one 0. This occurs in some of the finer HTE
  #' sample splits with the required block-id indicators.
  #'
  #' @param df A data frame containing the main sample data.
  #' @param covars A vector of strings indicating the covariates to check for singletons.
  #'
  #' @return An updated dataframe to be passed to alethea::compute_ATEs.
  #'
  rows_to_drop <- c()
  for (c in covars) {
    if (sum(df[[c]]) == 1) { # if covar only has one `1`
      rows_to_drop <- c(rows_to_drop, which(df[[c]] == 1))
    } else if (sum(df[[c]]) == (nrow(df) - 1)) { # if covar only has one `0`
      rows_to_drop <- c(rows_to_drop, which(df[[c]] == 0))
    }
  }
  rows_to_drop <- unique(rows_to_drop)
  if (length(rows_to_drop) != 0) {
    message(paste("Dropping", length(rows_to_drop), "rows:", rows_to_drop))
    df <- df[-rows_to_drop, ]
  }
  return(df)
}

get_extended_df <- function(df_model_list) {
  #' Helper funciton to extract R^2 and other model stats from a list of regression models.
  #'
  #' @param df_model_list A list of regression models.
  #'
  #' @return A dataframe containing regression results and column for R^2.
  #'
  df <- lapply(df_model_list, function(x) x$df) %>% bind_rows(.id = "dv")
  r2 <- lapply(df_model_list, function(x) summary(x$mod)$r.squared) %>% unlist()
  df$rsquared <- r2
  return(df)
}

standardize_survey_question <- function(question, weight) {
  #' Standardizes survey questions relative to weighted mean and standard deviation.
  #'
  #' @param question A vector of survey question responses.
  #' @param weight A vector of weights for the survey question responses.
  #'
  #' @return A vector of standardized survey question responses.
  #'
  weighted_q_mean <- weighted.mean(question, weight, na.rm = TRUE)
  weighted_q_std <- Hmisc::wtd.var(question, weight, na.rm = TRUE) %>% sqrt()
  standardized_q <- (question - weighted_q_mean) / weighted_q_std
  return(scale(standardized_q))
}
