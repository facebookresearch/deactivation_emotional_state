# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

#####################################################
###   heterogeneous - saturated_reg_functions.R   ###
#####################################################

run_hte_saturated <- function(
  group,
  sample,
  selected_covars,
  model_covars,
  iv,
  outcomes_list = NULL,
  model_type = c("PLATE"),
  weights_var = "weights_w4",
  verbose = FALSE
) {
  #' Estimates HTEs for all outcomes for a given group covariate in a saturated regression.
  #' Creates interactions with group variable and all other covariates.
  #' 
  #' @param group A string indicating the covariate to estimate HTEs for (e.g., 'participant_gender').
  #' @param sample A data frame containing the main sample data.
  #' @param selected_covars A list of lasso-selected covariates for each outcome.
  #' @param model_covars A vector of model block-id covariates.
  #' @param iv A string indicating the instrumental variable to use.
  #' @param outcomes_list A list of outcome variables to estimate HTEs for.
  #' @param model_type A string indicating the estimate to recover (e.g., 'PLATE'). Default is 'PLATE'.
  #' @param weights_var A string indicating the variable to use for weights. Default is 'weights_w4'.
  #' @param verbose A boolean indicating whether to print progress messages. Default is FALSE.
  #'
  #' @return A dataframe containing the HTE estimates for each subgroup.
  #'
  message(sprintf("### HTE: %s ###", toupper(group)))
  sample_interactions <- add_subgroup_interactions(
    df = sample,
    group = group,
    model_covars = model_covars,
    selected_covars = selected_covars,
    iv = iv
  )
  res <- bind_rows(
    lapply(
      outcomes_list,
      run_saturated_reg,
      group = group,
      sample = sample_interactions,
      selected_covars = selected_covars,
      model_covars = model_covars,
      iv = iv,
      weights_var = weights_var,
      verbose = verbose,
      model_type = model_type
    ),
    .id = "dv"
  )
  return(res)
}

add_subgroup_interactions <- function(df, group, model_covars, selected_covars, iv) {
  #' Helper for `run_hte_saturated`. Adds interactions with all variables for a given group covariate.
  #'
  #' @param df A data frame containing the main sample data.
  #' @param group A string indicating the covariate to estimate HTEs for (e.g., 'participant_gender').
  #' @param model_covars A vector of model block-id covariates.
  #' @param selected_covars A list of lasso-selected covariates for each outcome.
  #' @param iv A string indicating the instrumental variable to use.
  #' @param outcomes_list A list of outcome variables to estimate HTEs for.
  #'
  #' @return An updated data frame featuing interaction columns for each subgroup.
  #'
  # drop any NA rows of group
  df <- df %>% filter(!is.na(get(group)))
  # create dummies for group variable
  df <- df %>% dummy_cols(group)
  subgroup_names <- paste0(group, "_", unique(df[[group]]))
  # add interactions for treated, IV variables; selected and modle covariates
  all_selected_covars <- unique(unlist(selected_covars))
  df <- df %>%
    mutate(across(all_of(subgroup_names), ~ . * treated, .names = "treated_{.col}")) %>%
    mutate(across(all_of(subgroup_names), ~ . * get(iv), .names = paste0(iv, "_{.col}"))) %>%
    bind_cols(
      lapply(subgroup_names, add_covar_interactions, df = df, covars = all_selected_covars)
    ) %>%
    bind_cols(
      lapply(subgroup_names, add_covar_interactions, df = df, covars = model_covars)
    )
  if (group %in% c("partyid5", "participant_male_or_not")) {
      df <- df %>% janitor::clean_names()
  }
  return(df)
}

add_covar_interactions <- function(df, subgroup, covars) {
  #' Helper for `add_subgroup_interactions`. Adds interaction between subgroup and covariates.
  #'
  #' @param df A data frame containing the main sample data.
  #' @param subgroup A string indicating a level of the original group covariate.
  #' @param covars A vector of covariates to interact with the subgroup.
  #'
  #' @return An updated data frame featuing interaction columns for subgroup x covars.
  #'
  df %>%
    transmute(
      across(
        all_of(covars),
          ~ as.numeric(df[[subgroup]]) * as.numeric(.),
        .names = paste0(subgroup, "_{col}")
      )
    )
}

run_saturated_reg <- function(
  dv_dict,
  group,
  sample,
  selected_covars,
  model_covars,
  iv,
  weights_var,
  model_type = c("PLATE"),
  verbose = FALSE,
  return_model = TRUE
) {
  #' Estimates saturated regression for a given group covariate to obtain HTEs.
  #' Constructs linear hypothesis to test for significance of subgroup effects.
  #'
  #' @param dv_dict A list containing the outcome variable and its label.
  #' @param group A string indicating the covariate to estimate HTEs for (e.g., 'participant_gender').
  #' @param sample A data frame containing the main sample data.
  #' @param selected_covars A list of lasso-selected covariates for each outcome.
  #' @param model_covars A vector of model block-id covariates.
  #' @param iv A string indicating the instrumental variable to use.
  #' @param weights_var A string indicating the variable to use for weights. Default is 'weights_w4'.
  #' @param model_type A string indicating the estimate to recover (e.g., 'PLATE'). Default is 'PLATE'.
  #' @param verbose A boolean indicating whether to print progress messages. Default is FALSE.
  #' @param return_model A boolean indicating whether to return the model object. Default is TRUE.
  #'
  #' @return A dataframe containing the HTE estimates for each subgroup and the F-test p-value.
  #'
  dv <- dv_dict[[1]]
  dv_label <- dv_dict[[2]]
  subgroup_names <- paste0(group, "_", unique(sample[[group]]))
  if (is.list(selected_covars)) {
    selected_covars <- selected_covars[[dv]]
  }
  # get all of the names of the variables that are interacted with the group variable
  interacted_instruments <- colnames(sample %>% select(starts_with("treated_")))
  interacted_ivs <- colnames(sample %>% select(starts_with(paste0(iv, "_"))))
  interacted_selected_covars <- unlist(
    lapply(subgroup_names, function(x) paste(x, selected_covars, sep = "_"))
  )
  interacted_model_covars <- unlist(
    lapply(subgroup_names, function(x) paste(x, model_covars, sep = "_"))
  )
  if (group %in% c("partyid5", "participant_male_or_not")) {
    interacted_selected_covars <- janitor::make_clean_names(interacted_selected_covars)
    interacted_model_covars <- janitor::make_clean_names(interacted_model_covars)
  }
  # drop any NA rows of the interacted groups
  sample <- sample %>%
    filter(if_any(all_of(c(interacted_selected_covars, interacted_model_covars)), ~ !is.na(.)))
  model <- run_outcome(
    df = sample,
    dv = dv,
    dv_label = dv_label,
    model_covars = interacted_model_covars,
    selected_covars = interacted_selected_covars,
    iv = interacted_ivs,
    instruments = interacted_instruments,
    weights_var = weights_var,
    model_type = model_type,
    verbose = verbose,
    return_model = return_model
  )
  df <- model$df
  df$subgroup <- subgroup_names
  # estimate linear hypothesis to obtain F-test p-value
  if (length(interacted_ivs) > 1) {
    reference_var <- interacted_ivs[1]
    hypotheses <- sapply(interacted_ivs[-1], function(x) paste(reference_var, "-", x, "= 0"))
  } else {
    hypotheses <- character(0)
    print("Not enough variables to create hypotheses.")
  }
  # If there are hypotheses to test, proceed
  if (length(hypotheses) > 0) {
    hyp <- linearHypothesis(model$mod, hypotheses)
    df$f_test_pval <- unique(na.omit(hyp[["Pr(>Chisq)"]]))
    df$ate_var <- row.names(df)
  } else {
    df$f_test_pval <- NA
    df$ate_var <- row.names(df)
  }
  return(df)
}
