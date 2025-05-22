# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

#################################################
###   heterogeneous - compute_het_effects.R   ###
#################################################

library(car)
library(tidyr)
library(stringr)
library(Hmisc)
library(fastDummies)
library(janitor)

devtools::load_all("~/alethea")

source("lib/compute_effects.R")
source("lib/constants.R")
source("lib/outcome_dictionaries.R")
source("analysis/heterogeneous/code/saturated_reg_functions.R")

main <- function() {
  main_sample_fb <- readRDS("analysis/heterogeneous/input/main_sample_fb.rds")
  main_sample_ig <- readRDS("analysis/heterogeneous/input/main_sample_ig.rds")

  model_covars_fb <- readRDS("analysis/heterogeneous/input/model_covars_fb.rds")
  model_covars_ig <- readRDS("analysis/heterogeneous/input/model_covars_ig.rds")

  selected_covars_fb <- readRDS("analysis/heterogeneous/input/selected_covars_fb.rds")
  selected_covars_ig <- readRDS("analysis/heterogeneous/input/selected_covars_ig.rds")

  # Make variables and outcome labels
  emotion_outcomes_label_dict <- make_emotion_outcomes_label_dict(outcomes = "index")
  emotion_outcomes_baseline_dict <- make_emotion_outcomes_baseline_dict(outcomes = "index")
  emotion_outcomes <- process_dict_to_list(emotion_outcomes_label_dict)

  main_sample_fb <- main_sample_fb %>%
    mutate(fb_ts_base_wtd_base_win_quint = as.factor(ntile(fb_ts_base_wtd_base_win, 5)))
  main_sample_ig <- main_sample_ig %>%
    mutate(ig_ts_base_wtd_base_win_quint = as.factor(ntile(ig_ts_base_wtd_base_win, 5)))

  ### Emotional state moderators ###
  message(sprintf("Estimating HTEs for emotional state moderators"))
  primary_mods <- make_primary_moderators()
  primary_HTEs_fb <- lapply(
    names(primary_mods$group_fb),
    run_hte_split,
    sample = main_sample_fb,
    selected_covars = selected_covars_fb,
    model_covars = model_covars_fb,
    iv = iv_fb,
    outcomes_list = emotion_outcomes$all,
    model_type = "PLATE",
    weights_var = "weights_w4"
  )
  primary_HTEs_ig <- lapply(
    names(primary_mods$group_ig),
    run_hte_split,
    sample = main_sample_ig,
    selected_covars = selected_covars_ig,
    model_covars = model_covars_ig,
    iv = iv_ig,
    outcomes_list = emotion_outcomes$all,
    model_type = "PLATE",
    weights_var = "weights_w4"
  )
  primary_HTEs_comp_fb <- compare_female_below_median_age_ests(
    sample = main_sample_fb,
    res = primary_HTEs_fb,
    selected_covars = selected_covars_fb,
    model_covars = model_covars_fb,
    iv = iv_fb,
    plat = "FB"
  )
  primary_HTEs_comp_ig <- compare_female_below_median_age_ests(
    sample = main_sample_ig,
    res = primary_HTEs_ig,
    selected_covars = selected_covars_ig,
    model_covars = model_covars_ig,
    iv = iv_ig,
    plat = "IG"
  )
  # Saturated regressions
  primary_HTEs_sat_fb <- lapply(
    names(primary_mods$group_fb),
    run_hte_saturated,
    sample = main_sample_fb,
    model_covars = model_covars_fb,
    selected_covars = selected_covars_fb,
    iv = iv_fb,
    model_type = "PLATE",
    weights_var = "weights_w4",
    outcomes_list = emotion_outcomes$all
  )
  primary_HTEs_sat_ig <- lapply(
    names(primary_mods$group_ig),
    run_hte_saturated,
    sample = main_sample_ig,
    model_covars = model_covars_ig,
    selected_covars = selected_covars_ig,
    iv = iv_ig,
    model_type = "PLATE",
    weights_var = "weights_w4",
    outcomes_list = emotion_outcomes$all
  )
  saveRDS(primary_HTEs_fb, "analysis/heterogeneous/output/primary_HTEs_fb.rds")
  saveRDS(primary_HTEs_ig, "analysis/heterogeneous/output/primary_HTEs_ig.rds")
  saveRDS(primary_HTEs_comp_fb, "analysis/heterogeneous/output/primary_HTEs_comp_fb.rds")
  saveRDS(primary_HTEs_comp_ig, "analysis/heterogeneous/output/primary_HTEs_comp_ig.rds")
  saveRDS(primary_HTEs_sat_fb, "analysis/heterogeneous/output/primary_HTEs_sat_fb.rds")
  saveRDS(primary_HTEs_sat_ig, "analysis/heterogeneous/output/primary_HTEs_sat_ig.rds")

  ### Pre-registered moderators ###
  message(sprintf("Estimating HTEs for secondary (pre-registered) moderators"))
  secondary_mods <- make_secondary_moderators()
  secondary_HTEs_fb <- lapply(
    names(secondary_mods$group),
    run_hte_split,
    sample = main_sample_fb,
    selected_covars = selected_covars_fb,
    model_covars = model_covars_fb,
    iv = iv_fb,
    outcomes_list = emotion_outcomes$all,
    model_type = "PLATE",
    weights_var = "weights_w4"
  )
  secondary_HTEs_ig <- lapply(
    names(secondary_mods$group),
    run_hte_split,
    sample = main_sample_ig,
    selected_covars = selected_covars_ig,
    model_covars = model_covars_ig,
    iv = iv_ig,
    outcomes_list = emotion_outcomes$all,
    model_type = "PLATE",
    weights_var = "weights_w4"
  )
  # Saturated regressions
  secondary_HTEs_sat_fb <- lapply(
    names(secondary_mods$group),
    run_hte_saturated,
    sample = main_sample_fb,
    model_covars = model_covars_fb,
    selected_covars = selected_covars_fb,
    iv = iv_fb,
    model_type = "PLATE",
    weights_var = "weights_w4",
    outcomes_list = emotion_outcomes$all
  )

  secondary_HTEs_sat_ig <- lapply(
    names(secondary_mods$group),
    run_hte_saturated,
    sample = main_sample_ig,
    model_covars = model_covars_ig,
    selected_covars = selected_covars_ig,
    iv = iv_ig,
    model_type = "PLATE",
    weights_var = "weights_w4",
    outcomes_list = emotion_outcomes$all
  )
  saveRDS(secondary_HTEs_fb, "analysis/heterogeneous/output/secondary_HTEs_fb.rds")
  saveRDS(secondary_HTEs_ig, "analysis/heterogeneous/output/secondary_HTEs_ig.rds")
  saveRDS(secondary_HTEs_sat_fb, "analysis/heterogeneous/output/secondary_HTEs_sat_fb.rds")
  saveRDS(secondary_HTEs_sat_ig, "analysis/heterogeneous/output/secondary_HTEs_sat_ig.rds")
}


run_hte_split <- function(
  group_covar,
  sample,
  selected_covars,
  model_covars,
  iv,
  outcomes_list,
  model_type = c("PLATE"),
  weights_var = "weights_w4",
  verbose = FALSE
) {
  #' Estimates HTEs for all outcomes for a given group covariate, looping over each unique value.
  #'
  #' @param group_covar A string indicating the covariate to estimate HTEs for (e.g., 'participant_gender').
  #' @param sample A data frame containing the main sample data.
  #' @param selected_covars A list of lasso-selected covariates for each outcome.
  #' @param model_covars A vector of model block-id covariates.
  #' @param iv A string indicating the instrumental variable to use.
  #' @param outcomes_list A list of outcome variables to estimate HTEs for.
  #' @param model_type A string indicating the estimate to recover (e.g., 'PLATE'). Default is 'PLATE'.
  #' @param weights_var A string indicating the variable to use for weights. Default is 'weights_w4'.
  #' @param verbose A boolean indicating whether to print progress messages. Default is FALSE.
  #'
  #' @return A data frame containing the HTE estimates for each subgroup.
  #'
  message(sprintf("### HTE: %s ###", toupper(group_covar)))
  if (group_covar == "participant_gender") {
    # drop individuals not identifying as male/female for participant_gender HTE
    sample <- sample %>% filter(participant_gender %in% c("male", "female"))
  }
  subgroups <- unique(sample[[group_covar]])
  subgroups <- subgroups[!is.na(subgroups)]
  group_HTEs <- bind_rows(
    lapply(
      subgroups,
      run_sugbroup_hte_split,
      group_covar = group_covar,
      sample = sample,
      selected_covars = selected_covars,
      model_covars = model_covars,
      iv = iv,
      outcomes_list = outcomes_list,
      model_type = model_type,
      weights_var = weights_var,
      verbose = verbose
    )
  )
  return(group_HTEs)
}


run_sugbroup_hte_split <- function(
  subgroup,
  group_covar,
  sample,
  selected_covars,
  model_covars,
  iv,
  outcomes_list,
  model_type = c("PLATE"),
  weights_var = "weights_w4",
  verbose = FALSE
) {
  #' Estimates HTEs for all outcomes for a given subgroup.
  #'
  #' @param subgroup A string indicating the subgroup to estimate HTEs for (e.g., 'female').
  #' @param group_covar A string indicating the covariate to estimate HTEs for (e.g., 'participant_gender').
  #' @param sample A data frame containing the main sample data.
  #' @param selected_covars A list of lasso-selected covariates for each outcome.
  #' @param model_covars A vector of model block-id covariates.
  #' @param iv A string indicating the instrumental variable to use.
  #' @param outcomes_list A list of outcome variables to estimate HTEs for.
  #' @param model_type A string indicating the estimate to recover (e.g., 'PLATE'). Default is 'PLATE'.
  #' @param weights_var A string indicating the variable to use for weights. Default is 'weights_w4'.
  #' @param verbose A boolean indicating whether to print progress messages. Default is FALSE.
  #'
  #' @return A dataframe containing the HTE estimates for the subgroup.
  #'
  message(sprintf("# %s: \"%s\" #", group_covar, toupper(subgroup)))
  sample_filt <- sample %>% filter(!!as.symbol(group_covar) == subgroup)
  res <- bind_rows(
    lapply(
      outcomes_list,
      run_outcome_wrapper,
      sample = sample_filt,
      selected_covars = selected_covars,
      model_covars = model_covars,
      weights_var = weights_var,
      iv = iv,
      verbose = verbose,
      model_type = model_type
    ),
    .id = "dv"
  )
  res <- res %>%
    mutate(group = group_covar, subgroup = as.character(subgroup)) %>%
    select(group, subgroup, everything())
  return(res)
}


compare_female_below_median_age_ests <- function(
  sample,
  res,
  selected_covars,
  model_covars,
  iv,
  plat = "FB"
) {
  #' Compares main female below age median HTE with other estimates from different specifications.
  #'
  #' @param sample A data frame containing the main sample data.
  #' @param selected_covars A list of lasso-selected covariates for each outcome.
  #' @param model_covars A vector of model block-id covariates.
  #' @param iv A string indicating the instrumental variable to use.
  #' @param plat A string indicating the recruitment_platform_string to use (e.g., 'FB'). Default is 'FB'.
  #'
  #' @return A dataframe containing the different HTE estimates.
  #'
  covars_str <- paste(
    paste0(selected_covars[["emot_post"]], collapse = " + "),
    paste0(model_covars, collapse = " + "),
    sep = " + "
  )
  # need to remove participant_gender bc of collineraity (as we're looking at subgroups of participant_gender x age)
  covars_str <- str_replace(covars_str, "\\+ participant_male_or_not_bin_male", "")

  # get baseline use term
  if (plat == "FB") {
    baseline_use_var <- "fb_ts_base_wtd_base_win"
    baseline_use_quint_var <- "fb_ts_base_wtd_base_win_quint"
  } else {
    baseline_use_var <- "ig_ts_base_wtd_base_win"
    baseline_use_quint_var <- "ig_ts_base_wtd_base_win_quint"
  }

  # Estimate 1: Female 18-24 in subsample estimation
  res <- res[[1]] %>% filter(subgroup == "female_FALSE")
  est1 <- list(
    `Estimate` = res %>% pull(ate),
    `Std. Error` = res %>% pull(std_error),
    `t value` = res %>% pull(tstat),
    `Pr(>|t|)` = res %>% pull(pval)
  )

  # Estimate 2: Full sample w/ interaction terms
  f <- as.formula(
    paste0(
      "emot_post ~ 0 + ",
      "gender_age_above_med * ",
      iv,
      " + ",
      covars_str,
      " | 0 + ",
      "gender_age_above_med * treated",
      " + ",
      covars_str
    )
  )
  est2 <- get_ests(sample, f)

  # Estimate 3: + Deactivation x baseline use
  f <- as.formula(
    paste0(
      "emot_post ~ 0 + ",
      iv,
      " + ",
      "gender_age_above_med * ",
      iv,
      " + ",
      baseline_use_var,
      " * ",
      iv,
      " + ",
      covars_str,
      " |  0 + ",
      "treated",
      " + ",
      "gender_age_above_med * treated",
      " + ",
      baseline_use_var,
      " * treated",
      " + ",
      covars_str
    )
  )
  est3 <- get_ests(sample, f)

  # Estimate 4: + Deactivation x baseline use quintiles
  f <- as.formula(
    paste0(
      "emot_post ~ 0 + ",
      iv,
      " + ",
      "gender_age_above_med * ",
      iv,
      " + ",
      baseline_use_quint_var,
      " * ",
      iv,
      " + ",
      covars_str,
      " |  0 + ",
      "treated",
      " + ",
      "gender_age_above_med * treated",
      " + ",
      baseline_use_quint_var,
      " * treated",
      " + ",
      covars_str
    )
  )
  est4 <- get_ests(sample, f)

  # Estimate 5: + Deactivation x emotional state
  f <- as.formula(
    paste0(
      "emot_post ~ 0 + ",
      iv,
      " + ",
      "gender_age_above_med * ",
      iv,
      " + ",
      "emot_pre_above_med * ",
      iv,
      " + ",
      covars_str,
      " |  0 + ",
      "treated",
      " + ",
      "gender_age_above_med * treated",
      " + ",
      "emot_pre_above_med * treated",
      " + ",
      covars_str
    )
  )
  est5 <- get_ests(sample, f)

  # combined estimates
  ests <- rbind(est1, est2, est3, est4, est5) %>% as.data.frame()
  return(ests)
}


get_ests <- function(sample, f, vcov_type = "HC2") {
  #' Helper function for `compare_female_below_median_age_ests` to estimate models and extract estimates.
  #'
  #' @param sample A data frame containing the main sample data.
  #' @param f A formula object indicating the model to estimate.
  #' @param vcov_type A string indicating the type of variance estiamtor to use. Default is 'HC2'.
  #'
  #' @return A dataframe row of LATE and standard errors for the model.
  #'
  subgroup_reg <- ivreg(f, data = sample, weights = sample$weights_w4)
  b_vcov <- vcovHC(subgroup_reg, type = vcov_type)
  ct <- coeftest(subgroup_reg, vcov = b_vcov)
  return(ct["D_i", ])
}


## Execute
main()
