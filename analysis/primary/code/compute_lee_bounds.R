# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

##########################################
###   primary - compute_lee_bounds.R   ###
##########################################

source("lib/compute_effects.R")
source("lib/constants.R")
source("lib/outcome_dictionaries.R")

main <- function() {
  weighted_data_ids <- readRDS("analysis/primary/input/weighted_data_ids.rds")
  attriters <- readRDS("analysis/primary/input/attriters.rds")

  main_sample_fb <- readRDS("analysis/primary/input/main_sample_fb.rds")
  main_sample_ig <- readRDS("analysis/primary/input/main_sample_ig.rds")

  uncut_sample_fb <- readRDS("analysis/primary/input/uncut_sample_fb.rds")
  uncut_sample_ig <- readRDS("analysis/primary/input/uncut_sample_ig.rds")

  lasso_covars_fb <- readRDS("analysis/primary/input/lasso_covars_fb.rds")
  lasso_covars_ig <- readRDS("analysis/primary/input/lasso_covars_ig.rds")

  selected_covars_fb <- readRDS("analysis/primary/output/selected_covars_fb.rds")
  selected_covars_ig <- readRDS("analysis/primary/output/selected_covars_ig.rds")

  # Make variables and outcome labels
  emotion_outcomes_label_dict <- make_emotion_outcomes_label_dict()
  emotion_outcomes_baseline_dict <- make_emotion_outcomes_baseline_dict()
  emotion_outcomes <- process_dict_to_list(emotion_outcomes_label_dict)

  # Get ids of users who completed w2
  completed_w2_ids <- unlist(weighted_data_ids$participant_user_id[weighted_data_ids$completed_w2])

  # Build samples for Lee bound estimation
  sample_with_attriters_fb <- build_sample_with_attriters(
    main_sample = main_sample_fb,
    uncut_sample = uncut_sample_fb,
    attriters = attriters,
    completed_w2_ids = completed_w2_ids,
    lasso_covars = lasso_covars_fb,
    plat = "FB"
  )
  sample_with_attriters_ig <- build_sample_with_attriters(
    main_sample = main_sample_ig,
    uncut_sample = uncut_sample_ig,
    attriters = attriters,
    completed_w2_ids = completed_w2_ids,
    lasso_covars = lasso_covars_ig,
    plat = "IG"
  )
  # Calculate Lee bounds
  emotion_leebounds_fb <- lapply(
    emotion_outcomes$all,
    build_primary_leebounds,
    d = sample_with_attriters_fb,
    selected_covars = selected_covars_fb,
    iv = iv_fb,
    leebounds_weights_var = "weights_w2",
    effects_weights_var = "weights_w2"
  ) %>%
    bind_rows()
  emotion_leebounds_ig <- lapply(
    emotion_outcomes$all,
    build_primary_leebounds,
    d = sample_with_attriters_ig,
    selected_covars = selected_covars_ig,
    iv = iv_fb,
    leebounds_weights_var = "weights_w2",
    effects_weights_var = "weights_w2"
  ) %>%
    bind_rows()

  saveRDS(emotion_leebounds_fb, "analysis/primary/output/emotion_leebounds_fb.rds")
  saveRDS(emotion_leebounds_ig, "analysis/primary/output/emotion_leebounds_ig.rds")
}


build_sample_with_attriters <- function(
  main_sample,
  uncut_sample,
  attriters,
  completed_w2_ids,
  lasso_covars,
  plat = "FB"
) {
  #' Builds sample of main sample respondents and attriters for Lee bound estimation.
  #'
  #' @param main_sample A data frame containing the main sample.
  #' @param uncut_sample A data frame containing the uncut sample.
  #' @param attriters A data frame respondents and attriters.
  #' @param completed_w2_ids A vector of participant_user_id's of users who completed baseline survey.
  #' @param lasso_covars A vector of covariates to be used in the lasso.
  #' @param plat A string indicating the recruitment_platform_string ('FB' or 'IG'). Default is 'FB'.
  #'
  #' @return An updated version of main sample with attriters.
  #'
  baseline_varname <- sprintf("participant_time_spent_relative_to_baseline_%s", tolower(plat))
  # We use the max of low_usage sample (vs min of main_sample, i.e. high usage)
  # to remove outlier values on the baseline_ts variable for IG
  low_usage_sample_ids <- uncut_sample %>% filter(!main_sample) %>% select(participant_user_id) %>% unlist()
  low_usage_sample <- attriters %>% filter(participant_in_low_usage_sample)
  thresh_15min <- max(low_usage_sample[[baseline_varname]], na.rm = TRUE)
  # filter to attriting individuals with more than 15 minutes of usage
  attriters <- attriters %>%
    filter(study_experiment_group %in% c("deactivation", "deactivation_control")) %>%
    filter(has_withdrawn) %>%
    filter(recruitment_platform_string == plat) %>%
    filter(!!as.name(baseline_varname) > thresh_15min) %>%
    mutate(participant_user_id=as.character(NA))
  # factorize lasso covariates
  factor_lasso_covars <- c("participant_male_or_not", "participant_race_5_groups", "participant_political_ideology", "participant_party_identification", "participant_2016_voter_string")
  attriters <- attriters %>%
    alethea::make_factor_bins(covars = factor_lasso_covars, remove_most_frequent_dummy = FALSE)
  # select attriting individuals and relevant covariates and merge
  vars_to_select <- c("participant_user_id", baseline_varname, lasso_covars)
  attriters <- attriters$d %>% select(all_of(vars_to_select)) %>% mutate(main_sample = FALSE)
  sample_with_attriters <- main_sample %>%
    bind_rows(attriters) %>%
    alethea::impute_covars(
      covars = lasso_covars,
      weights_var = "weights_w2",
      sample = "main_sample",
      deact_impute = TRUE
    )
  res <- sample_with_attriters$d %>% filter(!is.na(participant_user_id))
  return(res)
}


build_primary_leebounds <- function(
  dv_prim,
  d,
  selected_covars,
  iv,
  leebounds_weights_var,
  effects_weights_var
) {
  #' Estimates ITT and Lee bounds for a primary outcome.
  #'
  #' @param dv_prim A list containing the primary outcome and its label.
  #' @param d A data frame containing the sample with attriters.
  #' @param selected_covars A list of selected covariates for each primary outcome.
  #' @param iv A string indicating the instrumental variable.
  #' @param leebounds_weights_var A string indicating the weights variable for Lee bounds.
  #' @param effects_weights_var A string indicating the weights variable for ITT.
  #'
  #' @return A data frame containing the primary outcome, its label, ITT, Lee bounds.
  #'
  dv <- dv_prim[[1]]
  dv_label <- dv_prim[[2]]
  selected_covars <- selected_covars[[dv]]
  primary_leebounds <- alethea::leebounds.covar(
    d = d,
    dv = dv,
    covars = selected_covars,
    weights_var = leebounds_weights_var,
    n_buckets = 10
  )
  # estimate ITT with bucket
  d_updated <- d %>%
    left_join(primary_leebounds[["covar.buckets"]], by = c("participant_user_id")) %>%
    alethea::make_factor_bins(c("predicted_bucket"))
  itt_leebounds_fb <- run_outcome(
    df = d_updated$d %>% filter(main_sample),
    dv = dv,
    dv_label = dv_label,
    model_covars = NULL,
    selected_covars = d_updated$updated_covars,
    iv = iv,
    weights_var = effects_weights_var,
    model_type = "PATE"
  )
  leebound_outputs <- data.frame(
    dv = dv,
    dv_label = dv_label,
    itt = itt_leebounds_fb$ate,
    ci_low = itt_leebounds_fb$ci_low,
    ci_hi = itt_leebounds_fb$ci_hi,
    lee_lower_tight = primary_leebounds$covar.lower,
    lee_upper_tight = primary_leebounds$covar.upper
  )
  return(leebound_outputs)
}


## Execute
main()
