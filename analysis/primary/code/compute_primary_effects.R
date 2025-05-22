# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

###############################################
###   primary - compute_primary_effects.R   ###
###############################################

source("lib/compute_effects.R")
source("lib/constants.R")
source("lib/outcome_dictionaries.R")
# NOTE: alethea, dplyr loaded in source

main <- function() {
  main_sample_fb <- readRDS("analysis/primary/input/main_sample_fb.rds")
  main_sample_ig <- readRDS("analysis/primary/input/main_sample_ig.rds")

  uncut_sample_fb <- readRDS("analysis/primary/input/uncut_sample_fb.rds")
  uncut_sample_ig <- readRDS("analysis/primary/input/uncut_sample_ig.rds")

  lasso_covars_fb <- readRDS("analysis/primary/input/lasso_covars_fb.rds")
  lasso_covars_ig <- readRDS("analysis/primary/input/lasso_covars_ig.rds")

  model_covars_fb <- readRDS("analysis/primary/input/model_covars_fb.rds")
  model_covars_ig <- readRDS("analysis/primary/input/model_covars_ig.rds")

  # Make variables and outcome labels
  emotion_outcomes_label_dict <- make_emotion_outcomes_label_dict()
  emotion_outcomes_baseline_dict <- make_emotion_outcomes_baseline_dict()
  emotion_outcomes <- process_dict_to_list(emotion_outcomes_label_dict)
  emotion_outcomes_og <- process_dict_to_list(make_emotion_outcomes_og_units_label_dict())

  # Estimate lasso to select covariates
  selected_covars_fb <- unlist(
    lapply(
      emotion_outcomes$vars,
      select_covars_lasso,
      df = main_sample_fb,
      dv_pre_list = emotion_outcomes_baseline_dict,
      weights_var = "weights_w4",
      lasso_covars = lasso_covars_fb
    ),
    recursive = FALSE
  )
  selected_covars_ig <- unlist(
    lapply(
      emotion_outcomes$vars,
      select_covars_lasso,
      df = main_sample_ig,
      dv_pre_list = emotion_outcomes_baseline_dict,
      weights_var = "weights_w4",
      lasso_covars = lasso_covars_ig
    ),
    recursive = FALSE
  )

  saveRDS(selected_covars_fb, "analysis/primary/output/selected_covars_fb.rds")
  saveRDS(selected_covars_ig, "analysis/primary/output/selected_covars_ig.rds")

  ##############################
  ### Primary Outcomes: Main ###
  ##############################
  message("Estimating primary outcomes...")
  res_fb <- lapply(
    emotion_outcomes$all,
    run_outcome_wrapper,
    sample = main_sample_fb,
    selected_covars = selected_covars_fb,
    model_covars = model_covars_fb,
    iv = iv_fb,
    weights_var = "weights_w4",
    verbose = FALSE
  ) %>%
    bind_rows(.id = "dv")
  res_ig <- lapply(
    emotion_outcomes$all,
    run_outcome_wrapper,
    sample = main_sample_ig,
    selected_covars = selected_covars_ig,
    model_covars = model_covars_ig,
    iv = iv_ig,
    weights_var = "weights_w4",
    verbose = FALSE
  ) %>%
    bind_rows(.id = "dv")

  #####################################
  ### Primary Outcomes: First Stage ###
  #####################################
  message("Estimating primary outcomes first stage...")
  res_fb_first_stage <- estimate_first_stage(
    users = main_sample_fb,
    selected_covars = selected_covars_fb,
    model_covars = model_covars_fb,
    iv = iv_fb,
    weights_var = "weights_w4"
  )
  res_ig_first_stage <- estimate_first_stage(
    users = main_sample_ig,
    selected_covars = selected_covars_ig,
    model_covars = model_covars_ig,
    iv = iv_ig,
    weights_var = "weights_w4"
  )

  ########################################
  ### Primary Outcomes: Unstandardized ###
  ########################################
  message("Estimating primary outcomes in original units...")
  # replace baseline variable (always at end) with baseline variable in original units
  selected_covars_og_fb <- setNames(selected_covars_fb, c("", emotion_outcomes_og$vars))
  selected_covars_og_fb <- lapply(selected_covars_og_fb, function(x) {
      x[length(x)] <- paste0(x[length(x)], "_og")
      return(x)
  })
  selected_covars_og_ig <- setNames(selected_covars_ig, c("", emotion_outcomes_og$vars))
  selected_covars_og_ig <- lapply(selected_covars_og_ig, function(x) {
      x[length(x)] <- paste0(x[length(x)], "_og")
      return(x)
  })
  res_og_fb <- lapply(
    emotion_outcomes_og$all,
    run_outcome_wrapper,
    sample = main_sample_fb,
    selected_covars = selected_covars_og_fb,
    model_covars = model_covars_fb,
    iv = iv_fb,
    model_type = "PLATE",
    weights_var = "weights_w4",
    verbose = FALSE
  ) %>%
    bind_rows(.id = "dv")

  res_og_ig <- lapply(
    emotion_outcomes_og$all,
    run_outcome_wrapper,
    sample = main_sample_ig,
    selected_covars = selected_covars_og_ig,
    model_covars = model_covars_ig,
    iv = iv_ig,
    model_type = "PLATE",
    weights_var = "weights_w4",
    verbose = FALSE
  ) %>%
    bind_rows(.id = "dv")

  #####################################
  ### Primary Outcomes: Full Sample ###
  #####################################
  res_uncut_fb <- lapply(
    emotion_outcomes$all,
    run_outcome_wrapper,
    sample = uncut_sample_fb,
    selected_covars = selected_covars_fb,
    model_covars = model_covars_fb,
    iv = iv_fb,
    model_type = "PLATE",
    weights_var = "weights_full_pop",
    verbose = FALSE
  ) %>%
    bind_rows(.id = "dv")

  res_uncut_ig <- lapply(
    emotion_outcomes$all,
    run_outcome_wrapper,
    sample = uncut_sample_ig,
    selected_covars = selected_covars_ig,
    model_covars = model_covars_ig,
    iv = iv_ig,
    model_type = "PLATE",
    weights_var = "weights_full_pop",
    verbose = FALSE
  ) %>%
    bind_rows(.id = "dv")

  saveRDS(res_fb, "analysis/primary/output/res_fb.rds")
  saveRDS(res_ig, "analysis/primary/output/res_ig.rds")

  saveRDS(res_fb_first_stage, "analysis/primary/output/res_fb_first_stage.rds")
  saveRDS(res_ig_first_stage, "analysis/primary/output/res_ig_first_stage.rds")

  saveRDS(res_og_fb, "analysis/primary/output/res_og_fb.rds")
  saveRDS(res_og_ig, "analysis/primary/output/res_og_ig.rds")

  saveRDS(res_uncut_fb, "analysis/primary/output/res_uncut_fb.rds")
  saveRDS(res_uncut_ig, "analysis/primary/output/res_uncut_ig.rds")
}

estimate_first_stage <- function(users, selected_covars, model_covars, iv, weights_var) {
  #' Estimates first stage of IV regression for emotional state index.
  #'
  #' @param users A data frame containing the main sample.
  #' @param selected_covars A list of selected covariates for each outcome.
  #' @param model_covars A vector of model block-id covariates.
  #' @param iv A string indicating the instrumental variable to use.
  #' @param weights_var A string indicating the variable to use as weights.
  #'
  #' @return A regression object (`lm`) for the first stage.
  #'
  users <-  users %>%
    filter(if_any(all_of(
      c(iv, "treated", selected_covars[["emot_post"]], model_covars)
    ), ~ !is.na(.)))
  res <- lm(
    as.formula(paste(
      paste0(iv, " ~ treated"),
      paste(
        paste0(selected_covars[["emot_post"]], collapse = " + "),
        paste0(model_covars, collapse = " + "),
        sep = " + "
      ),
      sep = " + "
    )),
    users,
    weights = users[[weights_var]]
  )
  return(res)
}

## Execute
main()
