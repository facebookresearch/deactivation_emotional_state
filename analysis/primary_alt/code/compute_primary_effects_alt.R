# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

#######################################################
###   primary_alt - compute_primary_effects_alt.R   ###
#######################################################

devtools::load_all("~/alethea")

source("lib/compute_effects.R")
source("lib/constants.R")
source("lib/outcome_dictionaries.R")
library(dplyr)

main <- function() {
  main_sample_fb <- readRDS("analysis/primary_alt/input/main_sample_fb.rds")
  main_sample_ig <- readRDS("analysis/primary_alt/input/main_sample_ig.rds")

  model_covars_fb  <- readRDS("analysis/primary_alt/input/model_covars_fb.rds")
  model_covars_ig <- readRDS("analysis/primary_alt/input/model_covars_ig.rds")

  response_date_covars_fb <- readRDS("analysis/primary_alt/input/response_date_covars_fb.rds")
  response_date_covars_ig <- readRDS("analysis/primary_alt/input/response_date_covars_ig.rds")

  selected_covars_fb <- readRDS("analysis/primary_alt/input/selected_covars_fb.rds")
  selected_covars_ig <- readRDS("analysis/primary_alt/input/selected_covars_ig.rds")

  # Make variables and outcome labels
  primary_outcomes_dict <- make_emotion_outcomes_label_dict()
  primary_outcomes_baseline_dict <- make_emotion_outcomes_baseline_dict()
  primary_outcomes <- process_dict_to_list(primary_outcomes_dict)

  # Get alternative weights variable and levels
  alt_weights_fb <- make_alt_weights_labels_dict(plat = "FB")
  alt_weights_ig <- make_alt_weights_labels_dict(plat = "IG")
  alt_weights_dict_fb <- process_dict_to_list(alt_weights_fb)
  alt_weights_dict_ig <- process_dict_to_list(alt_weights_ig)

  ##############################
  ### Primary Outcomes: Main ###
  ##############################
  message("Running primary outcomes...")
  res_fb_list <- lapply(
    primary_outcomes$all,
    run_outcome_wrapper,
    sample = main_sample_fb,
    selected_covars = selected_covars_fb,
    model_covars = model_covars_fb,
    iv = iv_fb,
    model_type = "PLATE",
    weights_var = "weights_w4",
    verbose = FALSE,
    return_model = TRUE
  )
  res_fb <- get_extended_df(res_fb_list)

  res_ig_list <- lapply(
    primary_outcomes$all,
    run_outcome_wrapper,
    sample = main_sample_ig,
    selected_covars = selected_covars_ig,
    model_covars = model_covars_ig,
    iv = iv_ig,
    model_type = "PLATE",
    weights_var = "weights_w4",
    verbose = FALSE,
    return_model = TRUE
  )
  res_ig <- get_extended_df(res_ig_list)

  saveRDS(res_fb, "analysis/primary_alt/output/res_fb.rds")
  saveRDS(res_ig, "analysis/primary_alt/output/res_ig.rds")

  ###########################################################
  ### Primary Outcomes: Controls for day of response (W4) ###
  ###########################################################
  message("Running primary outcomes with controls for day of response...")
  response_date_fb <- lapply(
    primary_outcomes$all,
    run_outcome_wrapper,
    sample = main_sample_fb,
    selected_covars = selected_covars_fb,
    model_covars = c(model_covars_fb , response_date_covars_fb),
    iv = iv_fb,
    model_type = "PLATE",
    weights_var = "weights_w4"
  ) %>%
    bind_rows(.id = "dv")

  response_date_ig <- lapply(
    primary_outcomes$all,
    run_outcome_wrapper,
    sample = main_sample_ig,
    selected_covars = selected_covars_ig,
    model_covars = c(model_covars_ig, response_date_covars_ig),
    iv = iv_ig,
    model_type = "PLATE",
    weights_var = "weights_w4"
  ) %>%
    bind_rows(.id = "dv")

  ##########################################
  ### Primary Outcomes: Baseline Control ###
  ##########################################
  message("Running primary outcomes with baseline controls...")
  res_base_control_fb_list <- lapply(
    primary_outcomes$all,
    run_outcome_wrapper,
    sample = main_sample_fb,
    selected_covars = primary_outcomes_baseline_dict,
    model_covars = model_covars_fb,
    iv = iv_fb,
    model_type = "PLATE",
    weights_var = "weights_w4",
    verbose = FALSE,
    return_model = TRUE
  )
  res_base_control_fb <- get_extended_df(res_base_control_fb_list)

  res_base_control_ig_list <- lapply(
    primary_outcomes$all,
    run_outcome_wrapper,
    sample = main_sample_ig,
    selected_covars = primary_outcomes_baseline_dict,
    model_covars = model_covars_ig,
    iv = iv_ig,
    model_type = "PLATE",
    weights_var = "weights_w4",
    verbose = FALSE,
    return_model = TRUE
  )
  res_base_control_ig <- get_extended_df(res_base_control_ig_list)

  saveRDS(res_base_control_fb, "analysis/primary_alt/output/res_base_control_fb.rds")
  saveRDS(res_base_control_ig, "analysis/primary_alt/output/res_base_control_ig.rds")

  #############################################
  ### Primary Outcomes: Behaghel Robustness ###
  #############################################
  message("Running primary outcomes on Behaghel sample...")
  main_sample_behaghel_fb <- behaghel_balance_df(main_sample_fb)
  main_sample_behaghel_ig <- behaghel_balance_df(main_sample_ig)

  res_behaghel_fb <- lapply(
    primary_outcomes$all,
    run_outcome_wrapper,
    sample = main_sample_behaghel_fb %>% filter(behaghel_non_attriter == 1),
    selected_covars = selected_covars_fb,
    model_covars = model_covars_fb,
    iv = iv_fb,
    weights_var = "weights_w4",
    model_type = "PLATE",
    verbose = FALSE
  ) %>%
    bind_rows(.id = "dv") %>%
    mutate(
      dv_label = forcats::fct_relevel(dv_label, primary_outcomes$labels),
      version = "Behaghel sample"
    )

  res_behaghel_ig <- lapply(
    primary_outcomes$all,
    run_outcome_wrapper,
    sample = main_sample_behaghel_ig %>% filter(behaghel_non_attriter == 1),
    selected_covars = selected_covars_ig,
    model_covars = model_covars_ig,
    iv = iv_ig,
    weights_var = "weights_w4",
    model_type = "PLATE",
    verbose = FALSE
  ) %>%
    bind_rows(.id = "dv") %>%
    mutate(
      dv_label = forcats::fct_relevel(dv_label, primary_outcomes$labels),
      version = "Behaghel sample"
    )

  saveRDS(res_behaghel_fb, "analysis/primary_alt/output/res_behaghel_fb.rds")
  saveRDS(res_behaghel_ig, "analysis/primary_alt/output/res_behaghel_ig.rds")

  ############################################
  ## Primary Outcomes: Alternative weights ###
  ############################################
  message("Running primary outcomes with alternative weights...")
  res_win_fb <- lapply(
    alt_weights_dict_fb$all,
    iterate_weights_run_primary,
    outcomes = primary_outcomes,
    users = main_sample_fb,
    selected_covars = selected_covars_fb,
    model_covars = model_covars_fb,
    iv = iv_fb,
    model_type = "PLATE"
  ) %>% bind_rows()

  res_win_ig <- lapply(
    alt_weights_dict_ig$all,
    iterate_weights_run_primary,
    outcomes = primary_outcomes,
    users = main_sample_ig,
    selected_covars = selected_covars_ig,
    model_covars = model_covars_ig,
    iv = iv_ig,
    model_type = "PLATE"
  ) %>% bind_rows()

  ##########################################
  ## Primary Outcomes: LATE, no weights  ###
  ##########################################
  message("Running primary outcomes with LATE...")
  res_late_fb <- lapply(
    primary_outcomes$all,
    run_outcome_wrapper,
    sample = main_sample_fb,
    selected_covars = selected_covars_fb,
    model_covars = model_covars_fb,
    iv = iv_fb,
    model_type = "LATE",
    weights_var = NULL,
    verbose = FALSE
  ) %>%
    bind_rows(.id = "dv") %>%
    mutate(version = "Unweighted")

  res_late_ig <- lapply(
    primary_outcomes$all,
    run_outcome_wrapper,
    sample = main_sample_ig,
    selected_covars = selected_covars_ig,
    model_covars = model_covars_ig,
    iv = iv_ig,
    model_type = "LATE",
    weights_var = NULL,
    verbose = FALSE
  ) %>%
    bind_rows(.id = "dv") %>%
    mutate(version = "Unweighted")

  # Bind late to res_win and alt_primary_outcomes
  res_win_fb <- bind_rows(res_win_fb, res_late_fb)
  res_win_ig <- bind_rows(res_win_ig, res_late_ig)

  saveRDS(res_win_fb, "analysis/primary_alt/output/res_win_fb.rds")
  saveRDS(res_win_ig, "analysis/primary_alt/output/res_win_ig.rds")

  ############################################
  # Primary outcomes: Multiple accounts
  ############################################
  message("Running primary outcomes by number of accounts...")
  res_multiple_fb <- lapply(
    primary_outcomes$all,
    run_outcome_wrapper,
    sample = main_sample_fb %>% filter(single_fb_acct),
    selected_covars = selected_covars_fb,
    model_covars = model_covars_fb ,
    iv = iv_fb,
    weights_var = "weights_w4",
    model_type = "PLATE",
    verbose = FALSE
  ) %>% 
    bind_rows(.id = "dv")

  res_multiple_ig <- lapply(
    primary_outcomes$all,
    run_outcome_wrapper,
    sample = main_sample_ig %>% filter(single_ig_acct),
    selected_covars = selected_covars_ig,
    model_covars = model_covars_ig,
    iv = iv_ig,
    weights_var = "weights_w4",
    model_type = "PLATE",
    verbose = FALSE
  ) %>%
    bind_rows(.id = "dv")

  saveRDS(res_multiple_fb, "analysis/primary_alt/output/res_multiple_fb.rds")
  saveRDS(res_multiple_ig, "analysis/primary_alt/output/res_multiple_ig.rds")

  ############################################
  # combine results from day of response controls, no controls, and late
  alt_primary_outcomes_fb <- bind_rows(
    response_date_fb %>% filter(est == "PLATE") %>%
      mutate(version = "Control for endline\nresponse date"),
    res_late_fb,
    res_behaghel_fb
  )
  alt_primary_outcomes_ig <- bind_rows(
    response_date_ig %>% filter(est == "PLATE") %>%
      mutate(version = "Control for endline\nresponse date"),
    res_late_ig,
    res_behaghel_ig
  )

  saveRDS(alt_primary_outcomes_fb, "analysis/primary_alt/output/alt_primary_outcomes_fb.rds")
  saveRDS(alt_primary_outcomes_ig, "analysis/primary_alt/output/alt_primary_outcomes_ig.rds")
}

iterate_weights_run_primary <- function(
  weights,
  outcomes,
  users,
  selected_covars,
  model_covars,
  iv,
  model_type
) {
  #' Estimates treatment effects on primary outcomes for a given weight variable.
  #'
  #' @param weights A list containing the variable and label of the alternative weight.
  #' @param outcomes A list of primary outcomes and labels.
  #' @param users A data frame containing the main sample.
  #' @param selected_covars A list of selected covariates for each outcome.
  #' @param model_covars A vector of model block-id covariates.
  #' @param iv A string indicating the instrumental variable to use.
  #' @param model_type A string indicating the estimate to recover (e.g., 'PLATE').
  #'
  #' @return A dataframe containing effects for each primary outcome.
  #'
  weights_var <- weights[[1]]
  weights_label <- weights[[2]]
  res <- lapply(
    outcomes$all,
    run_outcome_wrapper,
    sample = users,
    selected_covars = selected_covars,
    model_covars = model_covars,
    iv = iv,
    weights_var = weights_var,
    model_type = model_type,
    verbose = FALSE
  ) %>%
    bind_rows(.id = "dv") %>%
    filter(est == "PLATE") %>%
    mutate(version = weights_label)
  return(res)
}

behaghel_balance_df <- function(df, svy_wave = 4) {
  #' Constructs a balanced sample in accord with Behaghel et al. (2015).
  #' Sorts non-attriting treatment individuals by completion time, selects them until
  #' treatment attrition rate matches control attrition rate. Remaining non-attriting
  #' treatment individuals are 'marginal attriters'.
  #'
  #' @param df A data frame containing the main sample.
  #' @param svy_wave An integer indicating the survey wave. Default is '4'.
  #'
  #' @return An updated main sample dataframe with indicator for marginal attriters.
  #'
  # variables for indicator for and time of survey completion
  completed_var <- paste0("completed_w", svy_wave)
  completion_time_var <- paste0("ENDDT_T_W", svy_wave)

  control_df <- df %>% filter(experiment_group == "control")
  treat_df <- df %>% filter(experiment_group == "test")

  # response rate in control
  control_resp_rate <- sum(control_df[[completed_var]]) / nrow(control_df)
  # number of non attriters in treat if it were balanced
  treat_nonattrit_n <- control_resp_rate * nrow(treat_df)

  # order treatment individuals who dont attrit by completion time
  treat_df_nonattrit <- treat_df %>%
    filter(get(completed_var) == 1) %>%
    arrange(get(completion_time_var))

  # build Behaghel sample
  treat_df_behaghel <- bind_rows(
    # non attriters in behaghel
    treat_df_nonattrit %>%
      head(treat_nonattrit_n) %>%
      mutate(behaghel_non_attriter = 1),
    # marginal attriters by behaghel
    treat_df_nonattrit %>%
      tail(nrow(treat_df_nonattrit) - treat_nonattrit_n) %>%
      mutate(behaghel_non_attriter = 0),
    # attriters who never responded
    treat_df %>%
      filter(get(completed_var) == 0) %>%
      mutate(behaghel_non_attriter = 0)
  )
  df_bal <- bind_rows(
    treat_df_behaghel,
    control_df %>% mutate(behaghel_non_attriter = get(completed_var))
  )
  return(df_bal)
}

## Execute
main()
