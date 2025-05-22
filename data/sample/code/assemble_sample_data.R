# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

###########################################
###   sample - assemble_sample_data.R   ###
###########################################

## Load required libraries
library(stringr)
library(tidyr)
library(dplyr)
library(tibble)
library(aws.s3)

devtools::load_all("~/alethea")

source("lib/constants.R")
source("lib/outcome_dictionaries.R")

source("data/sample/code/construct_frame_helpers.R")
source("data/sample/code/joint_wrangling_helpers.R")
source("data/sample/code/plat_wrangling_helpers.R")

# lasso covariates
lasso_covars_all <- c(
  "participant_male_or_not",
  "median_age_of_bracket",
  "participant_race_5_groups",
  "participant_political_ideology",
  "participant_party_identification",
  "participant_2016_voter_string",
  "turnout_likelihood_pre_election",
  "votechoice_pre_election",
  "tv_political_news_frequency_pre_election",
  "sum_cable_political_news_frequency_pre_election",
  "news_websites_political_news_frequency_pre_election",
  "sum_social_media_political_news_frequency_pre_election",
  "newspapers_political_news_frequency_pre_election",
  "self_reported_attention_to_politics",
  "political_knowledge_score_pre_election",
  "weighted_index_issue_polarization_pre_election",
  "weighted_index_political_participation_pre_election",
  "digital_literacy_score"
)

## Main
main <- function() {
  out_path <- "data/sample/output/"

  # load input data and covariates
  dat <- load_input_data()
  covars <- load_covars()

  # construct skeleton of ids, sample binaries, and weights
  message("Constructing weighted data ids...")
  weighted_data_ids <- cf$construct_weighted_data_ids_frame(
    survey = dat$survey,
    plat_fb = dat$plat_users_fb,
    plat_ig = dat$plat_users_ig,
    weights = dat$survey_weights
  )
  saveRDS(weighted_data_ids, file.path(out_path, "weighted_data_ids.rds"))

  # create sample data frames unique to user-day for each recruitment_platform_string
  message("Building sample_ds data frames...")
  sample_ds_fb <- build_sample_ds(dat$plat_users_ds_fb, weighted_data_ids, dat$survey)
  sample_ds_ig <- build_sample_ds(dat$plat_users_ds_ig, weighted_data_ids, dat$survey)
  saveRDS(sample_ds_fb %>% filter(main_sample), file.path(out_path, "main_sample_ds_fb.rds"))
  saveRDS(sample_ds_ig %>% filter(main_sample), file.path(out_path, "main_sample_ds_ig.rds"))

  # create joint sample data frame unique to user, joining w/ survey and auxiliary data
  message("Building joint sample data frame...")
  sample <- build_joint_sample(dat, weighted_data_ids)
  # transform survey data jointly (i.e., creating emotional state variables from raw responses)
  sample <- jw$transform_survey_data(sample)

  # create main recruitment_platform_string-specific samples
  message("Finalizing sample data...")
  sample_fb <- finalize_sample_platform(
    sample = sample,
    sample_ds = sample_ds_fb,
    covars = covars,
    plat = "FB"
  )
  write_sample_objects(sample_fb, dir = out_path, plat = "FB")

  sample_ig <- finalize_sample_platform(
    sample = sample,
    sample_ds = sample_ds_ig,
    covars = covars,
    plat = "IG"
  )
  write_sample_objects(sample_ig, dir = out_path, plat = "IG")
}

load_input_data <- function(input_path = "data/sample/input/") {
  #' Loads all input data for sample data assembly.
  #'
  #' @param input_path A string representing the path to the input data. 
  #'    Default is 'data/sample/input/'.
  #'
  #' @return A list of data frames containing the following:
  #'   - `plat_users_ds_fb`: Platform data for Facebook at the user-day level.
  #'   - `plat_users_ds_ig`: Platform data for Instagram at the user-day level.
  #'   - `plat_users_fb`: Platform data for Facebook at the user level.
  #'   - `plat_users_ig`: Platform data for Instagram at the user level.
  #'   - `survey`: Survey data.
  #'   - `survey_weights`: Survey weights.
  #'   - `state_id`: State-level variables (i.e., battleground state indicators).
  #'
  return(list(
    plat_users_ds_fb = readRDS(file.path(input_path, "plat_users_ds_fb.rds")),
    plat_users_ds_ig = readRDS(file.path(input_path, "plat_users_ds_ig.rds")),
    plat_users_fb    = readRDS(file.path(input_path, "plat_users_fb.rds")),
    plat_users_ig    = readRDS(file.path(input_path, "plat_users_ig.rds")),
    survey           = readRDS(file.path(input_path, "survey.rds")),
    survey_weights   = readRDS(file.path(input_path, "survey_weights.rds")),
    state_id         = cf$construct_aux_frame("/data/us2020/deactivation_replication/data/state_indicators_replication.csv")
  ))
}

load_covars <- function() {
  #' Loads all covariates for sample data assembly.
  #'
  #' @return A list of covariates for:
  #'   - `model`: Block covariates for randomization (for factorization).
  #'   - `response_date`: Day of user response covariates (for factorization).
  #'   - `lasso`: Covariates to be used in the LASSO selection.
  #'   - `impute`: Covariates to be imputed.
  #'
  model_covars <- c("BLOCK_ID")
  response_date_covars <- c("ENDDT_W4")
  emotion_pre_covars <- unlist(make_emotion_outcomes_baseline_dict(), use.names = FALSE)
  emotion_og_pre_covars <- unlist(
    make_emotion_outcomes_og_units_baseline_dict(), use.names = FALSE
  )
  substitution_pre_covars <- unlist(make_substitution_outcomes_baseline_dict(), use.names = FALSE)
  return(list(
    model = model_covars,
    response_date = response_date_covars,
    lasso = lasso_covars_all,
    impute = c(emotion_pre_covars, emotion_og_pre_covars, substitution_pre_covars)
  ))
}

build_sample_ds <- function(plat_users_ds, weighted_data_ids, survey) {
  #' Joins recruitment_platform_string-specific user-day data wiith weights, sample binaries, and treatment group.
  #' Calculates relative time spent variables and winsoizes them.
  #'
  #' @param plat_users_ds A data frame unique at the user-day level.
  #' @param weighted_data_ids A data frame containing user ids, sample binaries, and weights.
  #' @param survey A data frame containing survey data.
  #'
  #' @return A data frame unique at the user-day level with relative time spent variables.
  #'
  sample_ds <- plat_users_ds %>%
    left_join(weighted_data_ids, by = c("participant_user_id")) %>%
    left_join(survey %>% select(participant_user_id, experiment_group), by = c("participant_user_id")) %>%
    filter(uncut_sample) %>%
    mutate(ds = as.Date(strptime(ds, "%Y-%m-%d"))) %>%
    pw$run_winsorization_routine(c("fb_ts_base", "ig_ts_base", "vpvs_per_day"), pct = 0.99)
  # Calculate weighted mean of time spent on fb/ig in the baseline period (unit of obs = user-ds)
  baseline_wtd <- pw$calc_wtd_ts_user_ds_avg(sample_ds, exp_dates$base$start, exp_dates$base$end)
  sample_ds <- sample_ds %>%
    mutate(
      fb_ts_base_wtd = fb_ts_base / baseline_wtd[["fb_ts_base_wtd_avg"]],
      ig_ts_base_wtd = ig_ts_base / baseline_wtd[["ig_ts_base_wtd_avg"]]
    ) %>%
    pw$run_winsorization_routine(
      vars_to_winsorize = c("fb_ts_base_wtd", "ig_ts_base_wtd"), pct = 0.99
    )
  return(sample_ds)
}

build_joint_sample <- function(dat, weighted_data_ids) {
  #' Builds joint sample data frame unique at the user level.
  #' Uses `weighted_data_ids` as skeleton, left joins with survey, recruitment_platform_string, and auxiliary data.
  #'
  #' @param dat A list of data frames containing recruitment_platform_string, survey, and auxiliary data.
  #' @param weighted_data_ids A data frame containing user ids, sample binaries, and weights.
  #'
  #' @return A new data frame unique at the user level, combining survey, recruitment_platform_string, and auxiliary data.
  #'
  plat_users <- bind_rows(
    dat$plat_users_fb %>% mutate(recruitment_platform_string = "FB"),
    dat$plat_users_ig %>% mutate(recruitment_platform_string = "IG")
  )
  sample <- weighted_data_ids %>% 
    left_join(dat$survey, by = c("participant_user_id", "recruitment_platform_string")) %>%
    left_join(plat_users, by = c("participant_user_id", "recruitment_platform_string")) %>%
    # drop as state indicators has updated versions of these
    select(-c(IS_SWING_STATE, IS_SWING_CD)) %>%
    left_join(dat$state_id, by = c("participant_user_id")) %>%
    filter(uncut_sample)
  return(sample)
}

finalize_sample_platform <- function(sample, sample_ds, covars, plat = "FB") {
  #' Platform specific wrangling for sample data.
  #'   - Factorizes treatment, lasso, model, and response date covariates.
  #'   - Creates instrumental variable.
  #'   - Creates moderator variables for heterogeneity analysis.
  #'   - Imputes specified covariates.
  #'   - Builds relative time spent variables (for baseline, endline, and post-endline periods).
  #'
  #' @param sample A data frame of combined survey, recruitment_platform_string, and auxiliary data, unique at user level.
  #' @param sample_ds A data frame of recruitment_platform_string data unique at user-day level (from `build_sample_ds`).
  #' @param covars A list of covariates for model, response date, lasso, and imputation.
  #' @param plat A string representing the recruitment_platform_string to wrangle. Default is 'FB'.
  #'
  #' @return A list of objects containing the following:
  #'  - `sample`: Final recruitment_platform_string-specific sample data frame.
  #'  - `lasso_covars`: Factorized version of all covariates for LASSO selection.
  #'  - `model_covars`: Factorized version of block covariates.
  #'  - `response_date_covars`: Factorized version of response date covariates.
  #'
  sample_objects <- sample %>%
    filter(recruitment_platform_string == plat) %>%
    alethea::make_treated_bin(treatment_var = "experiment_group", trt = "test") %>%
    jw$run_factor_bin_routine(
      lasso_covars = covars$lasso,
      model_covars = covars$model,
      response_date_covars = covars$response_date
    )
  lasso_covars <- sample_objects$lasso_covars

  platl <- tolower(plat)
  covars_to_impute <- unique(c(lasso_covars, covars$impute))
  covars_to_impute <- covars_to_impute[covars_to_impute != ""]
  sample <- sample_objects$sample %>%
    pw$create_iv() %>%
    pw$run_winsorization_routine(
      vars_to_winsorize = c(paste0("participant_time_spent_relative_to_baseline_", platl)), pct = 0.99
    ) %>%
    mutate(
      weight_var_win95 = alethea::winsorize(weights_w4, main_sample, pct = .95),
      weight_var_win90 = alethea::winsorize(weights_w4, main_sample, pct = .90)
    ) %>%
    pw$build_moderator_plat_vars(
      baseline_ts_var = paste0("participant_time_spent_relative_to_baseline_", platl, "_win"), plat = plat
    ) %>%
    pw$run_imputation_routine(covars_to_impute = covars_to_impute) %>%
    pw$build_wtd_relative_time_spent_vars(sample_ds)
  return(list(
    sample = sample,
    lasso_covars = lasso_covars,
    model_covars = sample_objects$model_covars,
    response_date_covars = sample_objects$response_date_covars
  ))
}

write_sample_objects <- function(sample_objects, dir, plat = "FB") {
  #' Saves sample data and covariates.
  #'
  #' @param sample_objects A list of objects containing returned values from `finalize_sample_platform`.
  #' @param dir A string representing the directory to save the sample data.
  #' @param plat A string representing the recruitment_platform_string of data. Default is 'FB'.
  #'
  platl <- paste0("_", tolower(plat))
  saveRDS(sample_objects$model_covars, file.path(dir, paste0("model_covars", platl, ".rds")))
  saveRDS(
    sample_objects$response_date_covars,
    file.path(dir, paste0("response_date_covars", platl, ".rds"))
  )
  saveRDS(sample_objects$lasso_covars, file.path(dir, paste0("lasso_covars", platl, ".rds")))

  saveRDS(sample_objects$sample, file.path(dir, paste0("uncut_sample", platl, ".rds")))
  saveRDS(
    sample_objects$sample %>% filter(main_sample),
    file.path(dir, paste0("main_sample", platl, ".rds"))
  )
}

## Execute
main()

message("
 ##########################################
      Finished assembling sample data
 ##########################################
")
