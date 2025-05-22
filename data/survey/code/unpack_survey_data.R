# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

#########################################
###   survey - unpack_survey_data.R   ###
#########################################

## Load required libraries
library(tidyr)
library(aws.s3)
library(lubridate)

devtools::load_all("~/alethea")

main <- function() {
  out_dir <- "data/survey/output/"
  dat <- load_raw_data()
  survey <- process_survey_data(dat$survey)
  survey_weights_fb <- construct_survey_weights_plat(
    dat$weights_fb, dat$weights_pop_fb, "FB"
  )
  survey_weights_ig <- construct_survey_weights_plat(
    dat$weights_ig, dat$weights_pop_ig, "IG"
  )
  survey_weights <- bind_rows(survey_weights_fb, survey_weights_ig) %>% select(-recruitment_platform_string)
  write_survey_objects(survey, survey_weights, out_dir)
  saveRDS(dat$attriters, file.path(out_dir, "attriters.rds"))
}


load_raw_data <- function(input_path = "data/survey/input/") {
  #' Loads all input raw data for initial survey data assembly.
  #'
  #' @param input_path A string representing the path to the input data.
  #'    Default is 'data/survey/input/'.
  #'
  #' @return A list of data frames containing the following:
  #'   - `survey`: Raw survey data frame.
  #'   - `weights_fb`: Raw survey weights (and alternative specifications) for Facebook.
  #'   - `weights_ig`: Raw survey weights (and alternative specifications) for Instagram.
  #'   - `weights_pop_fb`: Raw survey weights for full Facebook population.
  #'   - `weights_pop_ig`: Raw survey weights for full Instagram population.
  #'   - `attriters`: Raw survey data for users who attrited from survey.
  #'
  load_alethea <- function(fname, file_prefix = "") {
    alethea::read_file(
      paste0(file_prefix, fname),
      has_header = TRUE
    )
  }
  return(list(
    survey = read_file(
        filename='/data/us2020/emotional_state_replication/data/survey_emotional_state_all_waves.csv',
        has_header=TRUE
    ) %>% rename(participant_education = EDUC5),
    weights_fb = load_alethea("/data/us2020/deactivation_replication/data/survey_weights_fb_de_robustness_replication.csv"),
    weights_ig = load_alethea("/data/us2020/deactivation_replication/data/survey_weights_ig_de_robustness_replication.csv"),
    weights_pop_fb = load_alethea("/data/us2020/deactivation_replication/data/survey_weights_fb_de_full_sample_replication.csv"),
    weights_pop_ig = load_alethea("/data/us2020/deactivation_replication/data/survey_weights_ig_de_full_sample_replication.csv"),
    attriters = load_alethea(
      "/data/us2020/deactivation_replication/data/all_deactivation_users_demographics_before_attrition.csv",
      file_prefix = ""
    )
  ))
}

process_survey_data <- function(svy) {
  #' Process treatment group and response data for survey data, to be used in data/sample.
  #'
  #' @param svy A data frame containing raw survey data.
  #'
  #' @return An updated data frame with treatment group and response dates.
  #'
  svy <- svy %>%
    # 3 corresponds to deactivation_FB, 4 corresponds to deactivation_IG
    filter(P_SAMPLE_GROUP_W1 %in% c(3, 4)) %>%
    rename(participant_user_id = P_PSID) %>%
    mutate(
      recruitment_platform_string = case_when(
        P_SAMPLE_GROUP_W1 == 3 ~ "FB",
        P_SAMPLE_GROUP_W1 == 4 ~ "IG"
      ),
      experiment_group = case_when(
        !is.na(P_DEACT_CONTROL_W2) ~ "control",
        !is.na(P_DEACT_TEST_W2) ~ "test",
        TRUE ~ "unknown"
      )
    ) %>%
    filter(experiment_group %in% c("test", "control")) %>%
    mutate(
      # preserve date times
      ENDDT_T_W2 = mdy_hms(ENDDT_W2),
      ENDDT_T_W3 = mdy_hms(ENDDT_W3),
      ENDDT_T_W4 = mdy_hms(ENDDT_W4),
      ENDDT_T_W5 = mdy_hms(ENDDT_W5),
      # extract just dates 
      ENDDT_W2 = as.character(as.Date(ENDDT_W2, format = "%m/%d/%y")),
      ENDDT_W3 = as.character(as.Date(ENDDT_W3, format = "%m/%d/%y")),
      ENDDT_W4 = as.character(as.Date(ENDDT_W4, format = "%m/%d/%y")),
      ENDDT_W5 = as.character(as.Date(ENDDT_W5, format = "%m/%d/%y")),
    )
  return(svy)
}

construct_survey_weights_plat <- function(weights, weights_pop, plat = "FB") {
  #' Creates data frame with all survey weights for a given recruitment_platform_string.
  #' Calclulates weights for different specifications, joins together, and adds global population weights.
  #' Main weights used in analysis are `weights_w2` (baseline) and `weights_w4` (endline).
  #'
  #' @param weights A data frame containing different survey user-weight specifications.
  #' @param weights_pop A data frame containing global population weights.
  #'
  #' @return A data frame with all specifications of survey weights for a given recruitment_platform_string.
  #'
  #' @details See below for information on the different weight specifications for each recruitment_platform_string:
  #'    # | Module | Spec | Leave-out (Facebook)  | Leave-out (Instagram) | Diff |
  #' | -------------------------------------------------------------------- |
  #' |  Rake  |   1  | none                  | none                  |      |
  #' |  Rake  |   2  | Education             | Education             |      |
  #' |  Rake  |   3  | Party ID              | Party ID              |      |
  #' |  Rake  |   4  | Race                  | Race                  |      |
  #' |  IPSW  |   1  | none                  | none                  |      |
  #' |  IPSW  |   2  | L30 Bucket            | L30 Bucket            |      |
  #' |  IPSW  |   3  | Predicted Ideology    | Swing State           |   X  |
  #' |  IPSW  |   4  | Time Spent on FB      | Time Spent on IG      |      |
  #' |  IPSW  |   5  | Civic Pages Followed  | Number of followings  |   X  |
  #'
  weights <- weights %>% mutate(recruitment_platform_string = plat)
  if (plat == "IG") weights <- weights %>% rename(participant_user_id = P_PSID)
  weights_frame <- weights %>% 
    select(participant_user_id, recruitment_platform_string) %>%
    distinct() %>%
    join_survey_weight_spec(
      weights, waves = c(2), ipsw = 1, rake = 1, weight_label = "weights_w2"
    ) %>%
    join_survey_weight_spec(
      weights, waves = c(2), ipsw = 1, rake = 1, weight_label = "weights_w3"
    ) %>%
    join_survey_weight_spec(
      weights, waves = c(4), ipsw = 1, rake = 1, weight_label = "weights_w4"
    ) %>%
    join_survey_weight_spec(
      weights, waves = c(5), ipsw = 1, rake = 1, weight_label = "weights_w5"
    ) %>%
    join_survey_weight_spec(
      weights, waves = c(4), ipsw = 1, rake = 2, weight_label = "weight_alt_not_educ"
    ) %>%
    join_survey_weight_spec(
      weights, waves = c(4), ipsw = 1, rake = 3, weight_label = "weight_alt_not_party"
    ) %>%
    join_survey_weight_spec(
      weights, waves = c(4), ipsw = 1, rake = 4, weight_label = "weight_alt_not_race"
    ) %>%
    join_survey_weight_spec(
      weights, waves = c(4), ipsw = 2, rake = 1, weight_label = "weight_alt_not_l30"
    ) %>%
    join_survey_weight_spec(
      weights, waves = c(4), ipsw = 4, rake = 1, weight_label = "weight_alt_not_time"
    )
  if (plat == "FB") {
    weights_frame <- weights_frame %>%
      join_survey_weight_spec(
        weights, waves = c(4), ipsw = 3, rake = 1, weight_label = "weight_alt_not_ideo"
      ) %>%
      join_survey_weight_spec(
        weights, waves = c(4), ipsw = 5, rake = 1, weight_label = "weight_alt_not_civic"
      )
  } else if (plat == "IG") {
    weights_frame <- weights_frame %>%
      join_survey_weight_spec(
        weights, waves = c(4), ipsw = 3, rake = 1, weight_label = "weight_alt_not_swing"
      ) %>%
      join_survey_weight_spec(
        weights, waves = c(4), ipsw = 5, rake = 1, weight_label = "weight_alt_not_follow"
      )
  }
  weights_pop <- weights_pop %>%
    filter(wave == 2) %>%
    rename(participant_user_id = P_PSID, weights_full_pop = WEIGHT_MAIN)
  weights_frame <- weights_frame %>% 
    left_join(weights_pop %>% select(participant_user_id, weights_full_pop), by = c("participant_user_id"))
  return(weights_frame)
}

join_survey_weight_spec <- function(df, weights, waves, ipsw, rake, weight_label) {
  #' Calculate and join survey weights for a given specification.
  #' Computes average weights for a user based on waves, ipsw, and rake values.
  #'
  #' @param df A data frame containing finalized survey weights that this functions adds to.
  #' @param weights A data frame containing the raw survey weights for different specifications.
  #' @param waves A vector of integers representing the waves to include in the weight calculation.
  #' @param ipsw An integer representing the ipsw value to include in the weight calculation.
  #' @param rake An integer representing the rake value to include in the weight calculation.
  #' @param weight_label A string representing the column name for the new weight column.
  #'
  #' @return An updated dataframe `df` with the new weight column.
  #'
  # Get average value of weights of specified surveys, for a given ipsw and rake
  # value, and joins weight from specification to the survey dataset
  weight_spec <- weights %>%
    filter(ipsw_mod == ipsw & rake_mod == rake) %>%
    filter(wave %in% waves) %>%
    group_by(recruitment_platform_string, participant_user_id) %>%
    summarize(!!weight_label := mean(WEIGHT1, na.rm = TRUE), .groups = "drop") %>%
    select(all_of(c("participant_user_id", weight_label)))
  df <- df %>% left_join(weight_spec, by = c("participant_user_id"))
  df[[weight_label]] <- ifelse(is.na(df[[weight_label]]), 0, df[[weight_label]])
  return(df)
}

write_survey_objects <- function(survey, survey_weights, dir) {
  #' Save processed survey data and weights.
  #'
  #' @param survey A data frame containing processed survey data.
  #' @param survey_weights A data frame containing survey weight specifications.
  #' @param dir A string representing the directory to save the sample data.
  #'
  saveRDS(survey, file.path(dir, "survey.rds"))
  saveRDS(survey_weights, file.path(dir, paste0("survey_weights.rds")))
}


## Execute
main()

message("
 ##########################################
       Finished unpacking survey data
 ##########################################
")
