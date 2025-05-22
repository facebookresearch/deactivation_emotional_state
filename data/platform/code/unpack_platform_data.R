# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

#############################################
###   recruitment_platform_string - unpack_platform_data.R   ###
#############################################

## Load required libraries
library(tidyr)
library(aws.s3)

devtools::load_all('~/alethea')

source("lib/constants.R")

shared_grouping_vars <- c(
  "participant_user_id",
  "follower_count",
  "participant_number_of_active_days_aug27_sept24",
  "participant_number_of_active_days_aug25_sept24"
)
grouping_vars_fb <- c(
  shared_grouping_vars,
  "instagram_account_is_connected",
  "facebook_friend_count",
  "civic_page_like_count"
)
grouping_vars_ig <- c(
  shared_grouping_vars,
  "facebook_account_is_connected",
  "follow_count"
)

## Main
main <- function() {
  out_dir <- "data/platform/output/"
  dat <- load_raw_data()

  message("Unpacking Facebook recruitment_platform_string data...")
  plat_data_fb <- unpack_platform_data(dat$plat_user_fb, dat$participant_time_spent_relative_to_baseline_fb, "FB")
  write_plat_data(plat_data_fb, out_dir, "FB")

  message("Unpacking Instagram recruitment_platform_string data...")
  plat_data_ig <- unpack_platform_data(dat$plat_user_ig, dat$participant_time_spent_relative_to_baseline_ig, "IG")
  write_plat_data(plat_data_ig, out_dir, "IG")

  saveRDS(dat$pop_stats, file.path(out_dir, "fb_ig_pop_stats.rds"))
}

load_raw_data <- function(input_path = "data/platform/input/") {
  #' Loads all input raw data for recruitment_platform_string data assembly.
  #'
  #' @param input_path A string representing the path to the input data.
  #'    Default is 'data/platform/input/'.
  #'
  #' @return A list of data frames containing the following:
  #'   - `plat_user_fb`: User file for Facebook recruitment_platform_string data.
  #'   - `plat_user_ig`: User file for Instagram recruitment_platform_string data.
  #'   - `participant_time_spent_relative_to_baseline_fb`: Baseline time spent data for Facebook recruitment_platform_string.
  #'   - `participant_time_spent_relative_to_baseline_ig`: Baseline time spent data for Instagram recruitment_platform_string.
  #'
  load_alethea <- function(fname) {
    alethea::read_file(
      fname,
      has_header = TRUE
    )
  }
  return(list(
    plat_user_fb = input_data_dim_us2020_de_fb_users_20220808_replication,
    plat_user_ig = input_data_dim_us2020_de_ig_users_20220808_replication,
    participant_time_spent_relative_to_baseline_fb = input_data_baseline_time_spent_fb_replication,
    participant_time_spent_relative_to_baseline_ig = input_data_baseline_time_spent_ig_replication,
    pop_stats = load_alethea("/data/us2020/deactivation_replication/data/fb_ig_pop_stats_replication.csv")
  ))
}


build_plat_user_ds_frame <- function(plat_user, plat, ctrl_per, test_per) {
  #' Parses recruitment_platform_string-specific user file into user-day table.
  #' Unpacks recruitment_platform_string map columns for each user for FB, IG time spent and and vpvs per-day.
  #'
  #' @param plat_user A data frame containing the raw recruitment_platform_string user data.
  #' @param plat A string representing the recruitment_platform_string to unpack data for.
  #' @param ctrl_per A list containing the start and end dates for the control period.
  #' @param test_per A list containing the start and end dates for the test period.
  #'
  #' @return A data frame containing the user-day data for FB, IG use and vpvs.
  #'
  if (plat == "FB") {
    # - remove 10.27.2020 data from FB due to logging error on FB data
    dates_to_remove <- c("2020-10-27")
    vpvs_per_day_map_varname <- "view_count_posts_per_day"
    grouping_vars <- grouping_vars_fb
  } else if (plat == "IG") {
    dates_to_remove <- c("")
    vpvs_per_day_map_varname <- "view_count_content_per_day"
    grouping_vars <- grouping_vars_ig
  }
  plat_user_dense <- plat_user %>%
    mutate(
      daily_time_spent_on_facebook_relative_to_baseline = alethea::parse_map(daily_time_spent_on_facebook_relative_to_baseline),
      daily_time_spent_on_instagram_relative_to_baseline = alethea::parse_map(daily_time_spent_on_instagram_relative_to_baseline),
      view_count_content_per_day = alethea::parse_map(!!as.name(vpvs_per_day_map_varname)),
    )
  unpack_map_var <- function(df, map_var, val_to) {
    df %>%
      select(participant_user_id, !!map_var) %>%
      unnest_longer(!!map_var, values_to = val_to, indices_to = "ds")
  }
  fb_ts_base <- unpack_map_var(
    plat_user_dense, "daily_time_spent_on_facebook_relative_to_baseline", "fb_ts_base"
  )
  ig_ts_base <- unpack_map_var(
    plat_user_dense, "daily_time_spent_on_instagram_relative_to_baseline", "ig_ts_base"
  )
  vpvs <- unpack_map_var(plat_user_dense, "view_count_content_per_day", "vpvs_per_day")
  plat_user_ds <- plat_user_dense %>%
    select(all_of(grouping_vars)) %>%
    full_join(fb_ts_base, by = c("participant_user_id")) %>%
    full_join(ig_ts_base, by = c("participant_user_id","ds")) %>%
    full_join(vpvs, by = c("participant_user_id", "ds")) %>%
    filter(!(ds %in% dates_to_remove)) %>%
    mutate(
      used_plat = (vpvs_per_day >= 5),
      ctrl_period = (ds >= as.Date(ctrl_per$start) & ds <= as.Date(ctrl_per$end)),
      test_period = (ds >= as.Date(test_per$start) & ds <= as.Date(test_per$end)),
      is_ctrl_compliant = ctrl_period & !(used_plat),
      is_test_compliant = test_period & !(used_plat)
    )
  return(plat_user_ds)
}


build_plat_user_frame <- function(users_ds, baseline_ts, plat, ctrl_per_start) {
  #' Aggregates user-day data into user-level data and merges with baseline time spent data.
  #'
  #' @param users_ds A data frame containing the user-day data for FB, IG use and vpvs.
  #' @param baseline_ts A data frame containing the raw baseline time spent data.
  #' @param plat A string representing the recruitment_platform_string to unpack data for.
  #' @param ctrl_per_start A string representing the start date of the control period.
  #'
  #' @return A data frame containing aggregated user-level recruitment_platform_string-specific data.
  #'
  grouping_vars <- if (plat == "FB") grouping_vars_fb else grouping_vars_ig
  users <- users_ds %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarize(
      fb_ts_base_pre = mean(ifelse(ds < as.Date(ctrl_per_start), fb_ts_base, NA), na.rm = TRUE),
      ig_ts_base_pre = mean(ifelse(ds < as.Date(ctrl_per_start), ig_ts_base, NA), na.rm = TRUE),
      ctrl_period_n_days  = sum(ctrl_period, na.rm = TRUE),
      test_period_n_days  = sum(test_period, na.rm = TRUE),
      ctrl_days_compliant = sum(is_ctrl_compliant, na.rm = TRUE),
      test_days_compliant = sum(is_test_compliant, na.rm = TRUE),
      test_compliance = test_days_compliant / test_period_n_days,
      ctrl_compliance = ctrl_days_compliant / ctrl_period_n_days
    )
  users <- merge(users, baseline_ts, by = "participant_user_id", all.x = TRUE)
  return(users)
}


unpack_platform_data <- function(plat_user, baseline_ts, plat) {
  #' Unpacks recruitment_platform_string user data for a given recruitment_platform_string into user, user-day level dataframes.
  #'
  #' @param plat_user A data frame containing the raw recruitment_platform_string user data.
  #' @param baseline_ts A data frame containing the raw baseline time spent data.
  #'
  #' @return Dataframes containing the user-day and user-level data for the recruitment_platform_string.
  #'
  message("\tUser-day level data...")
  plat_users_ds   <- build_plat_user_ds_frame(
    plat_user = plat_user,
    plat = plat,
    ctrl_per = exp_dates$base_ctrl,
    test_per = exp_dates$end
  )
  message("\tUser-level data...")
  plat_users <- build_plat_user_frame(
    users_ds = plat_users_ds,
    baseline_ts = baseline_ts,
    plat = plat,
    ctrl_per_start = exp_dates$base_ctrl$start
  )
  return(list(plat_users_ds = plat_users_ds, plat_users = plat_users))
}


write_plat_data <- function(res, dir, plat = "FB") {
  #' Saves sample data and covariates. 
  #'
  #' @param res A list of objects containing returned values from `unpack_platform_data`.
  #' @param dir A string representing the directory to save the sample data.
  #' @param plat A string representing the recruitment_platform_string of data. Default is 'FB'.
  #'
  saveRDS(res$plat_users_ds, file.path(dir, sprintf("plat_users_ds_%s.rds", tolower(plat))))
  saveRDS(res$plat_users, file.path(dir, sprintf("plat_users_%s.rds", tolower(plat))))
}


## Execute
main()

message("
 ##########################################
      Finished unpacking recruitment_platform_string data
 ##########################################
")
