# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

#############################################################
###   substitution - compute_app_substitution_effects.R   ###
#############################################################

library(tidyr)
library(forcats)

source("lib/compute_effects.R")
source("lib/constants.R")
source("lib/outcome_dictionaries.R")
# NOTE: alethea, dplyr loaded in source

custom_app_dict <- make_substitution_custom_apps_dict()

main <- function() {
  main_sample_fb <- readRDS("analysis/substitution/input/main_sample_fb.rds")
  main_sample_ig <- readRDS("analysis/substitution/input/main_sample_ig.rds")

  lasso_covars_fb <- readRDS("analysis/substitution/input/lasso_covars_fb.rds")
  lasso_covars_ig <- readRDS("analysis/substitution/input/lasso_covars_ig.rds")

  model_covars_fb <- readRDS("analysis/substitution/input/model_covars_fb.rds")
  model_covars_ig <- readRDS("analysis/substitution/input/model_covars_ig.rds")

  raw_app_data <- readRDS("analysis/substitution/input/passive_tracking_users.rds")

  enter_exit <- raw_app_data %>% group_by(participant_user_id) %>% summarise(enter = min(date), exit = max(date))

  res_sub_fb <- substitution_app_effects(
    app_data = raw_app_data,
    enter_exit_ad = enter_exit,
    sample = main_sample_fb,
    lasso_covars = lasso_covars_fb,
    model_covars = model_covars_fb,
    iv = iv_fb,
    plat = "FB"
  )
  res_sub_ig <- substitution_app_effects(
    app_data = raw_app_data,
    enter_exit_ad = enter_exit,
    sample = main_sample_ig,
    lasso_covars = lasso_covars_ig,
    model_covars = model_covars_ig,
    iv = iv_ig,
    plat = "IG"
  )

  saveRDS(res_sub_fb, "analysis/substitution/output/res_sub_fb.rds")
  saveRDS(res_sub_ig, "analysis/substitution/output/res_sub_ig.rds")
}

substitution_app_effects <- function(
  app_data,
  enter_exit_ad,
  sample,
  lasso_covars,
  model_covars,
  iv,
  plat = "FB"
) {
  #' Calculates substitution effects of treatment for a specificied set of apps, and categories of apps.
  #' Constructs sample with balanced panel pre and post-intervention, i.e., including individuals who:
  #'      - at least two days of baseline data before deactivation began on 9/23
  #'      - non-missing data on >= 85% of days in baseline, post-intervention period
  #'
  #' @param app_data A dataframe containing raw app usage data from passive tracking.
  #' @param enter_exit_ad A dataframe containing the enter and exit dates for each user into app panel.
  #' @param sample A dataframe containing the main sample.
  #' @param lasso_covars A vector of covariates to be used in the lasso.
  #' @param model_covars A vector of model block-id covariates.
  #' @param iv A string indicating the instrumental variable to use.
  #' @param plat A string indicating the recruitment_platform_string ('FB' or 'IG'). Default is 'FB'.
  #'
  #' @return A dataframe containing substitution effects for each app/app category.
  #'
  # 1. Sample (i): Post-Intervention Balanced Sample
  sample_i <- create_post_intervention_sample(
    app_data = app_data,
    enter_exit_ad = enter_exit_ad,
    sample = sample,
    balance = 0.85    ## 0.85 corresponds roughly to 6/7 (i.e., 6 days a week), so >= 30 days
  )
  # 2. Sample (ii): Balanced panel with baseline data
  sample_ii_pre <- create_pre_intervention_sample(
    app_data = app_data,
    enter_exit_ad = enter_exit_ad,
    sample_i = sample_i,
    sample = sample,
    balance_rel = 0.85,   ## 0.85 corresponds roughly to 6/7 (i.e., 6 days a week)
    balance_abs = 0.17    ## 2 day restriction (11 day period, so 0.17 * 11 ~= 1.9)
  )
  sample_ii <- sample_i %>% filter(participant_user_id %in% sample_ii_pre$participant_user_id)
  sample_ii_effects <- estimate_app_substitution_effects(
    app_sample = sample_ii,
    sample = sample,
    lasso_covars = lasso_covars,
    model_covars = model_covars,
    iv = iv,
    plat = plat,
    include_pre_vars = TRUE,
    app_sample_pre = sample_ii_pre
  )
  return(sample_ii_effects)
}

create_post_intervention_sample <- function(
  app_data,
  enter_exit_ad,
  sample,
  start_date = "2020-09-30",
  end_date = "2020-11-03",
  balance = 1,
  agg = TRUE
) {
  #' Creates post-intervention balanced sample according to balance parameter.
  #'
  #' @param app_data A dataframe containing raw app usage data from passive tracking.
  #' @param enter_exit_ad A dataframe containing the enter and exit dates for each user into app panel.
  #' @param sample A dataframe containing the main sample.
  #' @param start_date A string indicating the start date of the post-intervention period. Default is '2020-09-30'.
  #' @param end_date A string indicating the end date of the post-intervention period. Default is '2020-11-03'.
  #' @param balance A numeric indicating the balance threshold. Default is 1.
  #' @param agg A boolean indicating whether to aggregate app data. Default is TRUE.
  #'
  #' @return A dataframe containing the post-intervention balanced sample with aggregated app time spent data.
  #'
  # Sample restrictions
  # 1. restrict to users who enter before start, exit after end
  enter_exit_ad <- enter_exit_ad %>% filter(enter <= start_date, exit >= end_date)
  app_data_filt <- app_data %>% filter(participant_user_id %in% enter_exit_ad$participant_user_id)
  # 2. restrict to observations in period
  app_data_filt <- app_data_filt %>% filter(date >= start_date, date <= end_date)
  # 3. restrict to observations in recruitment_platform_string sample
  app_data_filt <- app_data_filt %>%
    left_join(sample %>% select(participant_user_id, experiment_group)) %>%
    filter(!is.na(experiment_group))
  # 4. restrict to observations matching balance restrictions
  if (balance > 0) {
    bal_tpids <- app_data_filt %>%
      group_by(participant_user_id) %>%
      summarise(n_days = n_distinct(date)) %>%
      # needs to be present in as many days as balance * (n days between start and end date)
      filter(n_days >= (balance * (1 + (as.Date(end_date) - as.Date(start_date)))))
    app_data_filt <- app_data_filt %>% filter(participant_user_id %in% bal_tpids$participant_user_id)
  }
  # Ensure data quality (filter out negative time spent and winsorize)
  app_data_filt <- clean_app_data(app_data_filt)
  # Create custom app categories
  app_data_filt <- create_custom_app_categories(app_data_filt)
  # Aggregate app data
  if (agg) {
    ad_app_agg <- aggregate_app_data(app_data_filt)
    return(ad_app_agg)
  }
  return(app_data_filt)
}

create_pre_intervention_sample <- function(
  app_data,
  enter_exit_ad,
  sample_i,
  sample,
  start_date = "2020-09-12",
  end_date = "2020-09-22",
  balance_rel = 1,
  balance_abs = 0.2   # balance_abs controls balance relative to overall period
) {
  #' Creates pre-intervention balanced sample according to both relative and absolute balance parameters.
  #'
  #' @param app_data A dataframe containing raw app usage data from passive tracking.
  #' @param enter_exit_ad A dataframe containing the enter and exit dates for each user into app panel.
  #' @param sample_i A dataframe containing the main sample, balanced at post-intervention.
  #' @param start_date A string indicating the start date of the pre-intervention period.
  #'      Default is '2020-09-12'.
  #' @param end_date A string indicating the end date of the pre-intervention period.
  #'      Default is '2020-09-22'.
  #' @param balance_rel A numeric indicating the balance threshold relative to individual's enter date.
  #'      Default is 1.
  #' @param balance_abs A numeric indicating the balance threshold relative to overall period.
  #'      Default is 0.2.
  #'
  #' @return A dataframe containing the pre-intervention balanced sample with aggregated app baseline time spent data.
  #'
  # Sample restrictions
  # 1. restrict to observations in period
  app_data_filt <- app_data %>% filter(date >= start_date, date <= end_date)
  # 2. restrict to observations in recruitment_platform_string sample
  app_data_filt <- app_data_filt %>%
    left_join(sample %>% select(participant_user_id, experiment_group)) %>%
    filter(!is.na(experiment_group))
  # 3. restrict according to balance thresholds
  if (balance_rel > 0) {
    bal_tpids <- app_data_filt %>%
      group_by(participant_user_id) %>%
      summarise(n_days = n_distinct(date)) %>%
      left_join(enter_exit_ad, c("participant_user_id")) %>%
      filter(n_days >= (balance_rel * (1 + (as.Date(end_date) - as.Date(enter)))))
    app_data_filt <- app_data_filt %>% filter(participant_user_id %in% bal_tpids$participant_user_id)
  }
  if (balance_abs > 0) {
    bal_tpids <- app_data_filt %>%
      group_by(participant_user_id) %>%
      summarise(n_days = n_distinct(date)) %>% 
      filter(n_days >= (balance_abs * (1 + (as.Date(end_date) - as.Date(start_date)))))
    app_data_filt <- app_data_filt %>% filter(participant_user_id %in% bal_tpids$participant_user_id)
  }
  # 4. restrict to only individuals in sample i
  app_data_filt <- app_data_filt %>% filter(participant_user_id %in% sample_i$participant_user_id)
  # Ensure data quality (filter out negative time spent and winsorize)
  app_data_filt <- clean_app_data(app_data_filt)
  # Create custom app categories
  app_data_filt <- create_custom_app_categories(app_data_filt)
  # Aggregate app data
  ad_app_agg <- aggregate_app_data(app_data_filt)
  return(ad_app_agg)
}

aggregate_app_data <- function(app_data) {
  #' Aggregates app data to calculate average time spent per day for each app.
  #'
  #' @param app_data A dataframe containing raw app usage data from passive tracking.
  #'
  #' @return A aggregated dataframe with time spent (mins) per day for each app.
  #'
  # calculate number of days in period
  n_days <- 1 + as.integer(max(app_data$date) - min(app_data$date))
  # get average number of minutes spent per day for each app
  ad_app_agg <- app_data %>%
    group_by(participant_user_id, experiment_group, app_name) %>%
    summarise(time_spent = sum(time_spent)) %>%
    # time spent is in total seconds over the whole period,
    # thus, we divide by the total number of days * 60 to get time spent in minutes
    mutate(time_spent_min_per_day = time_spent / (n_days * 60)) %>%
    ungroup()
  return(ad_app_agg)
}

create_custom_app_categories <- function(app_data) {
  #' Categories apps into custom categories for substitution analysis.
  #'
  #' @param app_data A dataframe containing raw app usage data from passive tracking.
  #'
  #' @return A dataframe with updated app names reflecting desired custom categories.
  #'
  individual_apps <- c(
    "Facebook", "Instagram", "Twitter",
    "Snapchat", "TikTok",  "YouTube", "Facebook Messenger"
  )
  app_data <- app_data %>%
    mutate(
      app_name = case_when(
        app_name %in% individual_apps ~ app_name,
        app_category == "Search" ~ "Browsers",
        app_category == "Social Networking" ~ "Other social media",
        app_category == "Media & Video" ~ "Other media & video",
        app_category == "Music & Audio" ~ "Music & audio",
        app_category == "Communication" ~ "Other messenger & mail",
        .default = "Other"
      ),
      app_name = fct_relevel(app_name, unlist(custom_app_dict, use.names = FALSE))
    )
  return(app_data)
}

clean_app_data <- function(app_data, winsor_probs = 0.99) {
  #' Helper function for cleaning raw app data.
  #' Filters out negative time spent and winsorizes.
  #'
  #' @param app_data A dataframe containing raw app usage data from passive tracking.
  #' @param winsor_probs A numeric indicating the quantile threshold for winsorization. 
  #'      Default is 0.99.
  #'
  #' @return An updated version of app_data.
  #'
  # calculate winsorization threshold
  thr <- quantile(app_data$time_spent, probs = winsor_probs)
  app_data <- app_data %>%
    # filter out any observations with negative time spent
    filter(time_spent >= 0) %>%
    mutate(time_spent = ifelse(time_spent > thr, thr, time_spent))
  return(app_data)
}

estimate_app_substitution_effects <- function(
  app_sample,
  sample,
  lasso_covars,
  model_covars,
  iv,
  plat = "FB",
  include_pre_vars = FALSE,
  app_sample_pre = NULL
) {
  #' Estimates substitution effects of treatment for a specified set of apps/app categories.
  #' Also writes control group averages to disk.
  #'
  #' @param app_sample A dataframe containing users in desired sample with app time spent variables. 
  #' @param sample A dataframe containing the main sample.
  #' @param lasso_covars A vector of covariates to be used for the lasso-selection step.
  #' @param model_covars A vector of model block-id covariates.
  #' @param iv A string indicating the instrumental variable to use.
  #' @param plat A string indicating the recruitment_platform_string ('FB' or 'IG'). Default is 'FB'.
  #' @param include_pre_vars A boolean indicating whether to include pre-intervention app time spent variables. 
  #'      Default is FALSE
  #' @param app_sample_pre A dataframe containing users in desired sample with baseline app time spent variables.
  #'
  #' @return A dataframe containing substitution effects for each app/app category.
  #'
  ad_app_agg_wide <- reshape_app_usage_wide(app_sample)
  # merge with main sample (inner join)
  sample <- sample %>% merge(ad_app_agg_wide, by = c("participant_user_id"))
  # create variable lists
  app_vars <- c("time_spent_all_apps", names(custom_app_dict))
  app_names <-  c("All apps", unlist(custom_app_dict, use.names = FALSE))
  # extract control group average time spent for each category
  write_control_group_app_avg(sample, app_vars, plat)
  # if true, calcluate time spent variables for pre-intervention period and join on
  if (include_pre_vars){
    apps_pre <- reshape_app_usage_wide(app_sample_pre, app_names, suffix = "_pre")
    sample <- sample %>% left_join(apps_pre, c("participant_user_id"))
    app_vars_pre <- setNames(paste0(app_vars, "_pre"), app_vars)
  } else {
    app_vars_pre <- setNames(rep("", length(app_vars)), app_vars)
  }
  # estimate time spent LATE
  res  <- run_all_substitution(
    sample = sample,
    dv_varnames = app_vars,
    dv_labels = app_names,
    dv_pre_list = app_vars_pre,
    lasso_covars = lasso_covars,
    model_covars = model_covars,
    iv = iv,
    weights_var = NULL,
    model_type = "LATE"
  )
  return(res)
}

write_control_group_app_avg <- function(df, vars, plat = "FB") {
  #' Helper function to save control group averages to disk.
  #'
  #' @param df A dataframe containing the main sample with app time spent variables. 
  #' @param vars A list of time spent variables to calculate average for.
  #' @param plat A string indicating the recruitment_platform_string ('FB' or 'IG'). Default is 'FB'.
  #'
  control_avg <- df %>%
    filter(experiment_group == "control") %>%
    summarise(across(all_of(vars), ~mean(., na.rm = TRUE)))
  saveRDS(
    control_avg,
    paste0("analysis/substitution/output/", tolower(plat), "_control_app_average.rds")
  )
}

calculate_pre_vars <- function(users, ad_app_agg_pre, app_names) {
  #' Calculates baseline time spent variables for apps/app categories for main sample.
  #'
  #' @param users A dataframe containing the main sample with app time spent variables.
  #' @param ad_app_agg_pre A dataframe containing users in desired sample with baseline app time spent variables.
  #' @param app_names A character vector of labels for apps/app categories.
  #'
  #' @return A dataframe with aggregated baseline time spent variables for each app/app category.
  # calculate total app usage by summing over all time spent (categories are exhaustive)
  total_app_usage_pre <- ad_app_agg_pre %>%
    group_by(participant_user_id) %>%
    summarise(time_spent_all_apps_pre = sum(time_spent_min_per_day))
  # convert to wide dataframe
  ad_app_agg_pre_wide <- ad_app_agg_pre %>%
    filter(app_name %in% app_names) %>%
    mutate( # reformat app names to make variable names 
      app_name_col = str_replace_all(tolower(app_name), " ", ""),
      app_name_col = str_replace_all(app_name_col, "/", ""),
      app_name_col = str_replace_all(app_name_col, "&", "_"),
    ) %>%
    select(participant_user_id, app_name_col, time_spent_min_per_day) %>%
    pivot_wider(
      names_from = app_name_col,
      values_from = time_spent_min_per_day,
      names_glue = "time_spent_{app_name_col}_pre"
    ) %>%
    ungroup() %>%
    merge(total_app_usage_pre, by = c("participant_user_id"), all = TRUE)
  ad_app_agg_pre_wide[is.na(ad_app_agg_pre_wide)] <- 0
  return(ad_app_agg_pre_wide)
}

reshape_app_usage_wide <- function(df, app_names = NULL, suffix = "") {
  #' Reshapes app usage data to wide format.
  #'
  #' @param df A dataframe containing app usage data.
  #' @param app_names A character vector of labels for apps/app categories. Default is NULL.
  #' @param suffix A string to append to the column names. Default is an empty string.
  #'
  #' @return A dataframe with reshaped app usage data in wide format.
  #'
  if (!is.null(app_names)) df <- df %>% filter(app_name %in% app_names)
  # calculate total app usage by summing over all time spent (categories are exhaustive)
  total_usage_col <- paste0("time_spent_all_apps", suffix)
  total_app_usage <- df %>%
    group_by(participant_user_id) %>%
    summarise(!!total_usage_col := sum(time_spent_min_per_day), .groups = "drop")
  # define helper function clean app_name to make variable names
  clean_app_name <- function(x) {
    x <- str_replace_all(tolower(x), " ", "")
    x <- str_replace_all(x, "/", "")
    x <- str_replace_all(x, "&", "_")
    return(x)
  }
  df_wide <- df %>%
    mutate(app_name_clean = clean_app_name(app_name)) %>%
    select(participant_user_id, app_name_clean, time_spent_min_per_day) %>%
    pivot_wider(
      names_from = app_name_clean,
      values_from = time_spent_min_per_day,
      names_glue = paste0("time_spent_{app_name_clean}", suffix)
    ) %>%
    ungroup()
  df_wide[is.na(df_wide)] <- 0
  out <- df_wide %>% left_join(total_app_usage, by = c("participant_user_id"))
  return(out)
}

run_all_substitution <- function(
  sample,
  dv_varnames,
  dv_labels,
  dv_pre_list,
  lasso_covars,
  model_covars,
  iv,
  weights_var,
  model_type,
  verbose = FALSE
) {
  #' Estimates substitution treatment effects on time spent (mins) per day.
  #'
  #' @param sample A dataframe containing the main sample with app time spent variables.
  #' @param dv_varnames A character vector of time spent variables to calculate average for.
  #' @param dv_labels A character vector of time spent variable labels.
  #' @param dv_pre_list A character vector of baseline time spent variables to include.
  #' @param lasso_covars A vector of covariates to be used for the lasso-selection step.
  #' @param model_covars A vector of model block-id covariates.
  #' @param iv A string indicating the instrumental variable to use.
  #' @param weights_var A string indicating the weights variable to use.
  #' @param model_type A string indicating the estimate to recover (e.g., 'LATE').
  #' @param verbose A boolean indicating whether to print progress messages. Default is FALSE.
  #'
  #' @param return A list of two objects:
  #'  - `res`: A dataframe with the results of the substitution regressions.
  #' -  `selected_covars`: A list of the covariates selected in the lasso.
  #'
  dv_map <- mapply(list, dv_varnames, dv_labels, SIMPLIFY = FALSE)
  selected_covars <- unlist(
    lapply(
      dv_varnames,
      select_covars_lasso,
      df = sample,
      dv_pre_list = dv_pre_list,
      weights_var = weights_var,
      lasso_covars = lasso_covars
    ),
    recursive = FALSE
  )
  res <- bind_rows(
    lapply(
      dv_map,
      run_outcome_wrapper,
      sample = sample,
      selected_covars = selected_covars,
      model_covars = model_covars,
      iv = iv,
      weights_var = weights_var,
      model_type = model_type,
      verbose = verbose
    )
  )
  return(list(res = res, lasso = selected_covars))
}


## Execute
main()
