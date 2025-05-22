# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

#############################################
###   sample - plat_wrangling_helpers.R   ###
#############################################

pw <- list()
pwh <- list()

pw$calc_wtd_ts_user_ds_avg <- function(sample_ds, start_date, end_date) {
  #' Calculates weighted average of time spent (over user-days) for a given time period.
  #'
  #' @param sample A data frame unique at the user-day level.
  #' @param start_date A string representing the start date of the time period.
  #' @param end_date A string representing the end date of the time period.
  #'
  #' @return A data frame with the following columns:
  #'  - `fb_ts_base_wtd_avg`: Weighted average of time spent on Facebook.
  #'  - `ig_ts_base_wtd_avg`: Weighted average of time spent on Instagram.
  #'
  res <- sample_ds %>%
    filter(ds >= as.Date(start_date) & ds <= as.Date(end_date)) %>%
    filter(main_sample) %>%
    summarise(
      fb_ts_base_wtd_avg = weighted.mean(fb_ts_base, weights_w2, na.rm = TRUE),
      ig_ts_base_wtd_avg = weighted.mean(ig_ts_base, weights_w2, na.rm = TRUE)
    )
  return(res)
}

pw$create_iv <- function(sample){
  #' Creates the instrumental variable for the main sample.
  #'
  #' @param sample A data frame unique at the user level.
  #'
  #' @return An updated `sample` dataframe with a column `D_i` for the IV.
  #'
  main_sample_control <- sample %>% filter(experiment_group == "control" & main_sample)
  Vbar_C <- weighted.mean(
    x = main_sample_control$test_compliance,
    w = main_sample_control$weights_w2,
    na.rm = TRUE
  )
  sample  <- sample %>% mutate(
    D_i = (test_compliance - Vbar_C) / (1 - Vbar_C)
  )
  return(sample)
}

pw$run_imputation_routine <- function(sample, covars_to_impute) {
  #' Wraps the alethea::impute_covars function to impute covariates.
  #' Note: adds missingness indicators for imputed covariates if greater than 10% missing.
  #'
  #' @param sample A data frame unique at the user level.
  #' @param covars_to_impute A character vector of covariates to impute.
  #'
  #' @return An updated `sample` dataframe with imputed covariates.
  #'
  sample <- alethea::impute_covars(
    d = as.data.frame(sample),
    covars = covars_to_impute,
    weights_var = "weights_w2",
    sample = "main_sample",
    deact_impute = TRUE
  )$d
  return(sample)
}

pw$run_winsorization_routine <- function(sample, vars_to_winsorize, pct = 0.99) {
  #' Adds winsorization columns to the sample for specified variables at specified percentile.
  #' Uses alethea::winsorize functionality.
  #'
  #' @param sample A data frame unique at the user level.
  #' @param covars_to_impute A character vector of covariates to winsorize.
  #' @param pct A numeric value representing the percentile to winsorize at (`0.99` by default).
  #'
  #' @return An updated `sample` dataframe including winsorized variables (with `_win` suffix).
  #'
  for (var_name in vars_to_winsorize) {
      var <- sample[[c(var_name)]]
      main_sample <- sample[["main_sample"]]
      var_name_win <- paste0(var_name, "_win")
      sample[, c(var_name_win)] <- alethea::winsorize(var, pct = 0.99, main_sample = main_sample)
  }
  return(sample)
}

pwh$weighted_quantile_safe <- function(x, w, q, digits = 10) {
  #' Calculates the weighted quantile of a numeric vector, rounding to `digits` decimal places.
  #' Relies on Hmisc::wtd.quantile implementation.
  #'
  #' @param x A numeric vector.
  #' @param w A numeric vector of weights.
  #' @param q A numeric value representing the quantile to calculate.
  #' @param digits An integer value for the number of decimal places to round to (default: `10`).
  #'
  #' @return A numeric value representing the weighted quantile.
  #'
  if (length(x) == 0) {
    return(NA)
  }
  if (length(q) != 1) {
    stop("q must be a single number")
  }
  return(
    round(
      Hmisc::wtd.quantile(
        x = x,
        weights = w,
        probs = c(q),
        normwt = TRUE,
        na.rm = TRUE
      ),
      digits
    )
  )
}

pw$build_moderator_plat_vars <- function(sample, baseline_ts_var, plat = "FB") {
  #' Build moderator variables for heterogeneity analysis.
  #' Creates splits for:
  #' - baseline use;
  #' - age;
  #' - baseline emotional state;
  #' - baseline political participation;
  #' - age interacted with participant_gender.
  #'
  #' @param sample A data frame unique at the user level.
  #' @param baseline_ts_var A string representing the baseline time spent variable.
  #' @param plat A string representing the recruitment_platform_string (`FB` by default).
  #'
  #' @return An updated `sample` dataframe including variables for moderator splits.
  #'
  baseline_use_median <- pwh$weighted_quantile_safe(
    x = sample[[baseline_ts_var]],
    w = sample$weights_w2,
    q = 0.5
  )
  age_median <- pwh$weighted_quantile_safe(
    x = sample$median_age_of_bracket,
    w = sample$weights_w2,
    q = 0.5
  )
  emot_pre_median <- pwh$weighted_quantile_safe(
    x = sample$emot_pre,
    w = sample$weights_w2,
    q = 0.5
  )
  polpart_pre_median <- pwh$weighted_quantile_safe(
    x = sample$weighted_index_political_participation_pre_election,
    w = sample$weights_w2,
    q = 0.5
  )
  sample <- sample %>%
    mutate(
      baseline_use_above_med = case_when(
        is.na(!!as.name(baseline_ts_var)) ~ NA,
        TRUE ~ !!as.name(baseline_ts_var) >= baseline_use_median
      ),
      undecided_baseline_use_above_med = case_when(
        is.na(baseline_use_above_med) | is.na(undecided) ~ NA,
        TRUE ~ baseline_use_above_med & undecided
      ),
      age_above_med = case_when(
            is.na(median_age_of_bracket) ~ NA,
            TRUE ~ median_age_of_bracket >= age_median
      ),
      emot_pre_above_med = case_when(
            is.na(emot_pre) ~ NA,
            TRUE ~ emot_pre >= emot_pre_median
      ),
      polpart_pre_above_med = case_when(
            is.na(weighted_index_political_participation_pre_election) ~ NA,
            TRUE ~ weighted_index_political_participation_pre_election >= polpart_pre_median
      ),
      gender_age_above_med = ifelse(
        str_detect(participant_gender, "other"),
        NA,
        paste0(participant_gender, "_", age_above_med)
      )
    )
  if (plat == "FB") {
    # civic page fan count only exists for Facebook
    civic_page_fan_count_median <- pwh$weighted_quantile_safe(
      x = sample$civic_page_like_count,
      w = sample$weights_w2,
      q = 0.5
    )
    sample <- sample %>%
      mutate(
        civic_page_fan_count_above_med = case_when(
          is.na(civic_page_like_count) ~ NA,
          TRUE ~ civic_page_like_count >= civic_page_fan_count_median
        )
      )
  }
  return(sample)
}

pwh$wtd_relative_time_spent_helper <- function(sample_ds, var_name, start_date, end_date) {
  #' Calculates weighted relative time spent for a given time period.
  #'
  #' @param sample_ds A data frame unique at the user-day level.
  #' @param var A string representing the name of the resulting variable (w/o recruitment_platform_string suffix).
  #' @param start_date A string representing the start date of the time period.
  #' @param end_date A string representing the end date of the time period.
  #'
  #' @return An updated `sample` dataframe including variables for relative time spent.
  #'
  fb_var <- paste0("fb_", var_name)
  ig_var <- paste0("ig_", var_name)
  res <- sample_ds %>%
    filter(ds >= as.Date(start_date) & ds <= as.Date(end_date)) %>%
    group_by(participant_user_id) %>%
    summarise(
        n_plat_obs = n(),
        na_fb_ts_base = sum(is.na(fb_ts_base)),
        na_ig_ts_base = sum(is.na(ig_ts_base)),
        fb_ts_base_wtd = mean(fb_ts_base_wtd, na.rm = TRUE),
        ig_ts_base_wtd = mean(ig_ts_base_wtd, na.rm = TRUE)
    ) %>%
    mutate(
      !!sym(fb_var) := ifelse(n_plat_obs > na_fb_ts_base, fb_ts_base_wtd, NA),
      !!sym(ig_var) := ifelse(n_plat_obs > na_ig_ts_base, ig_ts_base_wtd, NA)
    )
  return(res)
}


pw$build_wtd_relative_time_spent_vars <- function(sample, sample_ds) {
  #' Builds weighted relative time spent variables for baseline, endline, and post-endline periods.
  #'
  #' @param sample A data frame unique at the user level.
  #' @param sample_ds A data frame unique at the user-day level.
  #'
  #' @return An updated `sample` dataframe including variables for relative time spent in each period.
  #'
  time_spent_base <- pwh$wtd_relative_time_spent_helper(
    sample_ds = sample_ds,
    var_name = "ts_base_wtd_base",
    start_date = exp_dates$base$start,
    end_date = exp_dates$base$end
  )
  time_spent_deact <- pwh$wtd_relative_time_spent_helper(
    sample_ds = sample_ds,
    var_name = "ts_deact_wtd_base",
    start_date = exp_dates$end$start,
    end_date = exp_dates$end$end
  )
  time_spent_post <- pwh$wtd_relative_time_spent_helper(
    sample_ds = sample_ds,
    var_name = "ts_post_wtd_base",
    start_date = exp_dates$post$start,
    end_date = exp_dates$post$end
  )
  sample <- sample %>%
    left_join(time_spent_base, by = c("participant_user_id")) %>%
    left_join(time_spent_deact, by = c("participant_user_id")) %>%
    left_join(time_spent_post, by = c("participant_user_id")) %>%
    pw$run_winsorization_routine(
        vars_to_winsorize = c(
          "fb_ts_base_wtd_base", "ig_ts_base_wtd_base",
          "fb_ts_deact_wtd_base", "ig_ts_deact_wtd_base",
          "fb_ts_post_wtd_base", "ig_ts_post_wtd_base"
        ),
        pct = 0.99
    )
  return(sample)
}
