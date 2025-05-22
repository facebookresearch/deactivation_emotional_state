# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

###################################
###   descriptive - balance.R   ###
###################################

library(Hmisc)
library(tidyr)
library(xtable)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")

source("lib/outcome_dictionaries.R")
source("lib/constants.R")

devtools::load_all("~/alethea")
theme_set(theme_alethea())

# map "{covariate}_{value}" to human-readable labels
variable_label_map <- c(
  "median_age_of_bracket_median_age_of_bracket" = "Age",
  "participant_male_or_not_male" = "Male",
  "partyid_D" = "Democrat",
  "partyid_R" = "Republican",
  "participant_has_degree_TRUE" = "College",
  "participant_race_5_groups_hispanic" = "Hispanic",
  "participant_race_5_groups_white, non-hispanic" = "White, non-hispanic",
  "participant_race_5_groups_black, non-hispanic" = "Black, non-hispanic",
  "participant_income_lower" = "Lower income tercile",
  "participant_income_mid" = "Middle income tercile",
  "participant_2016_voter_string_Voted" = "Voted in 2016",
  "BATTLEGROUND_BLOCK_yes" = "Swing state",
  "baseline_use_plat_win_baseline_use_plat_win" = "Baseline use",
  "emot_pre_emot_pre" = "Emotional state index",
  "emot_happy_pre_emot_happy_pre" = "Happy",
  "emot_depress_pre_emot_depress_pre" = "Depressed x (-1)",
  "emot_anxious_pre_emot_anxious_pre" = "Anxious x (-1)"
)

main <- function() {
  main_sample_fb <- readRDS("analysis/descriptive/input/main_sample_fb.rds")
  main_sample_ig <- readRDS("analysis/descriptive/input/main_sample_ig.rds")

  main_sample_joint <- bind_rows(main_sample_fb, main_sample_ig) %>%
    mutate(
      baseline_use_plat_win = case_when(
        recruitment_platform_string == "FB" ~ fb_ts_base_wtd_base_win,
        recruitment_platform_string == "IG" ~ ig_ts_base_wtd_base_win,
        TRUE ~ NA_real_
      )
    )

  covars <- c(
    "median_age_of_bracket",
    "participant_male_or_not",
    "partyid",
    "participant_has_degree",
    "participant_race_5_groups",
    "participant_income",
    "participant_2016_voter_string",
    "BATTLEGROUND_BLOCK",
    "baseline_use_plat_win"
  )
  # Covariate balance tables at baseline and endline
  write_covar_balance(main_sample_joint, covars, svy_wave = 2)
  write_covar_balance(main_sample_joint, covars, svy_wave = 4)

  outcomes_dict <- make_emotion_outcomes_baseline_dict()
  pre_outcomes <- unlist(outcomes_dict, use.names = FALSE)
  # Outcome balance at baseline and endline
  write_covar_balance(
    main_sample_joint,
    pre_outcomes,
    svy_wave = 2,
    weights = "weights_w2",
    suffix = "_outcomes"
  )
  write_covar_balance(
    main_sample_joint,
    pre_outcomes,
    svy_wave = 4,
    weights = "weights_w4",
    suffix = "_outcomes"
  )
  message("Finished balance checks.")
}

## Create table that checks balance between deactivation and control groups for each recruitment_platform_string
write_covar_balance <- function(
  sample,
  covars,
  svy_wave = 4,
  weights = NULL,
  suffix = ""
) {
  #' Writes a LaTeX table comparing covariate balance between treatment and control groups.
  #' For each of FB and IG, reports the means of specified covariates in treatment and control groups,
  #' along with p-values for each individual comparison and an F-test of joint significance.
  #'
  #' @param sample A data frame containing the main sample data.
  #' @param covars A character vector specifying the covariates to be included in the balance table.
  #' @param svy_wave An integer specifying which survey wave to use (default is 4).
  #' @param weights An optional string specifying the name of a column containing weights.
  #'   If NULL, the table is unweighted. Default is NULL.
  #' @param suffix A string to append to the output file name. Default is "".
  #'
  #' @return A table containing the formatted balance table for two platforms (FB and IG).
  #'
  res_fb <- covar_balance_plat(sample, covars, plat = "FB", svy_wave, weights)
  res_ig <- covar_balance_plat(sample, covars, plat = "IG", svy_wave, weights)
  balance_table <- full_join(res_fb$tab, res_ig$tab, by = c("covariate", "value")) %>%
    mutate(
      combined_key = paste(covariate, value, sep = "_"),
      row_labels   = variable_label_map[combined_key]
    )
  rownames(balance_table) <- balance_table$row_labels
  balance_table <- balance_table %>%
    select(-c(covariate, row_labels, value, t.value.y, t.value.x, combined_key))
  # format table
  n_cols <- ncol(balance_table)
  n_rows <- nrow(balance_table)
  # format and save tables
  addtorow <- list()
  addtorow$pos <- list(0, 0, 0, 0, n_rows, n_rows, n_rows)
  addtorow$command <- c(
    sprintf(
      "& \\multicolumn{%s}{c}{Facebook} & \\multicolumn{%s}{c}{Instagram} \\\\\n", 
      n_cols / 2, n_cols / 2
    ),
    sprintf(
      "\\cmidrule(r){%s-%s} \\cmidrule(r){%s-%s} \\\\\n", 
      2, 2 + n_cols / 2 - 1,
      2 + n_cols / 2, n_cols + 1
    ),
    paste("&", paste0("(", seq(1, n_cols), ")", collapse = " & "), "\\\\\n"),
    "Covariate & Deactivation & Control & Difference & $p$-value & Deactivation & Control & Difference & $p$-value \\\\\n",
    paste0("\\hline \n"),
    paste0(
      "\\# obs. & ", format(res_fb$n_test, big.mark = ",", scientific = FALSE),
      " & ", format(res_fb$n_ctrl, big.mark = ",", scientific = FALSE),
      "& & & ", format(res_ig$n_test, big.mark = ",", scientific = FALSE),
      " & ",  format(res_ig$n_ctrl, big.mark = ",", scientific = FALSE),
      "\\\\\n"
    ),
    paste0(
      "& F-stat & ", round(res_fb$fval, 3),
      "& $p$-value & ", round(res_fb$pval, 3),
      "& F-stat & ", round(res_ig$fval, 3),
      "& $p$-value & ", round(res_ig$pval, 3),
      "\\\\\n"
    )
  )
  balance_table_tex <- xtable(balance_table, type = "latex")
  align(balance_table_tex) <- c("l", rep("c", ncol(balance_table_tex)))
  print.xtable(
    balance_table_tex,
    add.to.row = addtorow,
    include.colnames = FALSE,
    file = sprintf(
      "analysis/descriptive/output/tables/covariate_balance_wave%s%s%s_table.tex",
      svy_wave,
      ifelse(is.null(weights), "", "_weighted"),
      suffix
    )
  )
  return(balance_table_tex)
}

covar_balance_plat <- function(sample, covars, plat = "FB", svy_wave = 4, weights = NULL) {
  #' Constructs a covariate balance table for a specific recruitment_platform_string.
  #' Filters the sample for a given recruitment_platform_string and wave, iterates over each covariate,
  #' and computes group means and differences and signifiance (from t-test).
  #'
  #' @param sample A data frame containing the main sample data.
  #' @param covars A character vector specifying the covariates to be included in the balance table.
  #' @param plat A string specifying the recruitment_platform_string name. Default is "FB".
  #' @param svy_wave An integer specifying which survey wave to use. Default is 4.
  #' @param weights An optional string specifying the name of a column containing weights.
  #'   If NULL, the table is unweighted. Default is NULL.
  #'
  #' @return A list with the following elements:
  #'   - A data frame containing variable-level balance results.
  #'   - The F-statistic for the joint significance test.
  #'   - The p-value for the joint significance test.
  #'   - The number of test group observations.
  #'   - The number of control group observations.
  #'
  sample_filt <- sample %>% filter(recruitment_platform_string == plat & !is.na(get(paste0("ENDDT_W", svy_wave))))
  balance_table <- data.frame(
    covariate = character(),
    value = character(),
    test.value = numeric(),
    ctrl.value = numeric(),
    diff = numeric(),
    t.value = numeric(),
    p.value = numeric()
  )
  covars_on_table <- c()
  for (var in covars) {
    if (class(sample_filt[, var]) == "numeric") {
      # run t-stat, store in table 
      balance_table <- run_t_test(balance_table, sample_filt, var, weights)
      # add to covars on final table
      covars_on_table <- c(covars_on_table, var)
    } else if (class(sample_filt[, var]) %in% c("character", "factor", "logical")) {
      values <- unique(sample_filt[, var])
      values <- values[!is.na(values)]
      values <- values[1:(length(values) - 1)]
      # Select specific factors based on current covariate
      if (var == "participant_male_or_not") values <- c("male")
      if (var == "participant_race_5_groups") values <- c("hispanic", "white, non-hispanic", "black, non-hispanic")
      if (var == "partyid") values <- c("D", "R")
      if (var == "participant_has_degree") values <- c(TRUE)
      if (var == "participant_2016_voter_string" | var == "VOTE16_W1") values <- c("Voted")
      if (var == "BATTLEGROUND_BLOCK") values <- c("yes")
      if (var == "participant_income") values <- c("lower", "mid")
      for (value in values){
        # create factor binary covariate
        factor_name <- paste(var, value, sep = "_") %>%
          gsub(",", "", .) %>%
          gsub(" ", "", .) %>%
          gsub("-", "_", .)
        sample_filt[, factor_name] <- (sample_filt[, var] == value)
        # run t-test
        balance_table <- run_t_test(balance_table, sample_filt, factor_name, weights, c(var, value))
        # add to covars on final table
        covars_on_table <- c(covars_on_table, factor_name)
      }
    }
  }
  joint_test <- joint_balance(sample_filt, covars_on_table, svy_wave = svy_wave, plat = plat)
  f_value <- summary(joint_test)$fstatistic
  f_pvalue <- pf(f_value[1], f_value[2], f_value[3], lower.tail=FALSE)
  n_test <- sample_filt %>% filter(experiment_group == "test") %>% nrow()
  n_ctrl <- sample_filt %>% filter(experiment_group == "control") %>% nrow()
  return(list(
    tab = balance_table,
    fval = f_value[1],
    pval = f_pvalue,
    n_test = n_test,
    n_ctrl = n_ctrl
  ))
}

impute_missing <- function(x, w = NULL) {
  #' Helper function to impute missing values.
  #' Replaces missing values in a numeric vector with either the unweighted mean or a weighted mean
  #' (if weights are provided).
  #'
  #' @param x A numeric vector with possible missing values.
  #' @param w An optional numeric vector of the same length as x, containing weights.
  #'   If NULL, performs unweighted mean imputation. Default is NULL.
  #'
  #' @return An updated version of x with missing values replaced.
  #'
  if (is.null(w)) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
  } else {
    x[is.na(x)] <- Hmisc::wtd.mean(x, w, na.rm = TRUE)
  }
  return(x)
}

run_t_test <- function(balance_table, sample_filt, var, weights = NULL, value = NULL) {
  #' Runs a T-test for a single variable (numeric or binary factor) and adds to balance table.
  #' Imputes missing values, fits a linear model comparing test vs. control, then appends the result
  #' (test means, control means, difference, p-value, etc.) to the provided balance table.
  #'
  #' @param balance_table A data frame that accumulates balance results across variables.
  #' @param sample_filt A data frame filtered to the relevant recruitment_platform_string and wave.
  #' @param var A string naming the current variable (numeric or binary factor) on which to run the T-test.
  #' @param weights An optional string specifying the name of a column containing weights.
  #'   If NULL, the t-test is unweighted. Default is NULL.
  #' @param value An optional vector of length 2 containing the original covariate name and
  #'   factor level string, if var was derived from a factor. Default is NULL.
  #'
  #' @return A data frame with one row appended containing t-test results.
  #'
  sample_filt[[var]] <- impute_missing(
    x = sample_filt[[var]],
    w = if (!is.null(weights)) sample_filt[[weights]] else NULL
  )
  t_wave <- lm(
    formula(paste0(var, " ~ control")),
    data = sample_filt %>% mutate(control = (experiment_group == "control")),
    weights = if (!is.null(weights)) sample_filt[[weights]] else NULL
  )
  if (!is.null(value)) {
    var <- value[1]
    value <- value[2]
  } else {
    value <- var
  }
  # extract t-test results and format for table
  t_wave_sum <- summary(t_wave)
  t_wave_coef <- coef(t_wave)
  balance_table <- balance_table %>%
    add_row(
      covariate = var,
      value = as.character(value),
      test.value = t_wave_coef[[1]],                   # Intercept
      ctrl.value = sum(t_wave_coef),                   # Intercept + Control
      diff = - t_wave_coef[[2]],                       # - Control
      t.value = t_wave_sum[["coefficients"]][2, 3],    # t-stat, p-value for Control
      p.value = t_wave_sum[["coefficients"]][2, 4]
    )
  return(balance_table)
}


joint_balance <- function(sample, covars, svy_wave = 4, plat = "FB") {
  #' Performs a joint significance test for balance on multiple covariates from linear model.
  #'
  #' @param sample A data frame containing the main sample data.
  #' @param covars A character vector specifying the covariates of interest.
  #' @param svy_wave An integer specifying which survey wave to use. Default is 4.
  #' @param plat A string specifying the recruitment_platform_string name. Default is "FB".
  #'
  #' @return A fitted linear model object.
  #'
  svy_sbs <- sample %>%
    filter(recruitment_platform_string == plat & !is.na(paste0("ENDDT_W", svy_wave))) %>%
    select(experiment_group, BLOCK_ID, all_of(covars)) %>%
    na.omit()
  m1 <- paste0("experiment_group == 'test' ~ ", paste(covars, collapse = " + "))
  m1_output <- lm(formula(m1), data = svy_sbs)
  return(m1_output)
}


## Execute
main()
