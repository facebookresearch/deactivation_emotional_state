# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

####################################################
###   comparison - compute_non_exper_effects.R   ###
####################################################

source("lib/compute_effects.R")
source("lib/prepare_outputs.R")
source("lib/constants.R")
source("lib/outcome_dictionaries.R")

library(xtable)
library(stringr)
library(forcats)
library(binsreg)
library(fixest)
library(tidyr)


main <- function() {
  main_sample_fb <- readRDS("analysis/comparison/input/main_sample_fb.rds")
  main_sample_ig <- readRDS("analysis/comparison/input/main_sample_ig.rds")

  model_covars_fb <- readRDS("analysis/comparison/input/model_covars_fb.rds")
  model_covars_ig <- readRDS("analysis/comparison/input/model_covars_ig.rds")

  lasso_covars_fb <- readRDS("analysis/comparison/input/lasso_covars_fb.rds")
  lasso_covars_ig <- readRDS("analysis/comparison/input/lasso_covars_ig.rds")

  selected_covars_fb <- readRDS("analysis/comparison/input/selected_covars_fb.rds")
  selected_covars_ig <- readRDS("analysis/comparison/input/selected_covars_ig.rds")

  emotion_baseline_vars <- unlist(make_emotion_outcomes_baseline_dict(), use.names = FALSE)

  # add control vars
  main_sample_fb <- main_sample_fb %>%
    mutate(
      income_mid = as.numeric(participant_income == "mid"),
      income_up = as.numeric(participant_income == "upper"),
      partyid = fct_relevel(partyid, c("I", "D", "R")) # relevel to get coeffs on D, R
    )
  main_sample_ig <- main_sample_ig %>%
    mutate(
      income_mid = as.numeric(participant_income == "mid"),
      income_up = as.numeric(participant_income == "upper"),
      partyid = fct_relevel(partyid, c("I", "D", "R")) # relevel to get coeffs on D, R
    )
  main_sample_joint <- bind_rows(main_sample_fb, main_sample_ig)

  # Main non-experimental estimates table
  create_non_experimental_table(
    df = main_sample_fb,
    selected_covars = selected_covars_fb,
    model_covars = model_covars_fb,
    plat = "FB"
  )
  create_non_experimental_table(
    df = main_sample_ig,
    selected_covars = selected_covars_ig,
    model_covars = model_covars_ig,
    plat = "IG"
  )

  # Selected covariates for baseline variables
  selected_covars_fb <- unlist(
    lapply(
      emotion_baseline_vars,
      select_covars_lasso,
      df = main_sample_fb,
      dv_pre_list = setNames(rep("", 4), emotion_baseline_vars), # no pre variable for baseline outcome
      weights_var = "weights_w2",
      lasso_covars = lasso_covars_fb
    ),
    recursive = FALSE
  )
  selected_covars_ig <- unlist(
    lapply(
      emotion_baseline_vars,
      select_covars_lasso,
      df = main_sample_ig,
      dv_pre_list = setNames(rep("", 4), emotion_baseline_vars),
      weights_var = "weights_w2",
      lasso_covars = lasso_covars_ig
    ),
    recursive = FALSE
  )

  # 1. baseline use
  baseline_use_regs_fb <- run_regs(
    df = main_sample_fb,
    covars = "fb_ts_base_wtd_base_win",
    model_covars = model_covars_fb,
    selected_covars = selected_covars_fb
  )
  baseline_use_regs_ig <- run_regs(
    df = main_sample_ig,
    covars = "ig_ts_base_wtd_base_win",
    model_covars = model_covars_ig,
    selected_covars = selected_covars_ig
  )

  # plot binned scatterplot (only for index)
  plot_baseline_use_scatter(
    df = main_sample_fb,
    model_covars = c(model_covars_fb, selected_covars_fb$emot_pre),
    plat = "FB"
  )
  plot_baseline_use_scatter(
    df = main_sample_ig,
    model_covars = c(model_covars_ig, selected_covars_ig$emot_pre),
    plat = "IG"
  )

  # 2. control regs
  # controls: college, income, age, race, participant_gender, party ID
  controls <- c(
    "participant_has_degree", "income_mid", "income_up",
    "median_age_of_bracket", "minority", "participant_male_or_not", "partyid"
  )
  context_regs <- run_regs(main_sample_joint, controls)
  control_effects_table(context_regs)

  # 3. ABEG non experimental estimates
  non_exper_reg_fb <- calc_non_experimental_est(
    df = main_sample_fb,
    model_covars = model_covars_fb,
    selected_covars = selected_covars_fb,
    iv = iv_fb,
    plat = "FB"
  )
  non_exper_reg_ig <- calc_non_experimental_est(
    df = main_sample_ig,
    model_covars = model_covars_ig,
    selected_covars = selected_covars_ig,
    iv = iv_ig,
    plat = "IG"
  )

  # non experimental estimates, unconditioned
  non_exper_reg_fb_uncon <- calc_non_experimental_est(
    df = main_sample_fb,
    model_covars = model_covars_fb,
    selected_covars = selected_covars_fb,
    iv = iv_fb,
    plat = "FB",
    include_controls = FALSE
  )
  non_exper_reg_ig_uncon <- calc_non_experimental_est(
    df = main_sample_ig,
    model_covars = model_covars_ig,
    selected_covars = selected_covars_ig,
    iv = iv_ig,
    plat = "IG",
    include_controls = FALSE
  )

  # get scalars
  contextualize_scalars(context_regs[[1]])
}

create_non_experimental_table <- function(
  df,
  selected_covars,
  model_covars,
  plat,
  outcome = "emot_post"
) {
  #' Creates a table comparing estimates from different non-experimental regression specifications.
  #' Estimates include:
  #'   1. "Experimental": IV regression of outcome on time spent post instrumented by treatment
  #'   2. "Non-experimental (no controls)": OLS of baseline outcome on time spent pre
  #'   3. "Non-experimental (controls)": OLS of baseline outcome on time spent pre and covars
  #'   4. "Non-experimental (panel)": OLS of change in outcome on change in time spent
  #' Writes table to 'analysis/comparison/output/tables/non_experimental_table_<plat>.tex'.
  #'
  #' @param df A data frame containing the main sample.
  #' @param selected_covars A list of selected covariates for each outcome.
  #' @param model_covars A vector of model block-id covariates.
  #' @param plat A string indicating the recruitment_platform_string ("FB" or "IG"). Default is "FB".
  #' @param outcome A string indicating the outcome variable. Default is "emot_post".
  #'
  # Define time spent variables, covariates
  outcome_pre <- str_replace_all(outcome, "_post", "_pre")
  if (plat == "FB") {
    ts_pre <- "fb_ts_base_wtd_base_win"
    ts_post <- "fb_ts_deact_wtd_base_win"
  } else {
    ts_pre <- "ig_ts_base_wtd_base_win"
    ts_post <- "ig_ts_deact_wtd_base_win"
  }
  covars <- paste(
    paste0(selected_covars[[outcome]], collapse = " + "),
    paste0(model_covars, collapse = " + "),
    sep = " + "
  )
  # Remove baseline outcome from covars (for regressions where its on LHS)
  covars_no_pre <- str_replace_all(covars, paste0(outcome_pre, " \\+"), "")

  # Estimate 1: "Experimental"
  # IV formula: outcome ~ ts_post + covars | treated + covars
  f <- as.formula(
    paste0(
      outcome,
      " ~ ",
      paste(ts_post, covars, sep = " + "),
      " | ",
      paste("treated", covars, sep = " + ")
    )
  )
  est1 <- ivreg(f, data = df, weights = df$weights_w2)
  out1 <- summary(est1)

  # Estimate 2: "Non-experimental (no controls)"
  f <- as.formula(
    paste0(
      outcome_pre,
      " ~ ",
      ts_pre
    )
  )
  est2 <- lm(f, data = df, weights = df$weights_w2)
  out2 <- summary(est2)

  # Estimate 3: "Non-experimental (controls)"
  f <- as.formula(
    paste0(
      outcome_pre,
      " ~ ",
      ts_pre,
      " + ",
      covars_no_pre
    )
  )
  est3 <- lm(f, data = df, weights = df$weights_w2)
  out3 <- summary(est3)

  # Estimate 4: "Non-experimental (panel)"
  df_c <- df %>% filter(experiment_group == "control")
  outcome_delta <- str_replace_all(outcome, "_post", "_delta")
  df_c[[outcome_delta]] <- df_c[[outcome]] - df_c[[outcome_pre]]
  df_c[["ts_delta"]] <- df_c[[ts_post]] - df_c[[ts_pre]]
  f <- as.formula(
    paste0(
      outcome_delta,
      " ~ ",
      "ts_delta",
      " + ",
      covars_no_pre
    )
  )
  est4 <- lm(f, data = df_c, weights = df_c$weights_w2)
  out4 <- summary(est4)

  # Extract coefficients
  coef1 <- coef(out1)[ts_post, "Estimate"]
  coef2 <- coef(out2)[ts_pre, "Estimate"]
  coef3 <- coef(out3)[ts_pre, "Estimate"]
  coef4 <- coef(out4)["ts_delta", "Estimate"]
  se1 <- coef(out1)[ts_post, "Std. Error"]
  se2 <- coef(out2)[ts_pre, "Std. Error"]
  se3 <- coef(out3)[ts_pre, "Std. Error"]
  se4 <- coef(out4)["ts_delta", "Std. Error"]
  n1 <- attr(out1$coefficients, "nobs")
  n2 <- length(out2$residuals)
  n3 <- length(out3$residuals)
  n4 <- length(out4$residuals)

  # Create table
  tab <- rbind(
    c(coef1, coef2, coef3, coef4),
    c(se1, se2, se3, se4)
  ) %>% round(3)
  tab[2, ] <- paste0("(", tab[2, ], ")")
  tab <- cbind(
    c("Baseline use", ""), tab
  )
  n_obs <- c(n1, n2, n3, n4)
  n_obs <- format(n_obs, big.mark = ",", scientific = FALSE)

  addtorow <- list()
  addtorow$pos <- list(0, 0, nrow(tab), nrow(tab))
  addtorow$command <- c(
    paste(" & ", paste0("(", seq(1, ncol(tab) - 1), ")", collapse = " & "), "\\\\ \n"),
    paste(
      " ",
      "Experimental",
      "\\multicolumn{2}{c}{Cross-sectional}",
      "Difference \\\\ \n",
      sep = " & "
    ),
    "Additional controls & Yes & & Yes & Yes \\\\ \n \\hline \n",
    paste0("\\# obs. & ", paste(n_obs, collapse = " & "), " \\\\ \n")
  )
  print(
    xtable(tab, align = c("l", "l", "c", "c", "c", "c")),
    type = "latex",
    include.rownames = FALSE,
    include.colnames = FALSE,
    add.to.row = addtorow,
    floating.environment = "tabular",
    file = paste0(
      "analysis/comparison/output/tables/non_experimental_table_",
      tolower(plat),
      ".tex"
    )
  )
}


run_baseline_reg <- function(df, outcome_var, covars, controls, weights_var) {
  #' Runs regression for a given baseline outcome variable.
  #'
  #' @param df A data frame containing the main sample.
  #' @param outcome_var A string indicating the outcome variable.
  #' @param covars A vector of covariates of interest.
  #' @param controls A vector of control variables.
  #' @param weights_var A string indicating the variable to use as weights.
  #'
  #' @return A regression object (`lm`) for the baseline regression.
  #'
  f <- as.formula(
    paste0(
      outcome_var, # outcome
      " ~ ",
      paste0(covars, collapse = " + "), # covars of interest
      if (is.null(controls)) "" else " + ",
      str_replace_all(
          paste0(controls, collapse = " + "),
          paste0("\\+ ", outcome_var), ""
      ) # any control vars
    )
  )
  lm <- lm(f, data = df, weights = df[[weights_var]])
  return(lm)
}

run_regs <- function(df, covars, model_covars = NULL, selected_covars = NULL) {
  #' Runs regressions for a given set of covariates for each baseline outcome.
  #'
  #' @param df A data frame containing the main sample.
  #' @param covars A vector of covariates of interest.
  #' @param model_covars A vector of model block-id covariates.
  #' @param selected_covars A list of selected covariates for each outcome.
  #'
  #' @return A list of regression objects (`lm`) for each baseline outcome.
  #'
  outcomes <- unlist(make_emotion_outcomes_baseline_dict(), use.names = FALSE)
  res <- list()
  for (outcome in outcomes) {
    outcome_post <- str_replace_all(outcome, "_pre", "_post")
    res[[outcome]] <- run_baseline_reg(
      df,
      outcome,
      covars,
      if (is.null(selected_covars)) model_covars else
        c(model_covars, selected_covars[[outcome_post]]),
      "weights_w2"
    )
  }
  return(res)
}

calc_non_experimental_est <- function(
  df,
  model_covars,
  selected_covars,
  iv,
  plat = "FB",
  include_controls = TRUE
) {
  #' Calculates non-experimental estimate as in ABEG for a given recruitment_platform_string.
  #' Calculates LATE on deactivation use, divides baseline time spent by LATE,
  #' and runs regression out baseline outcome variable on htilde.
  #'
  #' @param df A data frame containing the main sample.
  #' @param model_covars A vector of model block-id covariates.
  #' @param selected_covars A list of selected covariates for each outcome.
  #' @param iv A string indicating the instrumental variable to use.
  #' @param plat A string indicating the recruitment_platform_string ("FB" or "IG"). Default is "FB".
  #' @param include_controls A boolean indicating whether to include control variables. 
  #'  Default is TRUE.
  #'
  #' @return A list of regression objects (`lm`) for each baseline outcome.
  #'
  if (plat == "FB") {
    dv <- "fb_ts_deact_wtd_base_win"
  } else if (plat == "IG") {
    dv <- "ig_ts_deact_wtd_base_win"
  }
  dv_pre <- str_replace(dv, "deact", "base")

  # calculate LATE on deactivation use
  ts_res <- run_outcome(
    df,
    model_covars = model_covars,
    selected_covars = NULL,
    iv = iv,
    dv = dv,
    dv_label = paste0("Baseline use (", plat, ")"),
    weights_var = "weights_w2",
    model_type = "PLATE",
    verbose = FALSE
  )
  ts_late <- ts_res %>% filter(str_detect(dv_label, ("Baseline use"))) %>% pull(ate)

  # divide baseline time spent by LATE
  htilde <- paste0("htilde_", tolower(plat))
  df[[htilde]] <- df[[dv_pre]] / ts_late

  # run regression as specified in 655, ABEG
  res <- run_regs(
    df,
    htilde,
    if (include_controls) model_covars,
    if (include_controls) selected_covars
  )
  return(res)
}


control_effects_table <- function(context_regs, digits = 3) {
  #' Creates table of regression coefficients for control regressions for each baseline outcome.
  #' Writes table to 'analysis/comparison/output/tables/control_regs_table.tex'.
  #'
  #' @param context_regs A list of regression objects (`lm`) for each baseline outcome.
  #' @param digits Number of digits to round to. Default is 3.
  #'
  # Extract variable names, coefficients, and standard errors
  formatted_data <- regs_vals_to_df(context_regs)
  colnames(formatted_data) <- c("Var", "Index", "Happy", "Depressed x (-1)", "Anxious x (-1)")
  col_names_dict <- list(
    "(Intercept)" = "Intercept",
    "participant_has_degreeTRUE" = "College",
    "income_mid" = "Middle income",
    "income_up" = "Upper income",
    "median_age_of_bracket" = "Age",
    "minorityTRUE" = "Minority",
    "participant_male_or_notnot male" = "Female",
    "partyidD" = "Democrat",
    "partyidR" = "Republican"
  )
  formatted_data <- formatted_data %>%
    mutate(Var = ifelse(Var %in% names(col_names_dict), col_names_dict[Var], Var)) %>%
    filter(Var != "Intercept")
  formatted_data$Var[seq(2, nrow(formatted_data), by = 2)] <- ""
  colnames(formatted_data)[[1]] <- ""
  # Add headers to table
  outcomes_dict <- make_emotion_outcomes_label_dict()
  outcomes <- unlist(outcomes_dict, use.names = FALSE)
  n_obs <- lapply(context_regs, function(x) length(residuals(x))) %>%
    unlist() %>%
    format(big.mark = ",", scientific = FALSE)
  addtorow <- list(
    pos = list(0, 0, nrow(formatted_data)),
    command = c(
      paste("&", paste0("(", seq(1, ncol(formatted_data) - 1), ")", collapse = " & "), "\\\\ \n"),
      paste0(
        "\\cmidrule(lr){2-5} & Index & ",
        paste(outcomes[2:length(outcomes)], collapse = " & "),
        "\\\\ \n"
      ),
      paste0("\\# obs. & ", paste(n_obs, collapse = " & "), " \\\\ \n")
    )
  )
  print(xtable(formatted_data, align = c("l", "l", "c", "c", "c", "c")),
    type = "latex",
    floating = FALSE,
    add.to.row = addtorow,
    include.rownames = FALSE,
    include.colnames = FALSE,
    file = "analysis/comparison/output/tables/control_regs_table.tex"
  )
}


plot_baseline_use_scatter <- function(df, model_covars, plat = "FB") {
  #' Creates a binned scatterplot of baseline use vs. baseline emotional state index.
  #' Saves plot to 'analysis/comparison/output/pngs/baseline_use_scatter_<plat>.png/pdf'.
  #'
  #' @param df A data frame containing the main sample.
  #' @param model_covars A vector of model block-id covariates.
  #' @param plat A string indicating the recruitment_platform_string ("FB" or "IG"). Default is "FB".
  #'
  xvar <- paste0(tolower(plat), "_ts_base_wtd_base_win")
  covars <- df %>% select(all_of(model_covars))
  # estimate binscatter
  # 1. use linear best fit line `polyreg = 1`
  # 2. set `randcut = 1`, to prevent from dropping points
  # 3. weights in this formula need to have sum > 1, so we adjust accordingly
  # (since weights are interpreted as frequency weights)
  weights_adj <- df$weights_w2 * 10
  binscat <- binsreg(
    df$emot_pre,
    df[[xvar]],
    w = covars,
    polyreg = 1,
    randcut = 1,
    weights = weights_adj
  )
  po <- binscat$bins_plot + labs(
    x = "Normalized baseline use",
    y = "Baseline emotional state index\n (standard deviations)"
  )
  save_plot(
    po,
    dir = "analysis/comparison/output/",
    fname = paste0("baseline_use_scatter_", tolower(plat)),
    width = 5,
    height = 4
  )
}

# Estimate scalars
contextualize_scalars <- function(context_regs) {
  #' Creates scalars from coefficients of control regressions. Writes scalars to
  #' 'analysis/comparison/output/scalars/contextualize_primary_effects_scalars.tex'.
  #'
  #' @param context_regs A list of regression objects (`lm`) for each baseline outcome, 
  #'  regressing baseline emotional state index on controls.
  #'
  scalars <- list(
    "ContextIndexCollegeCoef" = coef(context_regs)["participant_has_degreeTRUE"],
    "ContextIndexIncomeMidCoef" = coef(context_regs)["income_mid"],
    "ContextIndexMinorityCoef" = coef(context_regs)["minorityTRUE"],
    # Note this coefficient is negative, so we take absolute value
    "ContextIndexFemaleCoef" = abs(coef(context_regs)["participant_male_or_notnot male"]),
    # Note here we report the effect of R vs. D, so we take difference
    "ContextIndexRepublicanCoef" = coef(context_regs)["partyidR"] - coef(context_regs)["partyidD"]
  )
  save_scalars_tex(
    scalars,
    filename = "analysis/comparison/output/scalars/contextualize_primary_effects_scalars.tex",
    digits = 2
  )
}

main()
