# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

###############################################
###   primary - prepare_primary_outputs.R   ###
###############################################

source("lib/constants.R")
source("lib/outcome_dictionaries.R")
source("lib/prepare_outputs.R")
# NOTE: ggplot2, forcats loaded in source

devtools::load_all("~/alethea") # for sharpened_fdr
theme_set(theme_alethea())
library(scales)
library(stringr)
library(cowplot)
library(patchwork)
library(ggpubr)

library(xtable)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")

main <- function() {
  main_sample_fb <- readRDS("analysis/primary/input/main_sample_fb.rds")
  main_sample_ig <- readRDS("analysis/primary/input/main_sample_ig.rds")

  res_fb <- readRDS("analysis/primary/output/res_fb.rds")
  res_ig <- readRDS("analysis/primary/output/res_ig.rds")

  res_fb_first_stage <- readRDS("analysis/primary/output/res_fb_first_stage.rds")
  res_ig_first_stage <- readRDS("analysis/primary/output/res_ig_first_stage.rds")

  res_og_fb <- readRDS("analysis/primary/output/res_og_fb.rds")
  res_og_ig <- readRDS("analysis/primary/output/res_og_ig.rds")

  res_uncut_fb <- readRDS("analysis/primary/output/res_uncut_fb.rds")
  res_uncut_ig <- readRDS("analysis/primary/output/res_uncut_ig.rds")

  emotion_leebounds_fb <- readRDS("analysis/primary/output/emotion_leebounds_fb.rds")
  emotion_leebounds_ig <- readRDS("analysis/primary/output/emotion_leebounds_ig.rds")

  # Create primary outcomes tables
  write_first_stage_table(res_fb_first_stage, res_ig_first_stage)

  ate_tables <- primary_outcome_table(
    res_fb = res_fb,
    res_ig = res_ig,
    estimate = "PLATE"
  )
  ate_og_tables <- primary_outcome_table(
    res_fb = res_og_fb,
    res_ig = res_og_ig,
    estimate = "PLATE",
    suffix = "og_"
  )
  ate_uncut_tables <- primary_outcome_table(
    res_fb = res_uncut_fb,
    res_ig = res_uncut_ig,
    estimate = "PLATE",
    suffix = "uncut_"
  )
  ate_lb_tables <- primary_outcome_table(
    res_fb = emotion_leebounds_fb,
    res_ig = emotion_leebounds_ig,
    estimate = "PATE",
    leebounds = TRUE
  )

  # Create primary outcomes figures
  make_combined_plot(res_fb, res_ig)

  # Create primary scalars
  scalars <- primary_scalars(
    ate_table_fb = ate_tables$fb,
    ate_table_ig = ate_tables$ig,
    ate_table_og_fb = ate_og_tables$fb,
    ate_table_og_ig = ate_og_tables$ig,
    ate_table_lb_fb = ate_lb_tables$fb,
    ate_table_lb_ig = ate_lb_tables$ig,
    first_stage_reg_fb = res_fb_first_stage,
    first_stage_reg_ig = res_ig_first_stage
  )
  pct_scalars <- get_percentile_scalars(
    scalars = scalars,
    df_fb = main_sample_fb,
    df_ig = main_sample_ig,
    res_ate_fb = res_fb,
    res_ate_ig = res_ig
  )
  scalars_dig <- rep(3, length(scalars))
  pct_scalars_dig <- rep(1, length(pct_scalars))
  save_scalars_tex(
    scalar_list = c(scalars, pct_scalars),
    digits = c(scalars_dig, pct_scalars_dig),
    filename = "analysis/primary/output/scalars/primary_scalars.tex"
  )
}

primary_outcome_table <- function(
  res_fb,
  res_ig,
  estimate = "PLATE",
  leebounds = FALSE,
  suffix = ""
) {
  #' Create primary outcome tables for Facebook and Instagram treatment effect estimates.
  #' Calls `write_platform_table()` to write as LaTeX tables.
  #'
  #' @param res_fb A dataframe of treatment effects for all outcomes for Facebook.
  #' @param res_ig A dataframe of treatment effects for all outcomes for Instagram.
  #' @param estimate A string representing the estimate of interest. Default is `PLATE`.
  #' @param leebounds A boolean indicating whether the estimates include Lee bounds.
  #' @param suffix A string to append to the filename for output path.
  #'
  #' @return A list of two dataframes for Facebook (`fb`) and Instagram (`ig`) output tables.
  #'
  res <- bind_rows(
    res_fb %>% mutate(plat = "FB"),
    res_ig %>% mutate(plat = "IG")
  ) %>%
    select(plat, everything())
  # filter to estimate of interest
  if ("est" %in% colnames(res)) {
    res <- res %>% filter(est == estimate) %>% select(-est)
  }
  # select columns of interest
  if (leebounds) {
    latex_res <- res %>%
      select(plat, dv_label, itt, ci_low, ci_hi, lee_lower_tight, lee_upper_tight)
    colnames(latex_res) <- c(
      "Platform", "Hypothesis", "Effect", "95\\% CI LB", "95\\% CI UB", "Lee LB", "Lee UB"
    )
  } else {
    # round p-value to nearest 3rd digit, if ~0, replace with < 0.001
    res <- res %>%
        mutate(pval = round(pval, 3)) %>% 
        mutate(pval = ifelse(pval == 0, "<0.001", sprintf("%.3f", pval)))
    latex_res <- res %>% select(plat, dv_label, ate, std_error, pval)
    colnames(latex_res) <- c("Platform", "Hypothesis", "Effect", "Standard error", "$p$-value")
  }
  if (estimate == "PLATE") {
    effect_name <- "Treatment effect"
  } else if (estimate == "PATE") {
    effect_name <- "ITT effect"
  }
  names(latex_res)[names(latex_res) == "Effect"] <- effect_name
  ate_table_fb <- write_platform_table(latex_res, "FB", estimate, leebounds, suffix)
  ate_table_ig <- write_platform_table(latex_res, "IG", estimate, leebounds, suffix)
  return(list(fb = ate_table_fb, ig = ate_table_ig))
}

write_platform_table <- function(res, plat, estimate, leebounds, suffix) {
  #' Create a LaTeX version of primary outcome table.
  #' Writes table to 'analysis/primary/output/tables/emotion_outcomes_*.tex'.
  #'
  #' @param res A dataframe of treatment effects for all primary outcomes.
  #' @param plat A string indicating the recruitment_platform_string ('FB' or 'IG').
  #' @param estimate A string representing the estimate of interest.
  #' @param leebounds A boolean indicating whether the estimates include Lee bounds.
  #' @param suffix A string to append to the filename for output path.
  #'
  #' @return A dataframe of recruitment_platform_string-specific primary outcome results.
  #'
  res_plat <- res %>% filter(Platform == plat) %>% select(-Platform)
  names(res_plat)[names(res_plat) == "Hypothesis"] <- ""
  addtorow <- list(
    pos = list(-1),
    command = c(paste(
      "\\hline \n", "&",
      paste0("(", seq(1, ncol(res_plat) - 1), ")", collapse = " & "),
      "\\\\ \n"
    ))
  )
  table <- xtable(res_plat, digits = 3, align = c("l", "l", rep("c", ncol(res_plat) - 1)))
  print(
    table,
    add.to.row = addtorow,
    hline.after = c(0, nrow(table)),
    type = "latex",
    include.rownames = FALSE,
    sanitize.text.function = identity,
    file = paste0(
      "analysis/primary/output/tables/emotion_outcomes_",
      ifelse(leebounds, "leebounds_", ""),
      ifelse(estimate == "PATE", "ITT_", ""),
      suffix,
      tolower(plat),
      ".tex"
    )
  )
  return(res_plat)
}

write_first_stage_table <- function(first_stage_reg_fb, first_stage_reg_ig) {
  #' Creates a LaTeX table of first stage regression results.
  #' Writes table to 'analysis/primary/output/tables/emotion_outcomes_first_stage.tex'.
  #'
  #' @param first_stage_reg_fb A fitted first stage regression object for Facebook.
  #' @param first_stage_reg_ig A fitted first stage regression object for Instagram.
  #'
  # extract estimates and create table
  out_fb <- summary(first_stage_reg_fb)
  out_ig <- summary(first_stage_reg_ig)
  coef_fb <- coef(out_fb)["treated", "Estimate"]
  coef_ig <- coef(out_ig)["treated", "Estimate"]
  se_fb <- coef(out_fb)["treated", "Std. Error"]
  se_ig <- coef(out_ig)["treated", "Std. Error"]
  tab <- tibble(
    ` ` = c("Facebook", "Instagram"),
    `Treatment effect` = c(coef_fb, coef_ig),
    `Standard error` = c(se_fb, se_ig)
  )
  # format table to LaTeX
  addtorow <- list(
    pos = list(-1),
    command = c(
      paste(
        "\\hline \n", "&",
        paste0("(", seq(1, ncol(tab) - 1), ")", collapse = " & "),
        "\\\\ \n"
      ))
  )
  tab <- xtable(tab, digits = 3, align = c("l", "l", rep("c", ncol(tab) - 1)))
  print(
    tab,
    add.to.row = addtorow,
    type = "latex",
    include.rownames = FALSE,
    sanitize.text.function = identity,
    file = "analysis/primary/output/tables/emotion_outcomes_first_stage.tex"
  )
}

make_combined_plot <- function(res_fb, res_ig) {
  #' Generates combined plot of primary outcomes for Facebook and Instagram.
  #' Saves plot to 'analysis/primary/output/figures/primary_outcomes.{pdf/png}'.
  #'
  #' @param res_fb A dataframe of treatment effects for all outcomes for Facebook.
  #' @param res_ig A dataframe of treatment effects for all outcomes for Instagram.
  #'
  # select only PLATE and ensure order of outcomes is consistent
  res_fb <- res_fb %>%
    filter(est == "PLATE") %>%
    select(-est) %>%
    mutate(dv_label = fct_relevel(dv_label, emotion_outcomes_order))
  res_ig <- res_ig %>% 
    filter(est == "PLATE") %>%
    select(-est) %>%
    mutate(dv_label = fct_relevel(dv_label, emotion_outcomes_order))
  # generate recruitment_platform_string-specific plots
  fb_po <- build_outcome_subplot(
    res = res_fb,
    y_lab = "Treatment effect\n(standard deviations)",
    y_lim_lo = -0.1,
    y_lim_hi = 0.1
  )
  ig_po <-  build_outcome_subplot(
    res = res_ig,
    y_lab = "Treatment effect\n(standard deviations)",
    y_lim_lo = -0.1,
    y_lim_hi = 0.1,
    no_labels = TRUE
  )
  add_subtitles <- function(plot, plat) {
    plot + labs(title = plat) + theme(plot.title = element_text(hjust = 0.5))
  }
  fb_po <- add_subtitles(fb_po, "Facebook")
  ig_po <- add_subtitles(ig_po, "Instagram")
  # combine plots together
  po <- plot_grid(
    plot_grid(fb_po, ncol = 1),
    plot_grid(ig_po, ncol = 1),
    ncol = 2,
    align = "v",
    rel_widths = c(0.8, 0.55)
  )
  save_plot(
    po = po,
    dir = "analysis/primary/output",
    fname = "primary_outcomes",
    width = 7,
    height = 2
  )
}

primary_scalars <- function(
    ate_table_fb, ate_table_ig,              # Primary Estimates
    ate_table_og_fb, ate_table_og_ig,        # Primary Estimates (OG units)
    ate_table_lb_fb, ate_table_lb_ig,        # Lee bounds estimates
    first_stage_reg_fb, first_stage_reg_ig   # First stage regressions
) {
  #' Generates scalars for primary outcomes from different specifications, specifically:
  #' LATE, LATE (original units), and Lee bounds, first stage regression results.
  #' Writes scalars to:
  #' - 'analysis/primary/output/scalars/primary_scalars.tex'
  #' - 'analysis/primary/output/scalars/primary_effects_percentile_scalars.tex'
  #'
  #' @param ate_table_fb A dataframe of LATEs for all primary outcomes for Facebook.
  #' @param ate_table_ig A dataframe of LATEs for all primary outcomes for Instagram.
  #' @param ate_table_og_fb A dataframe of LATEs in original units for all primary outcomes for Facebook.
  #' @param ate_table_og_ig A dataframe of LATEs in original units for all primary outcomes for Instagram.
  #' @param ate_table_lb_fb A dataframe of ITTs + Lee bounds for all primary outcomes for Facebook.
  #' @param ate_table_lb_ig A dataframe of ITTs + Lee bounds for all primary outcomes for Instagram.
  #' @param first_stage_reg_fb A regression object for the first stage for Facebook.
  #' @param first_stage_reg_ig A regression object for the first stage for Instagram.
  #'
  out_fb <- summary(first_stage_reg_fb)
  out_ig <- summary(first_stage_reg_ig)

  ate_table_lb <- rbind(ate_table_lb_fb, ate_table_lb_ig)
  ate_table_lb_neg <- ate_table_lb[ate_table_lb[["Lee LB"]] < 0, ]

  colnames <- c("Emotional_state", "Treatment effect", "Standard error", "$p$-value")

  colnames(ate_table_fb) <- colnames
  colnames(ate_table_ig) <- colnames
  colnames(ate_table_og_fb) <- colnames
  colnames(ate_table_og_ig) <- colnames

  scalars <- list(
    # 1. Emotional state index scalars
    # Facebook
    "IndexEffectFB" = ate_table_fb %>%
      filter(Emotional_state == "Emotional state index") %>%
      pull("Treatment effect"),
    "IndexStdErrorFB" = ate_table_fb %>%
      filter(Emotional_state == "Emotional state index") %>%
      pull("Standard error"),
    "IndexPValueFB" = ate_table_fb %>%
      filter(Emotional_state == "Emotional state index") %>%
      pull("$p$-value"),

    # Instagram
    "IndexEffectIG" = ate_table_ig %>%
      filter(Emotional_state == "Emotional state index") %>%
      pull("Treatment effect"),
    "IndexStdErrorIG" = ate_table_ig %>%
      filter(Emotional_state == "Emotional state index") %>%
      pull("Standard error"),
    "IndexPValueIG" = ate_table_ig %>%
      filter(Emotional_state == "Emotional state index") %>%
      pull("$p$-value"),

    # 2. Emotional state - happy scalars
    # Facebook
    "HappyEffectFB" = ate_table_fb %>%
      filter(Emotional_state == "Happy") %>%
      pull("Treatment effect"),
    "HappyStdErrorFB" = ate_table_fb %>%
      filter(Emotional_state == "Happy") %>%
      pull("Standard error"),
    "HappyPValueFB" = ate_table_fb %>%
      filter(Emotional_state == "Happy") %>%
      pull("$p$-value"),

    # Instagram
    "HappyEffectIG" = ate_table_ig %>%
      filter(Emotional_state == "Happy") %>%
      pull("Treatment effect"),
    "HappyStdErrorIG" = ate_table_ig %>%
      filter(Emotional_state == "Happy") %>%
      pull("Standard error"),
    "HappyPValueIG" = ate_table_ig %>%
      filter(Emotional_state == "Happy") %>%
      pull("$p$-value"),

    # 3. Emotional state - not depressed scalars
    # Facebook
    "DepressedEffectFB" = ate_table_fb %>%
      filter(Emotional_state == "Depressed x (-1)") %>%
      pull("Treatment effect"),
    "DepressedStdErrorFB" = ate_table_fb %>%
      filter(Emotional_state == "Depressed x (-1)") %>%
      pull("Standard error"),
    "DepressedPValueFB" = ate_table_fb %>%
      filter(Emotional_state == "Depressed x (-1)") %>%
      pull("$p$-value"),

    # Instagram
    "DepressedEffectIG" = ate_table_ig %>%
      filter(Emotional_state == "Depressed x (-1)") %>%
      pull("Treatment effect"),
    "DepressedStdErrorIG" = ate_table_ig %>%
      filter(Emotional_state == "Depressed x (-1)") %>%
      pull("Standard error"),
    "DepressedPValueIG" = ate_table_ig %>%
      filter(Emotional_state == "Depressed x (-1)") %>%
      pull("$p$-value"),

    # 4. Emotional state - not anxious scalars
    # Facebook
    "AnxiousEffectFB" = ate_table_fb %>%
      filter(Emotional_state == "Anxious x (-1)") %>%
      pull("Treatment effect"),
    "AnxiousStdErrorFB" = ate_table_fb %>%
      filter(Emotional_state == "Anxious x (-1)") %>%
      pull("Standard error"),
    "AnxiousPValueFB" = ate_table_fb %>%
      filter(Emotional_state == "Anxious x (-1)") %>%
      pull("$p$-value"),

    # Instagram
    "AnxiousEffectIG" = ate_table_ig %>%
      filter(Emotional_state == "Anxious x (-1)") %>%
      pull("Treatment effect"),
    "AnxiousStdErrorIG" = ate_table_ig %>%
      filter(Emotional_state == "Anxious x (-1)") %>%
      pull("Standard error"),
    "AnxiousPValueIG" = ate_table_ig %>%
      filter(Emotional_state == "Anxious x (-1)") %>%
      pull("$p$-value"),

    # 5. Emotional state - original units treatment effects
    # intended to be improvements so multiply negative outcomes by -1
    "OGHappyEffectFB" = ate_table_og_fb %>%
      filter(Emotional_state == "Happy") %>%
      pull("Treatment effect"),
    "OGDepressedEffectFB" = ate_table_og_fb %>%
      filter(Emotional_state == "Depressed") %>%
      pull("Treatment effect") * -1,
    "OGAnxiousEffectFB" = ate_table_og_fb %>%
      filter(Emotional_state == "Anxious") %>%
      pull("Treatment effect") * -1,

    "OGHappyEffectIG" = ate_table_og_ig %>%
      filter(Emotional_state == "Happy") %>%
      pull("Treatment effect"),
    "OGDepressedEffectIG" = ate_table_og_ig %>%
      filter(Emotional_state == "Depressed") %>%
      pull("Treatment effect") * -1,
    "OGAnxiousEffectIG" = ate_table_og_ig %>%
      filter(Emotional_state == "Anxious") %>%
      pull("Treatment effect") * -1,
    # 6. First stage results
    # Extract coefficients
    "FirstStageCoefFB" = coef(out_fb)["treated", "Estimate"],
    "FirstStageCoefIG" = coef(out_ig)["treated", "Estimate"],

    # Extract standard errors
    "FirstStageSEFB" = coef(out_fb)["treated", "Std. Error"],
    "FirstStageSEIG" = coef(out_ig)["treated", "Std. Error"],

    # 7. Lee bounds scalars
    "LeeBoundsNegLBMax" = ate_table_lb_neg[["Lee LB"]] %>% max() %>% abs(),
    "LeeBoundsNegLBMin" = ate_table_lb_neg[["Lee LB"]] %>% min() %>% abs()
  )
  scalars[["OGAverageEffect"]] <- (
    scalars[["OGHappyEffectFB"]] + scalars[["OGDepressedEffectFB"]] + scalars[["OGAnxiousEffectFB"]] + 
    scalars[["OGHappyEffectIG"]] + scalars[["OGDepressedEffectIG"]] + scalars[["OGAnxiousEffectIG"]] 
  ) / 6
  return(scalars)
}

calculate_percentile_normal_approx <- function(df, res_ate) {
  #' Calculate the percentile change in emotional state index using normal approximation.
  #'
  #' @param df A dataframe containing the main sample data.
  #' @param res_ate A dataframe containing the treatment effect estimates.
  #'
  #' @return A numeric value representing the percentile change in emotional state index.
  #'
  # calculate empirical mean and standard deviation
  mean_emot_post <- mean(df$emot_post, na.rm = TRUE)
  sd_emot_post <- sd(df$emot_post, na.rm = TRUE)
  emot_post_effect <- res_ate %>% filter(dv == "emot_post") %>% select(ate) %>% pull()
  # Calculate the new CDF value after applying the treatment effect under normal distribution
  percentile <- pnorm(
    mean_emot_post + emot_post_effect * sd_emot_post,
    mean = mean_emot_post,
    sd = sd_emot_post
  )
  return(percentile)
}

get_percentile_scalars <- function(scalars, df_fb, df_ig, res_ate_fb, res_ate_ig) {
  #' Generate scalars for the percentile change in emotional state index for Facebook and Instagram.
  #' Writes scalars to 'analysis/primary/output/scalars/primary_effects_percentile_scalars.tex'.
  #'
  #' @param df_fb A dataframe containing the main sample data for Facebook.
  #' @param df_ig A dataframe containing the main sample data for Instagram.
  #' @param res_ate_fb A dataframe containing the treatment effect estimates for Facebook.
  #' @param res_ate_ig A dataframe containing the treatment effect estimates for Instagram.
  #'
  percentile_fb <- calculate_percentile_normal_approx(df_fb, res_ate_fb)
  percentile_ig <- calculate_percentile_normal_approx(df_ig, res_ate_ig)
  scalars_pct <- list(
    "OGAverageEffectPct" = round(scalars[["OGAverageEffect"]] * 100, 1),
    "PercentileChangeEmotFB" = round(percentile_fb * 100, 1),
    "PercentileChangeEmotIG" = round(percentile_ig * 100, 1)
  )
  return(scalars_pct)
}

## Execute
main()
