# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

#####################################################
###   comparison - prepare_comparison_outputs.R   ###
#####################################################

library(haven)
library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)

devtools::load_all("~/alethea")
theme_set(theme_alethea())

source("lib/constants.R")
source("lib/prepare_outputs.R")

main <- function() {
  res_fb <- readRDS("analysis/comparison/input/emotion_ATEs_fb.rds")
  res_ig <- readRDS("analysis/comparison/input/emotion_ATEs_ig.rds")

  # create comparison dataframe with estimates from other papers
  df <- create_comparison_df(res_fb, res_ig)

  # create a comparison plot of emotional state estimates vs. other papers
  prepare_comp_plot(df, -0.25, 0.75)
  compute_comparison_scalars(df)
}

create_comparison_df <- function(res_fb, res_ig) {
  #' Create a comparison dataframe with estimates from other papers.
  #' For each paper, calculates the index of all emotional staate outcomes,
  #' standardizes the estimates and the standard errors.
  #'
  #' @param res_fb A dataframe of this paper's Facebook primary results.
  #' @param res_ig A dataframe of this paper's Instagram primary results.
  #'
  #' @return A dataframe with a column for the paper name, the average treatment effect,
  #' and the standard error.
  #'
  # extract estimates from emotional state regressions
  fb_est <- extract_estimates(res_fb)
  ig_est <- extract_estimates(res_ig)
  # create indices for other papers in the literature
  wf_est <- create_wf_index()
  ax_est <- create_ax_index()
  mo_est <- create_mo_index()
  lam_est <- create_lambert_index()
  tro_est <- create_tromholt_index()
  hall_est <- create_hall_index()
  turel_est <- create_turel_index()
  # create dataframe
  ests <- list(fb_est, ig_est, wf_est, ax_est, mo_est, lam_est, tro_est, hall_est, turel_est)
  paper_labels <- c(
    "This paper (Facebook)\n (N = 17,802)" # ES Facebook
    , "This paper (Instagram)\n (N = 13,480)" # ES Facebook
    , "Allcott et al. (2020)\n (N = 1,637)" # Welfare Effects
    , "Arceneaux et al. (2023)\n (N = 1,955)" # Arceneaux
    , "Mosquera et al. (2019)\n (N = 151)" # Mosquera
    , "Lambert et al. (2022)\n (N = 140)" # Lambert
    , "Tromholt (2016)\n (N = 888)" # Tromholt
    , "Hall et al. (2019)\n (N = 130)" # Hall
    , "Turel et al. (2018)\n (N = 555)" # Turel
  )
  df <- tibble(
    paper = paper_labels,
    ate = unlist(map(ests, function(x) x$pe)),
    se = unlist(map(ests, function(x) x$pe_sd))
  )
  return(df)
}

extract_estimates <- function(res) {
  #' Helper function for `create_comparison_df`. Extracts ATE and standard error
  #' from an emotional state regression dataframe.
  #'
  #' @param res A dataframe of this paper's emotional state regression results.
  #'
  #' @return A list with the ATE and standard error.
  #'
  pe <- res %>% filter(dv == "emot_post") %>% pull(ate)
  pe_sd <- res %>% filter(dv == "emot_post") %>% pull(std_error)
  return(list(pe = pe, pe_sd = pe_sd))
}


create_wf_index <- function() {
  #' Gets estmiates from ABEG, Welfare Effects paper.
  #' See Table A12, pg 23 from online appendix.
  #'
  #' @return A list with the ATE and standard error.
  #'
  wf_ests <- list(pe = 0.09, pe_sd = 0.04)
  return(wf_ests)
}


create_mo_index <- function() {
  #' Get estimates from Mosquera et al. (2019).
  #' Mosquera reports unstandardized estimates for five outcomes; estimates and standard errors
  #' need to be inferred from plot; standard deviations are from Appendix.
  #'
  #' @return A list with the ATE and standard error.
  #'
  # (x -1) indicates we have multiplied by -1 compared to OG paper
  mo_raw <- list(
    # Overall Satisfaction
    sat = list(pe = -0.005, pe_sd = 0.214, sd = 1.92),
    # Life worthwhile
    wwl = list(pe = 0.045, pe_sd = 0.235, sd = 1.88),
    # Feel happy
    hap = list(pe = 0.035, pe_sd = 0.25, sd = 2.12),
    # Worry (x -1)
    wor = list(pe = -0.36, pe_sd = 0.245, sd = 2.33),
    # Depressed (x -1)
    dep = list(pe = 0.57, pe_sd = 0.230, sd = 2.63)
  )
  mo_stan <- standardize_index_no_cov(mo_raw)
  return(mo_stan)
}


create_ax_index <- function() {
  #' Get estimates from Arceneaux et al. (2023).
  #' Arceneaux reports unstandardized estimates for all outcomes at endline in Appendix;
  #' standard deviations comes from summary statistics tables in Appendix.
  #'
  #' @return A list with the ATE and standard error.
  #'
  # (x -1) indicates we have multiplied by -1 compared to original paper
  ax_raw <- list(
    # Life satisfaction
    lsat = list(pe = 0.109, pe_sd = 0.072, sd = 2),
    # Joy
    joy  = list(pe = 0.319, pe_sd = 0.105, sd = 2.2),
    # Fulfilled
    ful  = list(pe = 0.248, pe_sd = 0.112, sd = 2.4),
    # Anxeity  (x -1)
    anx  = list(pe = 0.319, pe_sd = 0.138, sd = 3),
    # Bored  (x -1)
    bor  = list(pe = 0.204, pe_sd = 0.135, sd = 2.9),
    # Loneliness  (x -1)
    lon  = list(pe = 0.293, pe_sd = 0.135, sd = 3),
    # Depressed  (x -1)
    dep  = list(pe = 0.224, pe_sd = 0.124, sd = 2.9),
    # Isolated  (x -1)
    isol = list(pe = 0.155, pe_sd = 0.133, sd = 3.2)
  )
  ax_stan <- standardize_index_no_cov(ax_raw)
  return(ax_stan)
}


create_lambert_index <- function() {
  #' Get estimates from Lambert et al. (2022).
  #' Lambert reports unstandardized point estimate for all outcomes in Table 2. To calculate
  #' outcome standard deviation, we use the two group means and standard deviations in Table 2
  #' to calculate the full sample standard deviation.
  #'
  #' @return A list with the ATE and standard error.
  #'
  lam_raw <- list(
    # wellbeing
    web = list(pe = 4.90, pe_sd = 0.985, sd = 9.53),
    # depression (multiply by - 1)
    dep = list(pe = 2.17, pe_sd = 0.566, sd = 4.281),
    # anxiety (multiply by - 1)
    anx = list(pe = 1.68, pe_sd = 0.566, sd = 4.178)
  )
  lam_stan <- standardize_index_no_cov(lam_raw)
  return(lam_stan)
}


create_tromholt_index <- function() {
  #' Get estimates from Tromholt (2016).
  #' Tromholt reports unstandardized point estimates in Table 1. See text on pg. 664 to calculate
  #' treatment effect standard error from t-stats. Table 1 reports treatment and control group 
  #' means, standard deviations to calculate full sample outcome standard deviation.
  #'
  #' @return A list with the ATE and standard error.
  #'
  tro_raw <- list(
    # wellbeing
    lsat = list(pe = 0.37, pe_sd = 0.0918, sd = 1.329),
    # emotions
    emo = list(pe = 1.22, pe_sd = 0.244, sd = 6.491)
  )
  tro_stan <- standardize_index_no_cov(tro_raw)
  return(tro_stan)
}


create_hall_index <- function() {
  #' Get estimates from Hall et al. (2019).
  #' Hall reports unstandardized point estimates in Table 1. See description on pg 266 to calculate
  #' outcome standard error (from treatment, control group means and standard deviations).
  #'
  #' @return A list with the ATE and standard error.
  #'
  hall_raw <- list(
    lon = list(pe = 0.028, pe_sd = 0.050, sd = 1.217),
    awe = list(pe = 0.082, pe_sd = 0.054, sd = 1.305),
    qod = list(pe = 0.052, pe_sd = 0.056, sd = 1.315)
  )
  hall_stan <- standardize_index_no_cov(hall_raw)
  return(hall_stan)
}


create_turel_index <- function() {
  #' Get estimates from Turel et al. (2018).
  #' See Table 2 for point estimate. Effect standard error and outcome standard deviation
  #' from correspondence with Turel.
  #'
  #' @return A list with the ATE and standard error.
  #'
  pe <- 0.72 - 0.50
  pe_sd <- sqrt((0.089)^2 + (0.052)^2)
  sd <- 1.31
  return(list(pe = pe / sd, pe_sd = pe_sd / sd))
}


standardize_index_no_cov <- function(raw_ests) {
  #' Constructs comparable index in standardized units to compare across papers.
  #' Standardizes input of individual raw estimates, creates index, and calculates
  #' index standard errors.
  #' Following standard meta-analysis methods, we assume independence of estimates,
  #' and take unweighted average of estimates and variances. (295 of meta analysis book @TODO)
  #'
  #' @param raw_ests A list of raw estimates (in original units).
  #'
  # standardize estimates
  stan_ests <- map(raw_ests, function(x) {
    pe <- x$pe / x$sd
    pe_sd <- x$pe_sd / x$sd
    list(pe = pe, pe_sd = pe_sd)
  })
  # calculate index point estimate
  stan_ind_pe <- mean(unlist(map(stan_ests, function(x) x$pe)))
  #  calculate index variance
  stan_ind_pe_sd <- unlist(map(stan_ests, function(x) x$pe_sd))^2
  stan_ind_pe_var <- mean(stan_ind_pe_sd)
  stan_ind_pe_sd <- sqrt(stan_ind_pe_var)
  return(list(pe = stan_ind_pe, pe_sd = stan_ind_pe_sd))
}


prepare_comp_plot <- function(df, x_lim_lo, x_lim_hi) {
  #' Prepares a plot comparing our emotional state estimates vs. other papers.
  #' Saves plot to 'analysis/comparison/output/comparison_figure.png/pdf'.
  #'
  #' @param df A dataframe with paper, ATE, and standard error.
  #' @param x_lim_lo The lower limit of the x-axis.
  #' @param x_lim_hi The upper limit of the x-axis.
  #'
  df <- df %>%
    mutate(ci_hi = ate + 1.96 * se, ci_low = ate - 1.96 * se) %>%
    arrange(ate) %>%
    mutate(this_paper = as.factor(ifelse(str_detect(paper, "This paper"), "1", "0")))

  po <- df %>%
    ggplot(aes(y = paper, x = ate, color = this_paper)) +
    geom_vline(xintercept = 0, alpha = 0.5, color = grey, linetype = "dashed") +
    geom_point(shape = 16, position = position_dodge(width = 0.5)) +
    geom_errorbar(
      aes(xmin = ci_low, xmax = ci_hi),
      width = 0.25, position = position_dodge(width = 0.5)
    ) +
    scale_y_discrete(limits = unique(df$paper)) +
    scale_color_manual(values = c("1" = maroon, "0" = grey), guide = "none") +
    theme_minimal() +
    theme(
      text = element_text(size = 10),
      axis.title.x = element_text(size = 10),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.border = element_blank(),
      axis.text.y = element_text(hjust = 0.5)
    ) +
    xlim(x_lim_lo, x_lim_hi) +
    labs(
      x = "Treatment effect\n(standard deviations)",
      y = "",
      color = "Estimate"
    )
  save_plot(
    po,
    dir = "analysis/comparison/output/",
    fname = "comparison_figure",
    width = 6,
    height = 3
  )
}


compute_comparison_scalars <- function(df) {
  #' Computes scalars for the comparison estimates.
  #' Writes scalars to 'comparison_scalars.tex'.
  #'
  #' @param df A dataframe with paper, ATE, and standard error.
  #'
  # calculate average CI width for selected papers (removing this paper estimates)
  lit_avg_se <- df %>%
    filter(!str_detect(paper, "This paper")) %>%
    summarise(se = mean(se)) %>%
    pull()
  lit_avg_ci_width <- 1.96 * 2 * lit_avg_se

  # calculate CI width for this paper's Facebook estimate
  es_fb_se <- df %>% filter(str_detect(paper, "This paper \\(Facebook")) %>% pull(se)
  es_fb_ci_width <- 1.96 * 2 * es_fb_se

  # pull ATEs for Mosquera and Lambert
  mosquera_ate <- df %>% filter(str_detect(paper, "Mosquera")) %>% pull(ate)
  lambert_ate <- df %>% filter(str_detect(paper, "Lambert")) %>% pull(ate)

  comparison_scalars <- list(
    "MosqueraEstimate" = mosquera_ate,
    "LambertEstimate" = lambert_ate,
    "AverageCIWidthLiterature" = lit_avg_ci_width,
    "RelativeSizeCIWidth" = lit_avg_ci_width / es_fb_ci_width
  )
  comparison_scalars_dig <- c(2, 2, 2, 2)

  # extract sample size for Mosquera and Lambert
  mosquera_n <- df %>%
    filter(str_detect(paper, "Mosquera")) %>%
    pull(paper) %>%
    str_extract("(?<=N = )\\d+") %>%
    as.numeric()
  lambert_n <- df %>%
    filter(str_detect(paper, "Lambert")) %>%
    pull(paper) %>%
    str_extract("(?<=N = )\\d+") %>%
    as.numeric()

  # calculate min, max of two emotional state estimates in this paper
  es_fb_ate <- df %>% filter(str_detect(paper, "This paper \\(Facebook")) %>% pull(ate)
  es_ig_ate <- df %>% filter(str_detect(paper, "This paper \\(Instagram")) %>% pull(ate)
  es_min_ate <- min(es_fb_ate, es_ig_ate)
  es_max_ate <- max(es_fb_ate, es_ig_ate)

  comparison_scalars_int <- list(
    "MosqueraSample" = mosquera_n,
    "LambertSample" = lambert_n,
    "AvgPsychInterventionLB" = 100 * es_min_ate / 0.27,
    "AvgPsychInterventionUB" = 100 * es_max_ate / 0.27
  )
  comparison_scalars_int_dig <- c(0, 0, 0, 0)
  save_scalars_tex(
    scalar_list = c(comparison_scalars, comparison_scalars_int),
    filename = "analysis/comparison/output/scalars/comparison_scalars.tex",
    digits = c(comparison_scalars_dig, comparison_scalars_int_dig)
  )
}


## Execute
main()
