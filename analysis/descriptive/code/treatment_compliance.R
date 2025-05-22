# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

################################################
###   descriptive - treatment_compliance.R   ###
################################################

library(dplyr)
library(ggplot2)
library(cowplot)
library(ggpubr)
library(lubridate)

devtools::load_all("~/alethea")
theme_set(theme_alethea())

source("lib/constants.R")
source("lib/prepare_outputs.R")

main <- function() {
  main_sample_ds_fb <- readRDS("analysis/descriptive/input/main_sample_ds_fb.rds")
  main_sample_ds_ig <- readRDS("analysis/descriptive/input/main_sample_ds_ig.rds")

  main_sample_ds_fb <- main_sample_ds_fb %>% mutate(ds = as.Date(strptime(ds, "%Y-%m-%d")))
  main_sample_ds_ig <- main_sample_ds_ig %>% mutate(ds = as.Date(strptime(ds, "%Y-%m-%d")))

  # Compliance (first stage figure)
  build_compliance_plot(
    main_sample_ds_fb,
    main_sample_ds_ig,
    metric = "used_plat",
    weights_var = "weights_w2"
  )
  compliance_scalars(main_sample_ds_fb, main_sample_ds_ig)
}


build_compliance_plot <- function(
  main_sample_ds_fb,
  main_sample_ds_ig,
  metric,
  weights_var = NULL
) {
  #' Builds plot showing average `metric` for each recruitment_platform_string over study period.
  #' When metric = `used_plat` illustrates treatment compliance (first stage figure).
  #'
  #' @param main_sample_ds_fb A data frame with main sample data for Facebook per day.
  #' @param main_sample_ds_ig A data frame with main sample data for Instagram per day.
  #' @param metric A character string indicating the metric to plot. Must be one of "ts", "vpv", or "used_plat".
  #' @param weights_var A character string indicating weights variable to use.
  #'
  compliance_fb <- build_compliance_subplot(
    main_sample_ds_fb,
    plat = "FB",
    metric = metric,
    weights_var = weights_var
  )
  compliance_ig <- build_compliance_subplot(
    main_sample_ds_ig,
    plat = "IG",
    metric = metric,
    weights_var = weights_var
  )
  legend <- get_legend(compliance_ig)
  compliance_plot <- ggarrange(
    compliance_fb,
    compliance_ig,
    nrow = 2,
    ncol = 1,
    legend = "bottom",
    legend.grob = legend,
    label.x = c(0, 0),
    hjust = -3.1,
    vjust = 1.4
  )
  save_plot(
    po = compliance_plot,
    dir = "analysis/descriptive/output",
    fname = paste0("compliance_", metric),
    width = 6.5,
    height = 8
  )
}


build_compliance_subplot <- function(users_ds, plat = "FB", metric = "ts", weights_var = NULL) {
  #' Helper function to build compliance subplot for a given recruitment_platform_string, see `build_compliance_plot`
  #'
  #' @param users_ds A data frame with main sample data for a recruitment_platform_string per day.
  #' @param plat A character string indicating the recruitment_platform_string. Must be one of "FB" or "IG".
  #' @param metric A character string indicating the metric to plot. Must be one of "ts", "vpv", or "used_plat".
  #' @param weights_var A character string indicating weights variable to use. Default is NULL.
  #'
  #' @return A ggplot object representing compliance subplot for given recruitment_platform_string.
  #'
  if (plat == "FB") {
    plat_name <- "Facebook"
    plat_ts_base_wtd <- "fb_ts_base_wtd_win"
  } else if (plat == "IG") {
    plat_name <- "Instagram"
    plat_ts_base_wtd <- "ig_ts_base_wtd_win"
  }
  if (is.null(weights_var)) {
    users_ds$weights_var <- 1
    weights_var <- "weights_var"
  }
  users_ds <- users_ds %>% filter(!!as.name(weights_var) > 0)
  compliance <- users_ds %>%
    group_by(experiment_group, ds) %>%
    summarize(
      mean_plat_ts_base_wtd_win = weighted.mean(
        !!as.name(plat_ts_base_wtd), !!as.name(weights_var), na.rm = TRUE
      ),
      mean_vpvs_win = weighted.mean(vpvs_per_day_win, !!as.name(weights_var), na.rm = TRUE),
      used_plat = weighted.mean(used_plat, !!as.name(weights_var), na.rm = TRUE)
    ) %>%
    mutate(experiment_group = ifelse(experiment_group == "test", "Deactivation", "Control"))

  if (metric == "ts") {
    outcome <- "mean_plat_ts_base_wtd_win"
    outcome_label <- sprintf("Daily time on %s\n(baseline average = 1)", plat_name)
  } else if (metric == "vpv") {
    outcome <- "mean_vpvs_win"
    outcome_label <- sprintf("Average content views\non %s", plat_name)
  } else if (metric == "used_plat") { 
    outcome <- "used_plat"
    outcome_label <- sprintf("Share of participants\nusing %s", plat_name)
  } else {
    stop("Invalid metric name; must be ts, vpv, or used_plat")
  }
  ctrl <- "Deactivation and\nControl groups\nboth deactivated"
  deact <- "Treatment period\n(only Deactivation\ngroup deactivated)"
  rect_df <- data.frame(
    xmin = c(
      as.Date(strptime(exp_dates$base_ctrl$start, "%Y-%m-%d")),
      as.Date(strptime("2020-09-30", "%Y-%m-%d"))
    ),
    xmax = c(
      as.Date(strptime(exp_dates$end$start, "%Y-%m-%d")),
      as.Date(strptime(exp_dates$end$end, "%Y-%m-%d"))
    ),
    ymin = c(-Inf, -Inf),
    ymax = c(Inf, Inf),
    grp = factor(c(ctrl, deact), levels = c(ctrl, deact))
  )
  min_date <- as.Date(strptime(exp_dates$base$start, "%Y-%m-%d"))
  max_date <- as.Date(strptime("2020-12-23", "%Y-%m-%d"))
  compliance_subplot <- ggplot(compliance %>% filter(ds >= min_date, ds <= max_date)) +
    geom_rect(
      data = rect_df,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        fill = grp
      ),
      alpha = 0.9
    ) +
    scale_fill_manual(name = "", values = c("grey86", "grey94")) +
    geom_line(aes(x = ds, y = get(outcome), color = experiment_group, group = experiment_group)) +
    geom_point(aes(
        x = ds,
        y = get(outcome),
        color = experiment_group,
        shape = experiment_group,
        group = experiment_group
      ),
      size = 1.5
    ) +
    guides(linetype = "none") +
    labs(x = "Date", y = outcome_label, color = "Condition") +
    scale_color_manual(name = "", values = c(grey, maroon)) +
    scale_shape_manual(name = "", values = c(15, 18)) +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b %e",
      limits = c(min_date, max_date)
    ) +
    theme(
      aspect.ratio = 1 / 2,
      axis.title = element_text(size = 13),
      axis.text = element_text(size = 12),
      legend.text = element_text(size = 11, margin = margin(t = 3, b = 3, unit = "pt"))
    ) +
    theme(legend.position = "bottom")

  if (metric == "used_plat") {
      compliance_subplot <- compliance_subplot +
          scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1))
  } else if (metric == "ts") {
      compliance_subplot <- compliance_subplot +
          scale_y_continuous(breaks = seq(0, 1.2, 0.2), limits = c(0, 1.2))
  }
  return(compliance_subplot)
}

compliance_scalars <- function(main_sample_ds_fb, main_sample_ds_ig) {
  #' Writes compliance scalars to a LaTeX file, indicating share complying at different stages.
  #'
  #' @param main_sample_ds_fb A data frame with main sample data for Facebook per day.
  #' @param main_sample_ds_ig A data frame with main sample data for Instagram per day.
  #'
  compliance_fb <- calc_compliance_scalar_plat(main_sample_ds_fb, plat = "FB")
  compliance_ig <- calc_compliance_scalar_plat(main_sample_ds_ig, plat = "IG")
  compliance_scalars <- list(
    "ComplianceControlPreFB" = compliance_fb[["control_pre"]],
    "ComplianceDeactPreFB" = compliance_fb[["deact_pre"]],
    "ComplianceControlOneWeekDeactFB" = compliance_fb[["control_1w"]],
    "ComplianceDeactOneWeekDeactFB" = compliance_fb[["deact_1w"]],
    "ComplianceControlFiveWeekDeactFB" = compliance_fb[["control_5w"]],
    "ComplianceDeactFiveWeekDeactFB" = compliance_fb[["deact_5w"]],

    "ComplianceControlPreIG" = compliance_ig[["control_pre"]],
    "ComplianceDeactPreIG" = compliance_ig[["deact_pre"]],
    "ComplianceControlOneWeekDeactIG" = compliance_ig[["control_1w"]],
    "ComplianceDeactOneWeekDeactIG" = compliance_ig[["deact_1w"]],
    "ComplianceControlFiveWeekDeactIG" = compliance_ig[["control_5w"]],
    "ComplianceDeactFiveWeekDeactIG" = compliance_ig[["deact_5w"]]
  )
  save_scalars_tex(
    compliance_scalars,
    filename = "analysis/descriptive/output/scalars/compliance_scalars.tex",
    digits = 0,
  )
}

calc_compliance_scalar_plat <- function(sample_ds, plat = "FB") {
  #' Computes compliance scalars for a given recruitment_platform_string at different stages of the study
  #' by treatment group.
  #'
  #' @param main_sample_ds_fb A data frame with main sample data for given recruitment_platform_string per day.
  #'
  if (plat == "FB") {
    first_day_of_full_deact <- "2020-09-25"
  } else if (plat == "IG") {
    first_day_of_full_deact <- "2020-09-24"
  }
  tab <- sample_ds %>%
    mutate(
      per = case_when(
        (ds >= ymd(exp_dates$base$start) & ds < ymd(exp_dates$base_ctrl$start))
          ~ "pre deactivation",
        # first day of full deact for FB: Sep 25
        (ds >= ymd(first_day_of_full_deact) & ds <= ymd(exp_dates$base_ctrl$end))
          ~ "one-week deactivation",
        (ds >= ymd(exp_dates$end$start) & ds <= ymd(exp_dates$end$end))
          ~ "five-week deactivation",
        TRUE ~ as.character(NA)
      )
    ) %>%
    filter(!is.na(per)) %>%
    group_by(experiment_group, per) %>%
    summarize(compliance = weighted.mean(used_plat, weights_w2, na.rm = TRUE)) %>% 
    rename(treat = experiment_group)
  control_pre <- tab$compliance[tab$treat == "control" & tab$per == "pre deactivation"]
  deact_pre   <- tab$compliance[tab$treat == "test" & tab$per == "pre deactivation"]
  control_1w  <- tab$compliance[tab$treat == "control" & tab$per == "one-week deactivation"]
  deact_1w    <- tab$compliance[tab$treat == "test" & tab$per == "one-week deactivation"]
  control_5w  <- tab$compliance[tab$treat == "control" & tab$per == "five-week deactivation"]
  deact_5w    <- tab$compliance[tab$treat == "test" & tab$per == "five-week deactivation"]
  res <- 100 * c(
    control_pre = control_pre,
    deact_pre = deact_pre,
    control_1w = control_1w,
    deact_1w = deact_1w,
    control_5w = control_5w,
    deact_5w = deact_5w
  )
  return(res)
}


## Execute
main()
