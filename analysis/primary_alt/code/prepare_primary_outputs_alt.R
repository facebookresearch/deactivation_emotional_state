# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

#######################################################
###   primary_alt - prepare_primary_outputs_alt.R   ###
#######################################################

library(patchwork)
library(ggpubr)
library(forcats)
library(xtable)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")

source("lib/prepare_outputs.R")
source("lib/constants.R")
source("lib/outcome_dictionaries.R")

devtools::load_all("~/alethea")
theme_set(theme_alethea())

lab <- list(
  "primary" = "Primary estimates",
  "unweighted" = "Unweighted",
  "c_endline_resp" = "Control for endline\nresponse date",
  "c_baseline" = "Control only for\nbaseline outcome",
  "c_response" = "Control for response day",
  "behaghel" = "Behaghel sample",
  "one_acct" = "Participants who report\nhaving only one account"
)
alt_weights_fb <- make_alt_weights_labels_dict(plat = "FB") # Note: labels are same for IG, FB

robustness_order <- c(
  lab$primary,
  lab$unweighted,
  lab$c_endline_resp,
  lab$behaghel
)
# replace Unweighted with Control only for baseline outcome
robustness_order_control <- robustness_order
robustness_order_control[2] <- lab$c_baseline

main <- function() {
  ## Load results from primary_alt analysis
  res_fb <- readRDS("analysis/primary_alt/output/res_fb.rds")
  res_ig <- readRDS("analysis/primary_alt/output/res_ig.rds")

  res_base_control_fb <- readRDS("analysis/primary_alt/output/res_base_control_fb.rds")
  res_base_control_ig <- readRDS("analysis/primary_alt/output/res_base_control_ig.rds")

  alt_primary_outcomes_fb <- readRDS("analysis/primary_alt/output/alt_primary_outcomes_fb.rds")
  alt_primary_outcomes_ig <- readRDS("analysis/primary_alt/output/alt_primary_outcomes_ig.rds")

  res_win_fb <- readRDS("analysis/primary_alt/output/res_win_fb.rds")
  res_win_ig <- readRDS("analysis/primary_alt/output/res_win_ig.rds")

  res_multiple_fb <- readRDS("analysis/primary_alt/output/res_multiple_fb.rds")
  res_multiple_ig <- readRDS("analysis/primary_alt/output/res_multiple_ig.rds")

  # add version to results
  res_fb <- res_fb %>% filter(est == "PLATE") %>% mutate(version = lab$primary)
  res_ig <- res_ig %>% filter(est == "PLATE") %>% mutate(version = lab$primary)
  res_base_control_fb <- res_base_control_fb %>% mutate(version = lab$c_baseline)
  res_base_control_ig <- res_base_control_ig %>% mutate(version = lab$c_baseline)

  primary_alt_figures(
    res_fb     = res_fb,
    res_alt_fb = alt_primary_outcomes_fb %>%
      filter(version != lab$unweighted) %>%
      bind_rows(res_base_control_fb),
    res_ig     = res_ig,
    res_alt_ig = alt_primary_outcomes_ig %>%
      filter(version != lab$unweighted) %>%
      bind_rows(res_base_control_ig),
    order      = robustness_order_control,
    labels     = robustness_order_control,
    colors     = c(maroon, grey, orange, navy, green),
    suffix     = "robustness_control",
    legend_spacing = 0.35
  )

  alt_weights <- unlist(alt_weights_fb, use.names = FALSE)
  alt_weights_labs <- str_replace_all(alt_weights, " / ", "\n")
  primary_alt_figures(
    res_fb      = res_fb,
    res_alt_fb  = res_win_fb,
    res_ig      = res_ig,
    res_alt_ig  = res_win_ig,
    order       = c(lab$unweighted, lab$primary, alt_weights),
    labels      = c(lab$unweighted, lab$primary, alt_weights_labs),
    colors      = c(
      "black", maroon, grey, orange, navy, "hotpink",
      "lightslateblue", "olivedrab", "purple", "salmon", "tan"
    ),
    suffix      = "weights_alt",
    long        = TRUE,
    legend_spacing = 0.5
  )

  primary_alt_figures(
    res_fb     = res_fb,
    res_alt_fb = res_multiple_fb %>% mutate(version = lab$one_acct),
    res_ig     = res_ig,
    res_alt_ig = res_multiple_ig %>% mutate(version = lab$one_acct),
    order      = c(lab$primary, lab$one_acct),
    labels     = c(lab$primary, lab$one_acct),
    colors     = c(maroon, grey),
    suffix     = "mult_accounts",
    legend_spacing = 0.35
  )
}


primary_alt_figures <- function(
  res_fb,
  res_alt_fb,
  res_ig,
  res_alt_ig,
  order = NULL,
  labels = NULL,
  colors = NULL,
  suffix = NULL,
  long = FALSE,
  legend_spacing = NULL
) {
  #' Creates primary outcomes figure for Facebook and Instagram comparing main estimates
  #' to alternative estimates.
  #' Saves the figure to 'analysis/primary_alt/output/primary_outcomes_{suffix}.pdf/png'.
  #'
  #' @param res_fb A dataframe of main treatment effects for all outcomes for Facebook.
  #' @param res_alt_fb A dataframe of alternative treatment effects for all outcomes for Facebook.
  #' @param res_ig A dataframe of main treatment effects for all outcomes for Instagram.
  #' @param res_alt_ig A dataframe of alternative treatment effects for all outcomes for Instagram.
  #' @param order A vector of strings indicating the order of the estimates in the plot.
  #' @param labels A vector of strings indicating the labels for the estimates in the plot.
  #' @param colors A vector of colors for the estimates in the plot.
  #' @param suffix A string to append to the output file name.
  #' @param long A boolean indicating whether the plot should be long format.
  #' @param legend_spacing A numeric value indicating the spacing between legend items.
  #'
  #' @return Combined plot of primary and alternative estimates for Facebook and Instagram.
  #'
  res_fb <- bind_rows(res_fb, res_alt_fb) %>%
    mutate(dv_label = fct_relevel(dv_label, emotion_outcomes_order))
  res_ig <- bind_rows(res_ig, res_alt_ig) %>% 
    mutate(dv_label = fct_relevel(dv_label, emotion_outcomes_order))

  fb_std <- build_primary_alt_subplot(
    res = res_fb,
    y_lab = "Treatment effect\n(standard deviations)",
    y_lim_lo = -0.1,
    y_lim_hi = 0.15,
    order = order,
    labels = labels,
    colors = colors,
    add_legend = FALSE,
    legend_spacing = legend_spacing
  )
  ig_std <- build_primary_alt_subplot(
    res = res_ig,
    y_lab = "Treatment effect\n(standard deviations)",
    y_lim_lo = -0.1,
    y_lim_hi = 0.15,
    order = order,
    labels = labels,
    colors = colors,
    add_legend = TRUE,
    legend_spacing = legend_spacing,
    no_labels = TRUE
  )
  po <- merge_primary_alt_subplots(fb_std, ig_std)

  if (long) {
    dim <- list(width = 6.5, height = 10)
  } else {
    dim <- list(width = 8, height = 6)
  }
  save_plot(
    po = po,
    dir = "analysis/primary_alt/output/",
    fname = paste0("primary_outcomes_", suffix),
    width = dim$width,
    height = dim$height
  )
  return(po)
}


build_primary_alt_subplot <- function(
  res,
  y_lab,
  y_lim = NULL,
  y_lim_lo = NULL,
  y_lim_hi = NULL,
  order = NULL,
  labels = NULL,
  colors = NULL,
  add_legend = FALSE,
  no_labels = FALSE,
  legend_spacing = NULL
) {
  #' Build recruitment_platform_string-specific primary and alternative estimates subplot.
  #'
  #' @param res A dataframe of all treatment effects for all outcomes for a recruitment_platform_string.
  #' @param y_lab A string indicating the y-axis label.
  #' @param y_lim A numeric value indicating the y-axis limits (if mirrored).
  #' @param y_lim_lo A numeric value indicating the lower y-axis limit.
  #' @param y_lim_hi A numeric value indicating the upper y-axis limit.
  #' @param order A vector of strings indicating the order of the estimates in the plot.
  #' @param labels A vector of strings indicating the labels for the estimates in the plot.
  #' @param colors A vector of colors for the estimates in the plot.
  #' @param add_legend A boolean indicating whether to add a legend to the plot.
  #' @param no_labels A boolean indicating whether to remove y-axis labels.
  #' @param legend_spacing A numeric value indicating the spacing between legend items.
  #'
  #' @return Combined plot of primary and alternative estimates for Facebook and Instagram.
  #'
  res <- res %>% mutate(version = fct_relevel(version, rev(order)))
  po <- build_outcome_subplot(
    res = res,
    y_lab = y_lab,
    y_lim = y_lim,
    y_lim_lo = y_lim_lo,
    y_lim_hi = y_lim_hi,
    group_var = "version",
    no_labels = no_labels
  ) +
    theme(
      legend.title = element_blank(),
      legend.position = ifelse(add_legend, "bottom", "none")
    )
  if (!is.null(order)) {
    po <- po + scale_color_manual(
      breaks = all_of(order),
      labels = all_of(labels),
      values = colors
    )
  }
  if (!is.null(legend_spacing)) {
    po <- po + theme(
      legend.title = element_blank(),
      legend.position = ifelse(add_legend, "bottom", "none"),
      legend.text = element_text(margin = margin(r = as.numeric(legend_spacing), unit = "cm"))
    )
  }
  if (no_labels) po <- po + theme(axis.text.y = element_blank())
  return(po)
}

merge_primary_alt_subplots <- function(fb_std, ig_std) {
  #' Merge Facebook and Instagram subplots
  #'
  #' @param fb_std Facebook subplot.
  #' @param ig_std Instagram subplot.
  #'
  #' @return Combined plot of primary and alternative estimates for Facebook and Instagram.
  #'
  # Add whitespace for figure labels
  fb_std <- fb_std + theme(plot.margin = margin(t = 20, r = 5, b = 5, l = 5, unit = "pt"))
  ig_std <- ig_std + theme(plot.margin = margin(t = 20, r = 5, b = 5, l = 5, unit = "pt"))
  legend <- get_legend(ig_std)
  merged_plots <- ggarrange(
    fb_std,
    ig_std,
    nrow = 1,
    ncol = 2,
    legend = "bottom",
    legend.grob = legend,
    widths = c(1, 0.6),
    labels = c("Facebook", "Instagram"),
    label.x = c(0.45, 0.22),
    font.label = list(size = 10)
  )
return(merged_plots)
}


## Execute
main()
