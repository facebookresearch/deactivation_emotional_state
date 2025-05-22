# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

#########################################################
###   substitution - prepare_substitution_outputs.R   ###
#########################################################

source("lib/constants.R")
source("lib/outcome_dictionaries.R")
source("lib/prepare_outputs.R")
# NOTE: ggplot2, forcats loaded in source
devtools::load_all("~/alethea")

library(patchwork)
library(utils)
library(tibble)
library(stringr)

main <- function() {
  res_sub_fb <- readRDS("analysis/substitution/output/res_sub_fb.rds")
  res_sub_ig <- readRDS("analysis/substitution/output/res_sub_ig.rds")

  fb_control_app_average <- readRDS("analysis/substitution/output/fb_control_app_average.rds")
  ig_control_app_average <- readRDS("analysis/substitution/output/ig_control_app_average.rds")

  make_control_group_average_plot(fb_control_app_average, ig_control_app_average)

  make_app_substitution_plot(
    res_fb = res_sub_fb$res,
    res_ig = res_sub_ig$res,
    y_lim_lo = -40,
    y_lim_hi = 20,
    suffix = "_custom"
  )
  apps_scalars <- list()
  apps_scalars <- write_app_substitution_table(
    res = res_sub_fb$res,
    scalars = apps_scalars,
    plat = "FB",
    suffix = "_custom"
  )
  apps_scalars <- write_app_substitution_table(
    res = res_sub_ig$res,
    scalars = apps_scalars,
    plat = "IG",
    suffix = "_custom"
  )

  social_media_app_dict <- make_substitution_custom_apps_social_media_dict()
  social_media_app_names <- unlist(social_media_app_dict, use.names = FALSE)
  apps_scalars[["SubCustomAllSocialMediaEffectFB"]] <- res_sub_fb$res %>%
    filter(dv_label %in% social_media_app_names) %>%
    pull(ate) %>%
    sum()
  apps_scalars[["SubCustomAllSocialMediaEffectIG"]] <- res_sub_ig$res %>%
    filter(dv_label %in% social_media_app_names) %>%
    pull(ate) %>%
    sum()
  apps_scalars <- lapply(apps_scalars, abs)
  apps_scalars <- add_control_avg_scalars(
    control_avg_fb = fb_control_app_average,
    control_avg_ig = ig_control_app_average,
    scalars = apps_scalars
  )
  save_scalars_tex(
    scalar_list = apps_scalars,
    filename = "analysis/substitution/output/scalars/apps_scalars.tex",
    digits = 0
  )
}

make_app_substitution_plot <- function(res_fb, res_ig, y_lim_lo = -5, y_lim_hi = 5, suffix = "") {
  #' Generates combined app substitution plot for Facebook and Instagram.
  #' Saves plot as 'analysis/substitution/output/substitution_apps_*.pdf'
  #'
  #' @param res_fb Dataframe containing substitution effects for Facebook.
  #' @param res_ig Dataframe containing substitution effects for Instagram.
  #' @param y_lim_lo Lower bound for y-axis limits. Default is -5.
  #' @param y_lim_hi Upper bound for y-axis limits. Default is 5.
  #' @param suffix Suffix to add to file name. Default is empty string.
  #'
  # combine estimates into one dataframe
  res <- bind_rows(
    res_fb %>% mutate(subgroup = "FB"),
    res_ig %>% mutate(subgroup = "IG")
  ) %>%
    mutate(
      subgroup = factor(subgroup, levels = c("IG", "FB")),
      dv_label = fct_relevel(dv_label, unique(dv_label))
    ) %>%
    arrange(subgroup, dv_label)
  # generate plot and save
  po <- build_outcome_subplot(
    res = res,
    y_lab = "Treatment effect (minutes/day)",
    y_lim_lo = y_lim_lo,
    y_lim_hi = y_lim_hi,
    group_var = "subgroup"
  ) +
    labs(color = "Platform Deactivated") +
    scale_color_manual(
      values = c(navy, orange),
      breaks = c("FB", "IG"),
      labels = c("Facebook", "Instagram")
    )
  save_plot(
    po = po,
    dir = "analysis/substitution/output",
    fname = paste0("substitution_apps", suffix),
    width = 6.5,
    height = 4
  )
}

make_control_group_average_plot <- function(res_fb, res_ig) {
  #' Generates combined bar plot of control group averages for Facebook and Instagram.
  #' Saves plot as 'analysis/substitution/output/substitution_apps_custom_control_avg.pdf'.
  #'
  #' @param res_fb Dataframe containing control group averages for Facebook.
  #' @param res_ig Dataframe containing control group averages for Instagram.
  #'
  # create joint dataframe of control group averages
  custom_app_dict <- make_substitution_custom_apps_dict()
  r_fb <- as.data.frame(t(res_fb)) %>% rownames_to_column("app") %>% mutate(plat = "Facebook")
  r_ig <- as.data.frame(t(res_ig)) %>% rownames_to_column("app") %>% mutate(plat = "Instagram")
  dat <- bind_rows(r_fb, r_ig) %>%
    rename(time_spent = V1) %>%
    mutate(
        app = recode(app, !!!custom_app_dict),
        app = fct_relevel(app, unlist(custom_app_dict))
    )
  # generate plot and save
  po <- dat %>%
    filter(app != "time_spent_all_apps") %>%
    ggplot(aes(x = app, y = time_spent, fill = plat)) +
    geom_col(position = "dodge") +
    scale_fill_manual(
      labels = c("Facebook", "Instagram"),
      values = c(navy, orange),
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 10),
      axis.title.x = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.border = element_blank(),
      axis.text.y = element_text(hjust = 0.5)
    ) +
    labs(
      x = "Application(s)",
      y = "Average time spent (minutes per day)",
      fill = "Sample"
    )
  save_plot(
    po = po,
    dir = "analysis/substitution/output",
    fname = "substitution_apps_custom_control_avg",
    width = 6.5,
    height = 4
  )
}


write_app_substitution_table <- function(res, scalars, plat, suffix = "") {
  #' Creates table of substitution effects for Facebook and Instagram.
  #' Writes table to 'analysis/substitution/output/tables/substitution_apps_*.tex'.
  #' Also stores scalar values for substitution effects in scalars list.
  #'
  #' @param res Dataframe containing substitution effects for a recruitment_platform_string.
  #' @param scalars List to store scalar values for substitution effects.
  #' @param plat Character string indicating the recruitment_platform_string ("FB" or "IG").
  #' @param suffix Suffix to add to file name. Default is empty string.
  #'
  #' @return Updated scalars list with new scalar values added.
  tab <- xtable(res, digits = 3)
  print(
    tab,
    type = "latex",
    floating = FALSE,
    include.rownames = FALSE,
    sanitize.text.function = identity,
    file = paste0(
      "analysis/substitution/output/tables/",
      "substitution_apps_",
      tolower(plat),
      suffix,
      ".tex"
    )
  )
  # process scalars from table
  table_lab <- str_split(suffix, "_")[[1]] %>%
    str_to_title() %>%
    paste(collapse = "")
  table_lab <- paste0("Sub", table_lab)
  for (i in res$dv_label) {
    lab <- str_replace_all(i, " ", "") %>% str_replace_all("&", "")
    scalars[[paste0(table_lab, lab, "Effect", plat)]] <- res %>%
      filter(dv_label == i) %>%
      pull(ate)
  }
  return(scalars)
}


add_control_avg_scalars <- function(control_avg_fb, control_avg_ig, scalars) {
  #' Adds control average scalar values for substitution effects to scalars list.
  #'
  #' @param control_avg_fb Dataframe containing control average values for Facebook.
  #' @param control_avg_ig Dataframe containing control average values for Instagram.
  #' @param scalars List to store scalar values for substitution effects.
  #'
  #' @return Updated scalars list with new scalar values added.
  social_media_app_vars <- names(make_substitution_custom_apps_social_media_dict())
  scalars[["PlatUsageControlFB"]] <- control_avg_fb[1, "time_spent_facebook"]
  scalars[["PlatUsageControlIG"]] <- control_avg_ig[1, "time_spent_instagram"]
  scalars[["SocialMediaUsageControlFB"]] <- sum(control_avg_fb[1, social_media_app_vars])
  scalars[["SocialMediaUsageControlIG"]] <- sum(control_avg_ig[1, social_media_app_vars])
  return(scalars)
}


## Execute
main()
