# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

#################################################
###   heterogeneous - prepare_het_outputs.R   ###
#################################################

source("lib/constants.R")
source("lib/outcome_dictionaries.R")
source("lib/prepare_outputs.R")

library(purrr)
library(stringr)
library(tidyr)

library(cowplot)
library(ggpubr)
library(ggforce)
library(patchwork)

library(kableExtra)
library(xtable)
options(knitr.kable.NA = "---")
options(xtable.floating = FALSE)
options(xtable.timestamp = "")

devtools::load_all("~/alethea")
theme_set(theme_alethea())

main <- function() {
  res_fb_primary <- readRDS("analysis/heterogeneous/output/primary_HTEs_fb.rds")
  res_ig_primary <- readRDS("analysis/heterogeneous/output/primary_HTEs_ig.rds")
  res_fb_secondary <- readRDS("analysis/heterogeneous/output/secondary_HTEs_fb.rds")
  res_ig_secondary <- readRDS("analysis/heterogeneous/output/secondary_HTEs_ig.rds")
  primary_HTEs_comp_fb <- readRDS("analysis/heterogeneous/output/primary_HTEs_comp_fb.rds")
  primary_HTEs_comp_ig <- readRDS("analysis/heterogeneous/output/primary_HTEs_comp_ig.rds")

  # get moderators
  primary_mods <- make_primary_moderators()
  secondary_mods_fb <- make_secondary_moderators(plat = "FB")
  secondary_mods_ig <- make_secondary_moderators(plat = "IG")
  # Remove partyid moderator (per PAP we should only have partyid5)
  secondary_mods_fb$group$partyid <- NULL
  secondary_mods_ig$group$partyid <- NULL
  secondary_mods_fb$subgroup$partyid <- NULL
  secondary_mods_ig$subgroup$partyid <- NULL

  res_fb_primary <- res_fb_primary %>% bind_rows() %>% join_f_test(plat = "FB")
  res_ig_primary <- res_ig_primary %>% bind_rows() %>% join_f_test(plat = "IG")
  res_fb_secondary <- res_fb_secondary %>%
    bind_rows() %>%
    join_f_test(plat = "FB", dataset_string = "secondary")
  res_ig_secondary <- res_ig_secondary %>%
    bind_rows() %>%
    join_f_test(plat = "IG", dataset_string = "secondary")

  ### Moderator tables ###
  mod_fb_primary <- create_moderator_table(
    moderator_table = res_fb_primary,
    moderator_df = data.frame(
        group = names(primary_mods$group_fb),
        group_label = unlist(primary_mods$group_fb)
    ),
    subgroup_df = extract_dict_to_df(primary_mods$subgroup_fb)
  )
  mod_ig_primary <- create_moderator_table(
    moderator_table = res_ig_primary,
    moderator_df = data.frame(
        group = names(primary_mods$group_ig),
        group_label = unlist(primary_mods$group_ig)
    ),
    subgroup_df = extract_dict_to_df(primary_mods$subgroup_ig)
  )
  mod_fb_secondary <- create_moderator_table(
    moderator_table = res_fb_secondary,
    moderator_df = data.frame(
        group = names(secondary_mods_fb$group),
        group_label = unlist(secondary_mods_fb$group)
    ),
    subgroup_df = extract_dict_to_df(secondary_mods_fb$subgroup),
    primary = FALSE
  )
  mod_ig_secondary <- create_moderator_table(
    moderator_table = res_ig_secondary,
    moderator_df = data.frame(
        group = names(secondary_mods_ig$group),
        group_label = unlist(secondary_mods_ig$group)
    ),
    subgroup_df = extract_dict_to_df(secondary_mods_ig$subgroup),
    primary = FALSE
  )
  HTE_moderator_scalars(
    fb_primary = mod_fb_primary,
    ig_primary = mod_ig_primary,
    fb_secondary = mod_fb_secondary,
    ig_secondary = mod_ig_secondary
  )

  ### HTE Plots ###
  HTE_primary_outputs(
    res_fb = res_fb_primary,
    res_ig = res_ig_primary,
    primary_mod_dict = primary_mods
  )
  HTE_secondary_outputs(
    res_fb = res_fb_secondary,
    res_ig = res_ig_secondary,
    secondary_mod_dict_fb = secondary_mods_fb,
    secondary_mod_dict_ig = secondary_mods_ig
  )
  HTE_scalars(res_fb_primary, res_ig_primary)

  create_women_below_median_age_table(primary_HTEs_comp_fb, primary_HTEs_comp_ig)
}

join_f_test <- function(res, plat, dataset_string = "primary") {
  #' Joins on the F-test p-value from the saturated regression results to the HTE results.
  #'
  #' @param res A data frame containing the HTE estimates for each subgroup.
  #' @param plat A string indicating the recruitment_platform_string, either 'FB' or 'IG'.
  #' @param dataset_string A string indicating the dataset, either 'primary' or 'secondary'.
  #'
  #' @return A data frame containing the HTE estimates for each subgroup with the F-test p-value.
  #'
  # load saturated regression results
  dir_in <- "analysis/heterogeneous/output"
  sat_reg_path <- sprintf("%s/%s_HTEs_sat_%s.rds", dir_in, dataset_string, tolower(plat))
  res_sat <- readRDS(sat_reg_path) %>%
    bind_rows() %>%
    mutate(subgroup_string = subgroup) %>%
    select(subgroup_string, f_test_pval)
  if (dataset_string == "secondary") {
    res_sat <- res_sat %>%
      mutate(subgroup_string = case_when(
        subgroup_string == "gender2_0" ~ "gender2_notmale",
        subgroup_string == "gender2_1" ~ "gender2_male",
        TRUE ~ subgroup_string
    ))
  }
  # process HTE results and join with saturated regression results
  res <- res %>%
    mutate(
        recruitment_platform_string = plat,
        subgroup_string = paste0(group, "_", subgroup),
        subgroup_string = gsub(" ", "", subgroup_string)
    ) %>%
    select(recruitment_platform_string, everything()) %>%
    left_join(res_sat, by = join_by(subgroup_string))
  if (dataset_string == "secondary") {
    # Per the PAP, partyid5 should have "All Democrat" and "All Republican"
    # This corresponds to "D" and "R" from partyid
    res <- res %>%
      filter(!(
        group == "partyid5" &
        subgroup %in% c(
          "Independent",
          "Democrat or Lean Democrat",
          "Republican or Lean Republican"
        ))
      ) %>%
      mutate(group = case_when(group == "partyid" ~ "partyid5", TRUE ~ group)) %>%
      mutate(
        subgroup = case_when(
          subgroup == "I" ~ "Independent",
          subgroup == "D" ~ "All Democrat",
          subgroup == "R" ~ "All Republican",
          TRUE ~ subgroup
      ))
  }
  return(res)
}

HTE_primary_outputs <- function(res_fb, res_ig, primary_mod_dict) {
  #' Creates the primary HTE plots and saves them as a png and pdf file.
  #'
  #' @param res_fb A data frame containing the HTE estimates for each subgroup on Facebook.
  #' @param res_ig A data frame containing the HTE estimates for each subgroup on Instagram.
  #' @param primary_mod_dict A list of dicts with the group and subgroup labels for each moderator
  #'
  primary_fb_plot <- create_joint_HTEs_plot(
    res = res_fb %>% filter(dv == "emot_post"),
    group_labels_dict = primary_mod_dict$group_fb,
    subgroup_labels_dict = primary_mod_dict$subgroup_fb,
    xlim_lo = -0.25,
    xlim_hi = 0.25
  )
  primary_ig_plot <- create_joint_HTEs_plot(
    res = res_ig %>% filter(dv == "emot_post"),
    group_labels_dict = primary_mod_dict$group_ig,
    subgroup_labels_dict = primary_mod_dict$subgroup_ig,
    xlim_lo = -0.25,
    xlim_hi = 0.25
  )
  primary_plot <- combined_primary_plot(res_fb, res_ig, primary_mod_dict)
  save_plot(
      po = primary_fb_plot,
      dir = "analysis/heterogeneous/output",
      fname = "primary_HTEs_fb",
      width = 4.5,
      height = 5
  )
  save_plot(
      po = primary_ig_plot,
      dir = "analysis/heterogeneous/output",
      fname = "primary_HTEs_ig",
      width = 4.5,
      height = 5
  )
  save_plot(
      po = primary_plot,
      dir = "analysis/heterogeneous/output",
      fname = "primary_HTEs",
      width = 9,
      height = 5
  )
}

combined_primary_plot <- function(res_fb, res_ig, primary_mod_dict) {
  #' Creates the primary HTE plots and saves them as a png and pdf file.
  #'
  #' @param res_fb A data frame containing the HTE estimates for each subgroup on Facebook.
  #' @param res_ig A data frame containing the HTE estimates for each subgroup on Instagram.
  #' @param primary_mod_dict A list of dicts with the group and subgroup labels for each moderator
  #'
  # add combined subgroup labels for gender_age_above_med since differ by recruitment_platform_string
  primary_mod_dict$subgroup_fb$gender_age_above_med <- list(
      female_FALSE = "Female, aged 18-34 (FB) or 18-24 (IG)",
      female_TRUE = "Female, aged 35+ (FB) or 25+ (IG)",
      male_FALSE = "Male, aged 18-34 (FB) or 18-24 (IG)",
      male_TRUE = "Male, aged 35+ (FB) or 25+ (IG)"
  )
  primary_fb_plot <- create_joint_HTEs_plot(
    res = res_fb %>% filter(dv == "emot_post"),
    group_labels_dict = primary_mod_dict$group_fb,
    subgroup_labels_dict = primary_mod_dict$subgroup_fb,
    xlim_lo = -0.25,
    xlim_hi = 0.25
  )
  primary_ig_plot <- create_joint_HTEs_plot(
    res = res_ig %>% filter(dv == "emot_post"),
    group_labels_dict = primary_mod_dict$group_ig,
    subgroup_labels_dict = primary_mod_dict$subgroup_ig,
    xlim_lo = -0.25,
    xlim_hi = 0.25,
    no_ylabels = TRUE
  )
  add_subtitles <- function(plot, plat) {
      plot + labs(title = plat) + theme(plot.title = element_text(hjust = 0.5))
  }
  primary_fb_plot <- add_subtitles(primary_fb_plot, "Facebook")
  primary_ig_plot <- add_subtitles(primary_ig_plot, "Instagram")
  # add empty_plot because FB has 5 moderators and IG only 4
  empty_plot <- ggplot() + theme_void() + theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
  po <- plot_grid(
    plot_grid(primary_fb_plot, ncol = 1),
    plot_grid(primary_ig_plot, empty_plot, ncol = 1, rel_heights = c(1, 0.2)),
    ncol = 2,
    align = "v",
    rel_widths = c(0.8, 0.50)
  )
  return(po)
}

HTE_secondary_outputs <- function(res_fb, res_ig, secondary_mod_dict_fb, secondary_mod_dict_ig) {
  #' Creates the secondary HTE plots and saves them as a png and pdf file.
  #'
  #' @param res_fb A data frame containing the HTE estimates for each subgroup on Facebook.
  #' @param res_ig A data frame containing the HTE estimates for each subgroup on Instagram.
  #' @param secondary_mod_dict_fb A list of dicts with the group and subgroup labels for each
  #'   moderator on Facebook.
  #' @param secondary_mod_dict_ig A list of dicts with the group and subgroup labels for each
  #'   moderator on Instagram.
  #'
  res_all <- bind_rows(res_fb, res_ig) %>% arrange(recruitment_platform_string, subgroup, dv_label)
  secondary_fb_plot <- create_joint_HTEs_plot(
    res = res_fb %>% filter(dv == "emot_post"),
    group_labels_dict = secondary_mod_dict_fb$group,
    subgroup_labels_dict = secondary_mod_dict_fb$subgroup[
      names(secondary_mod_dict_fb$subgroup) != "partyid"
    ],
    xlim_lo = -0.25,
    xlim_hi = 0.25,
    ncols = 2
  )
  secondary_ig_plot <- create_joint_HTEs_plot(
    res = res_ig %>% filter(dv == "emot_post"),
    group_labels_dict = secondary_mod_dict_ig$group,
    subgroup_labels_dict = secondary_mod_dict_ig$subgroup[
      names(secondary_mod_dict_ig$subgroup) != "partyid"
    ],
    xlim_lo = -0.25,
    xlim_hi = 0.25,
    ncols = 2
  )
  save_plot(
    secondary_fb_plot,
    dir = "analysis/heterogeneous/output",
    fname = "secondary_HTEs_fb",
    width = 8,
    height = 6
  )
  save_plot(
    secondary_ig_plot,
    dir = "analysis/heterogeneous/output",
    fname = "secondary_HTEs_ig",
    width = 8,
    height = 6
  )
}

create_joint_HTEs_plot <- function(
    res,
    group_labels_dict,
    subgroup_labels_dict,
    ncols = 1,
    no_ylabels = FALSE,
    xlim_lo = NULL,
    xlim_hi = NULL
) {
  #' Creates a joint HTE plot for the primary and secondary datasets (Facebook and Instagram).
  #' The plot is saved as a png and pdf file.
  #'
  #' @param res A data frame containing the HTE estimates for each subgroup.
  #' @param group_labels_dict A list of dictionaries containing the group labels for each moderator.
  #' @param subgroup_labels_dict A list of dictionaries containing the subgroup labels for each moderator.
  #' @param ncols An integer indicating the number of columns for the facet wrap. Default is 1.
  #' @param no_ylabels A boolean indicating whether to remove the y-axis labels. Default is FALSE.
  #' @param xlim_lo A numeric indicating the lower limit for the x-axis. Default is NULL.
  #' @param xlim_hi A numeric indicating the upper limit for the x-axis. Default is NULL.
  #' 
  #' @return A ggplot object.
  color_scale <- function(var, n) {
    if (var[1] == "partyid5") {
      colors <- partyid_colors
    } else {
      colors <- rep(maroon, n)
    }
    return(colors)
  }
  # generate df storing all group-subgroups and their labels
  labels_df <- bind_rows(subgroup_labels_dict, .id = "group") %>%
    pivot_longer(-group, names_to = "subgroup", values_to = "subgroup_label") %>%
    filter(!is.na(subgroup_label)) %>%
    mutate(
      group_label = recode(group, !!!group_labels_dict),
      group_label = factor(group_label, levels = unique(unlist(group_labels_dict)))
    ) %>%
    filter(!is.na(group_label))
  subgroup_label_order <- unique(unlist(subgroup_labels_dict))
  # merge with labels, filter NAs, order by subgroup_label, and assign color
  res <- res %>%
    left_join(labels_df, by = c("group", "subgroup")) %>%
    mutate(subgroup_label = factor(subgroup_label, levels = rev(subgroup_label_order))) %>%
    filter(!is.na(group_label)) %>%
    group_by(group) %>%
    arrange(subgroup_label) %>%
    mutate(custom_color = color_scale(group, n())) %>%
    ungroup()
  # build the initial plot object
  po <- ggplot(res, aes(y = subgroup_label, x = ate, color = custom_color)) +
    geom_point(shape = 16, position = position_dodge(width = 0.5)) +
    geom_errorbarh(
      aes(xmin = ci_low, xmax = ci_hi, group = subgroup_label),
      height = 0.25,
      position = position_dodge(width = 0.5)
    ) +
    scale_color_identity()
  # facet wrap if multiple columns are desired
  if (ncols > 1) {
    po <- po + facet_wrap(vars(group_label), ncol = ncols, scales = "free_y")
  } else {
    po <- po + facet_col(vars(group_label), scales = "free_y", space = "free")
  }
  # define x label, and set global min and max values for confidence intervals
  xlabel <- if ("partyid5" %in% names(group_labels_dict)) {
    "Treatment effect (standard deviations)"
  } else {
    "Treatment effect\n(standard deviations)"
  }
  if (is.null(xlim_lo)) xlim_lo <- min(res$ci_low) - 0.02
  if (is.null(xlim_hi)) xlim_hi <- max(res$ci_hi) + 0.02
  # finalize plot object with theme settings
  po <- po + labs(y = NULL, x = xlabel) +
    theme_bw() +
    theme(
      text = element_text(size = 11),
      axis.title.x = element_text(size = 11),
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.border = element_blank(),
      strip.background = element_blank(),
      strip.placement = "outside",
      strip.text.x = element_text(size = 11, hjust = 0.48)
    ) +
    xlim(xlim_lo, xlim_hi) +
    geom_vline(xintercept = 0, color = "black") +
    theme(plot.margin = margin(0, 0, 0, 1, "cm"))
  if (no_ylabels) po <- po + theme(axis.text.y = element_blank())
  return(po)
}


create_moderator_table <- function(moderator_table, moderator_df, subgroup_df, primary = TRUE) {
  #' Creates moderator table for the HTE estimates with the F-test p-value.
  #' Writes moderator table as a LaTeX file.
  #'
  #' @param moderator_table A data frame containing the HTE estimates for each subgroup.
  #' @param moderator_df A data frame containing the group labels for each moderator.
  #' @param subgroup_df A data frame containing the subgroup labels for each moderator.
  #' @param primary A boolean indicating whether the table is for the primary or secondary dataset.
  #'
  #' @return A data frame containing the HTE estimates for each subgroup with the F-test p-value.
  #'
  # filter out partyid consistent with PAP
  subgroup_df <- subgroup_df %>% filter(group != "partyid")
  subgroup_df$subgroup_label <- factor(
    subgroup_df$subgroup_label, levels = unique(subgroup_df$subgroup_label)
  )
  # join on labels to table
  moderator_table <- moderator_table %>%
    left_join(moderator_df, by = "group") %>%
    left_join(subgroup_df, by = c("group", "subgroup")) %>%
    mutate(
      group_label = factor(group_label, levels = unique(moderator_df$group_label)),
      subgroup_label = factor(subgroup_label, levels = unique(subgroup_df$subgroup_label))
    ) %>%
    arrange(group_label, subgroup_label)
  # transform data into a list of moderator-specific dataframes
  df_list <- list()
  for (moderator in moderator_df$group_label) {
    df_list[[moderator]] <- moderator_table %>%
      filter(group_label == moderator) %>%
      select(subgroup_label, ate, std_error, pval, f_test_pval)
  }
  # returns data used for generating table
  df_tab <- create_moderator_latex_table(
    results = df_list,
    recruitment_platform_string = unique(moderator_table$recruitment_platform_string),
    primary = primary
  )
  return(df_tab)
}


create_moderator_latex_table <- function(results, recruitment_platform_string, primary = TRUE) {
  #' Creates a LaTeX table for the HTE estimates with the F-test p-value and saves as a LaTeX file.
  #'
  #' @param results A list of data frames containing the HTE estimates for each moderator.
  #' @param recruitment_platform_string A string indicating the recruitment_platform_string, either 'FB' or 'IG'.
  #' @param primary A boolean indicating whether the table is for the primary or secondary dataset. 
  #'    Default is TRUE.
  #'
  #' @return String version of the moderator LaTeX table.
  #'
  for (moderator in names(results)) {
    mod_res <- results[[moderator]]
    # move f_test_pval from column to its own row
    f_test_row <- data.frame(
      subgroup_label = "\\textit{F-test of equality}",
      ate = NA,
      std_error = NA,
      pval = unique(na.omit(mod_res$f_test_pval))
    )
    if ("f_test_pval" %in% names(mod_res)) {
      mod_res <- mod_res %>% select(-f_test_pval)
    }
    results[[moderator]] <- rbind(mod_res, f_test_row)
  }
  # join all moderator dataframes together into one table
  all_data <- bind_rows(lapply(names(results), function(moderator) {
    cbind(Moderator = moderator, results[[moderator]])
  }))
  # create the latex table
  col_names <- c("Subgroup", "Treatment effect", "Standard error", "$p$-value")
  latex_table <- kable(
    all_data[, -1],
    "latex",
    booktabs = TRUE,
    align = "lccc",
    col.names = col_names,
    escape = FALSE,
    digits = c(0, 3, 3, 3)
  ) %>%
    add_header_above(c(" " = 1, "(1)" = 1, "(2)" = 1, "(3)" = 1), line = FALSE) %>%
    kable_styling(latex_options = c("striped", "hold_position"), table.envir = "@empty")
  # add moderator labels to the table
  start_row <- 1
  for (moderator in names(results)) {
    n_rows <- nrow(results[[moderator]])
    latex_table <- pack_rows(latex_table, moderator, start_row, start_row + n_rows - 1)
    start_row <- start_row + n_rows
  }
  file_name <- sprintf(
    "analysis/heterogeneous/output/tables/moderator_table_%s%s.tex",
    if (primary) "primary_" else "secondary_",
    recruitment_platform_string
  )
  save_kable(latex_table, file = file_name)
  return(all_data)
}

extract_dict_to_df <- function(my_list) {
  #' Helper function to convert list to data frame.
  #'
  #' @param my_list A list of dictionaries.
  #'
  #' @return Dataframe version of the list.
  #'
  rows <- list()
  for (g in names(my_list)) {
    subgroups <- names(my_list[[g]])
    for (s in subgroups) {
      rows[[length(rows) + 1]] <- list(
        group = g, subgroup = s, subgroup_label = my_list[[g]][[s]]
      )
    }
  }
  df <- do.call(rbind, lapply(rows, function(x) data.frame(x, stringsAsFactors = FALSE)))
  return(df)
}


create_women_below_median_age_table <- function(comp_fb, comp_ig, digits = 3) {
  #' Creates a table comparing the different HTE estimates for women below median age across
  #' the different specifications. Saves the table as a LaTeX file.
  #'
  #' @param comp_fb A data frame containing the different HTE estimates for Facebook.
  #' @param comp_ig A data frame containing the different HTE estimates for Instagram.
  #' @param digits The number of digits to round the estimates to. Default is 3.
  #'
  vars <- c(
    "Female below-median age subsample",
    "Full sample with interaction",
    " + Deactivation x baseline use",
    " + Deactivation x baseline use quintiles",
    " + Deactivation x above-median baseline emotional state"
  )
  # helper function to format coefficients and standard errors
  get_column <- function(ests, digits) {
    coef_values <- lapply(
        ests[["Estimate"]], 
        function(x) formatC(x, format = "f", digits = digits)
    )
    se_values <- lapply(
        ests[["Std. Error"]],
        function(x) paste0("(", formatC(x, format = "f", digits = digits), ")")
    )
    col <- c(rbind(coef_values, se_values))
    return(col)
  }
  fb_column <- get_column(comp_fb, digits)
  ig_column <- get_column(comp_ig, digits)
  tab <- bind_cols(
    ` ` = unlist(map(vars, ~ c(.x, "")))[1:length(fb_column)],
    `Facebook` = unlist(fb_column),
    `Instagram` = unlist(ig_column)
  )
  tab <- xtable(tab, type = "latex", align = c("c", "l", "c", "c"))
  print(
    tab,
    include.rownames = FALSE,
    file = "analysis/heterogeneous/output/tables/HTE_young_women_comp.tex"
  )
}


HTE_scalars <- function(tab_fb, tab_ig) {
  #' Generates scalar values for the HTE estimates from the main primary FB and IG tables.
  #' Writes the scalar values to a LaTeX file.
  #'
  #' @param tab_fb A data frame containing the primary HTE estimates for Facebook.
  #' @param tab_ig A data frame containing the primary HTE estimates for Instagram.
  #'
  scalars <- list(
    # 1. Gender Age
    # Facebook
    "HTEFemaleAboveFB" = tab_fb %>%
        filter(group == "gender_age_above_med" &
            subgroup == "female_TRUE") %>%
        pull(ate),
    "HTEFemaleAbovePValueFB" = tab_fb %>%
        filter(group == "gender_age_above_med" &
            subgroup == "female_TRUE") %>%
        pull(pval),
    "HTEFemaleBelowFB" = tab_fb %>%
        filter(group == "gender_age_above_med" &
            subgroup == "female_FALSE") %>%
        pull(ate),
    "HTEFemaleBelowPValueFB" = tab_fb %>%
        filter(group == "gender_age_above_med" &
            subgroup == "female_FALSE") %>%
        pull(pval),
    "HTEMaleBelowFB" = tab_fb %>%
        filter(group == "gender_age_above_med" &
            subgroup == "male_FALSE") %>%
        pull(ate),
    "HTEMaleBelowPValueFB" = tab_fb %>%
        filter(group == "gender_age_above_med" &
            subgroup == "male_FALSE") %>%
        pull(pval),
    "HTEMaleAboveFB" = tab_fb %>%
        filter(group == "gender_age_above_med" &
            subgroup == "male_TRUE") %>%
        pull(ate),
    "HTEMaleAbovePValueFB" = tab_fb %>%
        filter(group == "gender_age_above_med" &
            subgroup == "male_TRUE") %>%
        pull(pval),

    # Instagram
    "HTEFemaleAboveIG" = tab_ig %>%
        filter(group == "gender_age_above_med" &
            subgroup == "female_TRUE") %>%
        pull(ate),
    "HTEFemaleAbovePValueIG" = tab_ig %>%
        filter(group == "gender_age_above_med" &
            subgroup == "female_TRUE") %>%
        pull(pval),
    "HTEFemaleBelowIG" = tab_ig %>%
        filter(group == "gender_age_above_med" &
            subgroup == "female_FALSE") %>%
        pull(ate),
    "HTEFemaleBelowPValueIG" = tab_ig %>%
        filter(group == "gender_age_above_med" &
            subgroup == "female_FALSE") %>%
        pull(pval),
    "HTEMaleBelowIG" = tab_ig %>%
        filter(group == "gender_age_above_med" &
            subgroup == "male_FALSE") %>%
        pull(ate),
    "HTEMaleBelowPValueIG" = tab_ig %>%
        filter(group == "gender_age_above_med" &
            subgroup == "male_FALSE") %>%
        pull(pval),
    "HTEMaleAboveIG" = tab_ig %>%
        filter(group == "gender_age_above_med" &
            subgroup == "male_TRUE") %>%
        pull(ate),
    "HTEMaleAbovePValueIG" = tab_ig %>%
        filter(group == "gender_age_above_med" &
            subgroup == "male_TRUE") %>%
        pull(pval),

    # 2. Baseline use
    # Facebook
    "HTEBaselineUseAboveFB" = tab_fb %>%
        filter(group == "baseline_use_above_med" &
            subgroup == "TRUE") %>%
        pull(ate),
    "HTEBaselineUseAbovePValueFB" = tab_fb %>%
        filter(group == "baseline_use_above_med" &
            subgroup == "TRUE") %>%
        pull(pval),
    "HTEBaselineUseBelowFB" = tab_fb %>%
        filter(group == "baseline_use_above_med" &
            subgroup == "FALSE") %>%
        pull(ate),
    "HTEBaselineUseBelowPValueFB" = tab_fb %>%
        filter(group == "baseline_use_above_med" &
            subgroup == "FALSE") %>%
        pull(pval),
    # Instagram
    "HTEBaselineUseAboveIG" = tab_ig %>%
        filter(group == "baseline_use_above_med" &
            subgroup == "TRUE") %>%
        pull(ate),
    "HTEBaselineUseAbovePValueIG" = tab_ig %>%
        filter(group == "baseline_use_above_med" &
            subgroup == "TRUE") %>%
        pull(pval),
    "HTEBaselineUseBelowIG" = tab_ig %>%
        filter(group == "baseline_use_above_med" &
            subgroup == "FALSE") %>%
        pull(ate),
    "HTEBaselineUseBelowPValueIG" = tab_ig %>%
        filter(group == "baseline_use_above_med" &
            subgroup == "FALSE") %>%
        pull(pval),

    # 3. Baseline emotional state
    # Facebook
    "HTEBaselineEmotionalStateAboveFB" = tab_fb %>%
        filter(group == "emot_pre_above_med" & subgroup == TRUE) %>%
        pull(ate),
    "HTEBaselineEmotionalStateAbovePValueFB" = tab_fb %>%
        filter(group == "emot_pre_above_med" & subgroup == TRUE) %>%
        pull(pval),
    "HTEBaselineEmotionalStateBelowFB" = tab_fb %>%
        filter(group == "emot_pre_above_med" & subgroup == FALSE) %>%
        pull(ate),
    "HTEBaselineEmotionalStateBelowPValueFB" = tab_fb %>%
        filter(group == "emot_pre_above_med" & subgroup == FALSE) %>%
        pull(pval),
    # Instagram
    "HTEBaselineEmotionalStateAboveIG" = tab_ig %>%
        filter(group == "emot_pre_above_med" & subgroup == TRUE) %>%
        pull(ate),
    "HTEBaselineEmotionalStateAbovePValueIG" = tab_ig %>%
        filter(group == "emot_pre_above_med" & subgroup == TRUE) %>%
        pull(pval),
    "HTEBaselineEmotionalStateBelowIG" = tab_ig %>%
        filter(group == "emot_pre_above_med" & subgroup == FALSE) %>%
        pull(ate),
    "HTEBaselineEmotionalStateBelowPValueIG" = tab_ig %>%
          filter(group == "emot_pre_above_med" & subgroup == FALSE) %>%
          pull(pval)
  )
  save_scalars_tex(
    scalars,
    filename = "analysis/heterogeneous/output/scalars/HTE_scalars.tex",
    digits = 3
  )
}


HTE_moderator_scalars <- function(fb_primary, ig_primary, fb_secondary, ig_secondary) {
  #' Generates scalar values for the HTE estimates from moderator tables.
  #' Writes the scalar values to a LaTeX file.
  #'
  #' @param fb_primary The moderator table for Facebook primary HTEs.
  #' @param ig_primary The moderator table for Instagram primary HTEs.
  #' @param fb_secondary The moderator table for Facebook secondary HTEs.
  #' @param ig_secondary The moderator table for Instagram secondary HTEs.
  #'
  extract_f_test_pval <- function(res, mod) {
    pval <- res[
        (res$Moderator == mod & str_detect(res$subgroup_label, "F-test")), "pval"
    ]
    return(pval)
  }
  moderator_scalars <- list(
    "HTEGenderAgePValueIG" = extract_f_test_pval(ig_primary, "Gender x age"),
    "HTEAgePValueFB" = extract_f_test_pval(fb_secondary, "Age"),
    "HTEUndecidedPValueFB" = extract_f_test_pval(fb_secondary, "Undecided voters"),
    "HTECollegePValueFB" = extract_f_test_pval(fb_secondary, "Education")
  )
  save_scalars_tex(
    moderator_scalars,
    filename = "analysis/heterogeneous/output/scalars/HTE_moderator_scalars.tex",
    digits = 3
  )
}


## Execute
main()
