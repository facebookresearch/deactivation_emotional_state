# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

######################################################
###   comparison - prepare_demographic_outputs.R   ###
######################################################

source("lib/compute_effects.R")
source("lib/prepare_outputs.R")
source("lib/constants.R")
source("lib/outcome_dictionaries.R")

library(gridExtra)
library(cowplot)
library(xtable)

# Define the y-axis vars
y_levels <- rev(
  c(
    "College degree", "No college degree",
    "Lower income", "Upper and middle income",
    "Age 18-34", "Age 35+",
    "Minority", "Non-minority",
    "Male", "Female"
  )
)

main <- function() {
  harmonized_main_sample <- readRDS("analysis/comparison/input/harmonized_NSDUH_main_sample.rds")
  harmonized_NSDUH_2020 <- readRDS("analysis/comparison/input/harmonized_NSDUH_2020.rds")

  # factor moderators
  harmonized_main_sample_fct <- factor_moderators(harmonized_main_sample)
  harmonized_NSDUH_2020_fct <- factor_moderators(harmonized_NSDUH_2020)

  # create demographic table comparing the two samples
  create_demographic_table(harmonized_main_sample_fct, harmonized_NSDUH_2020_fct)

  # create demographic plots comparing the two samples
  create_demographic_plots(harmonized_main_sample, harmonized_NSDUH_2020)
}

create_demographic_table <- function(main_sample, NSDUH_2020) {
  #' Creates a table comparing regression of outcomes on controls in main sample and NSDUH.
  #'
  #' @param main_sample df A data frame containing the harmonized main sample.
  #' @param NSDUH_2020 df A data frame containing the harmonized NSDUH 2020 sample.
  #'
  demographic_vars <- c(
    "participant_has_degree",
    "income_bins",
    "age_bins",
    "minority",
    "participant_male_or_not"
  )
  demographic_vars_str <- paste0(demographic_vars, collapse = " + ")
  main_sample_reg <- lm(
    as.formula(paste0("emot_pre ~ ", demographic_vars_str)),
    data = main_sample,
    weights = main_sample[["weights_w2"]]
  )
  NSDUH_reg <- lm(
    as.formula(paste0("k6_avg ~ ", demographic_vars_str)),
    data = NSDUH_2020
  )
  control_effects_table(main_sample_reg, NSDUH_reg)
}

create_demographic_plots <- function(main_sample, NSDUH_2020) {
  #' Generates binned summary plots for demographic subgroups from main
  #' sample and the NSDUH 2020 sample.
  #' Saves combined plot to "analysis/comparison/output/pngs/comparison_mods.png/pdf".
  #'
  #' @param main_sample A data frame representing the main sample.
  #' @param NSDUH_2020 A data frame representing the NSDUH 2020 sample.
  #'
  # Define the demographic grouping variables and outcome variables for each dataset
  group_vars <- c("participant_has_degree", "income_bins", "age_bins", "minority", "participant_male_or_not")
  outcome_main <- "emot_pre"
  outcome_nsduh <- "k6_avg"
  # Define extra plot parameters for each grouping variable
  extra_params_main <- list(
    participant_has_degree = list(plot_title = "(a) Experimental sample"),
    income_bins       = list(),
    age_bins          = list(),
    minority          = list(),
    participant_male_or_not         = list(x_axis_label = "Emotional state index\n(standard deviations)")
  )
  extra_params_nsduh <- list(
    participant_has_degree = list(plot_title = "(b) NSDUH 2020 sample", remove_y_labels = TRUE),
    income_bins       = list(remove_y_labels = TRUE),
    age_bins          = list(remove_y_labels = TRUE),
    minority          = list(remove_y_labels = TRUE),
    participant_male_or_not         = list(
      remove_y_labels = TRUE,
      x_axis_label = "Kessler-6 score\n(standard deviations)"
    )
  )
  # helper function to calculate summary for a grouping variable and creates a plot
  get_plot <- function(data, group_var, outcome, extra_params) {
    summary_df <- calculate_summary(
      data = data,
      grouping_var = group_var,
      outcome_var = outcome
    )
    args <- c(list(summary_df, y_levels), extra_params)
    do.call(create_demographic_plot, args)
  }
  # Generate plots for each grouping variable for both samples.
  main_plots <- lapply(group_vars, function(var) {
    get_plot(main_sample, var, outcome_main, extra_params_main[[var]])
  })
  nsduh_plots <- lapply(group_vars, function(var) {
    get_plot(NSDUH_2020, var, outcome_nsduh, extra_params_nsduh[[var]])
  })
  main_sample_plot <- plot_grid(plotlist = main_plots, ncol = 1, align = "v")
  NSDUH_plot <- plot_grid(plotlist = nsduh_plots, ncol = 1, align = "v")
  comparison_plot <- plot_grid(
    main_sample_plot,
    NSDUH_plot,
    ncol = 2,
    rel_widths = c(2, 1.75)
  )
  save_plot(
    comparison_plot,
    dir = "analysis/comparison/output/",
    fname = "comparison_mods",
    width = 8,
    height = 8
  )
}

calculate_summary <- function(data, grouping_var, outcome_var) {
  #' Helper function for `create_demographic_outputs.R` that calculates mean and standard error
  #' of a given outcome variable given a grouping variable.
  #'
  #' @param data A data frame containing the data.
  #' @param grouping_var A string representing the grouping variable.
  #' @param outcome_var A string representing the outcome variable.
  #'
  #' @return A data frame containing the mean and standard error of the outcome variable for each group.
  data <- data %>%
    group_by(!!sym(grouping_var)) %>%
    summarise(
      mean_index_avg = mean(!!sym(outcome_var), na.rm = TRUE),
      se_index_avg = sd(!!sym(outcome_var), na.rm = TRUE) / sqrt(n())
    ) %>%
    mutate(
      group = !!sym(grouping_var),
      ci_hi = mean_index_avg + 1.96 * se_index_avg,
      ci_low = mean_index_avg - 1.96 * se_index_avg
    ) %>%
    ungroup()
  return(data)
}


create_demographic_plot <- function(
    df,
    y_levels,
    x_axis_label = NULL,
    plot_title = "",
    remove_y_labels = FALSE
) {
  #' Helper function for `create_demographic_outputs.R` that plots given outcome variable by
  #' a given grouping variable.
  #'
  #' @param df A data frame containing the aggregated sample data.
  #' @param y_levels A vector of levels for the y-axis.
  #' @param x_axis_label A string representing the x-axis label. Default is NULL.
  #' @param plot_title A string representing the plot title. Default is "".
  #' @param remove_y_labels A boolean indicating whether to remove y-axis labels. Default is FALSE.
  #'
  #' @return A ggplot object showing mean outcome by group with error bars.
  #'
  df <- df %>% na.omit() %>% mutate(group = factor(group, levels = y_levels))
  # create plot 
  colors <- c(maroon, grey, navy)
  p <- ggplot(df, aes(y = group, x = mean_index_avg, color = as.factor(group))) +
    geom_vline(xintercept = 0, alpha = 0.5, color = "#808080", linetype = "dashed") +
    geom_point(shape = 16, position = position_dodge(width = 0.5)) +
    geom_errorbar(
      aes(xmin = ci_low, xmax = ci_hi),
      width = 0.25, position = position_dodge(width = 0.5)
    ) +
    scale_color_manual(
      values = rev(colors[1 : length(unique(df$group))]),
      guide = "none"
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 10),
      axis.title.x = element_text(size = 10, margin = margin(t = 10, r = 0, b = 0, l = 0)),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.border = element_blank(),
    ) +
    xlim(-.4, .4) +
    labs(title = plot_title) +
    theme(plot.title = element_text(hjust = 0.5, size = 10))
  if (is.null(x_axis_label)) {
    p <- p + labs(x = "", y = "")
  } else {
    p <- p + labs(x = x_axis_label, y = "")
  }
  if (remove_y_labels) {
    p <- p + theme(axis.text.y = element_blank())
  }
  return(p)
}

factor_moderators <- function(data) {
  #' Helper function that converts the demographic moderators to factors.
  #'
  #' @param data A data frame containing the data.
  #'
  #' @return An updated data frame with the demographic moderators converted to factors.
  #'
  data <- data %>%
    mutate(
      participant_has_degree = factor(
        participant_has_degree,
        levels = c("No college degree", "College degree")
      ),
      age_bins = factor(age_bins, levels = c("Age 18-34", "Age 35+")),
      participant_male_or_not = factor(participant_male_or_not, levels = c("Male", "Female")),
      income_bins = factor(
        income_bins,
        levels = c("Lower income", "Upper and middle income")
      ),
      minority = factor(minority, levels = c("Non-minority", "Minority"))
    )
  return(data)
}

control_effects_table <- function(main_sample_reg, NSDUH_reg, digits = 3) {
  #' Creates a table comparing regression of outcomes on controls in main sample and NSDUH.
  #' Writes table to "analysis/comparison/output/tables/comparison_mods_table.tex".
  #'
  #' @param main_sample_reg A linear model object for the main sample regression.
  #' @param NSDUH_reg A linear model object for the NSDUH sample regression.
  #' @param digits An integer specifying the number of digits to round to. Default is 3.
  #'
  # Extract variable names, coefficients, and standard errors
  main_table <- regs_vals_to_df(list(main_sample_reg))
  NSDUH_table <- regs_vals_to_df(list(NSDUH_reg))
  colnames(main_table) <- c("Var", "Index")
  colnames(NSDUH_table) <- c("Var", "K6 Score")
  # rename variables to to their labels, drop intercept term
  col_names_dict <- list(
    "(Intercept)" = "Intercept",
    "participant_has_degreeCollege degree" = "College",
    "income_binsUpper and middle income" = "Upper and middle income",
    "age_binsAge 35+" = "Age 35+",
    "minorityMinority" = "Minority",
      "participant_male_or_notFemale" = "Female"
  )
  main_table <- main_table %>%
    mutate(Var = ifelse(Var %in% names(col_names_dict), col_names_dict[Var], Var)) %>%
    filter(Var != "Intercept")
  NSDUH_table <- NSDUH_table %>%
    filter(Var != "(Intercept)") %>%
    select(-c("Var"))
  main_table <- cbind(main_table, NSDUH_table)
  # Clear the variable names on every second row for presentation
  main_table$Var[seq(2, nrow(main_table), by = 2)] <- ""
  colnames(main_table)[[1]] <- ""

  # Add headers to table
  n_obs <- length(residuals(main_sample_reg)) %>% format(big.mark = ",", scientific = FALSE)
  n_obs_nsduh <- length(residuals(NSDUH_reg)) %>% format(big.mark = ",", scientific = FALSE)
  addtorow <- list(
    pos = list(0, 0, nrow(main_table)),
    command = c(
      paste("&", paste0("(", seq(1, ncol(main_table) - 1), ")", collapse = " & "), "\\\\ \n"),
      paste0(" & Emotional state index & Kessler-6 score", "\\\\ \n"),
      paste0("\\# obs. & ", paste(n_obs, collapse = " & "), " & ", paste(n_obs_nsduh), " \\\\ \n")
    )
  )
  print(
    xtable(
      main_table,
      align = list(
        "l",
        "l",
        ">{\\centering\\arraybackslash}p{2.5cm}",
        ">{\\centering\\arraybackslash}p{2.5cm}"
      )
    ),
    type = "latex",
    floating = FALSE,
    add.to.row = addtorow,
    include.rownames = FALSE,
    include.colnames = FALSE,
    file = "analysis/comparison/output/tables/comparison_mods_table.tex"
  )
}

## Execute
main()
