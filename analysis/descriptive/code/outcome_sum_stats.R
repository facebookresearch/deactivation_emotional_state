# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

#############################################
###   descriptive - outcome_sum_stats.R   ###
#############################################

library(Hmisc)
library(tidyr)
library(dplyr)
library(stringr)
library(xtable)

devtools::load_all('~/alethea')
theme_set(theme_alethea())

source("lib/outcome_dictionaries.R")
source("lib/prepare_outputs.R")

main <- function() {
    main_sample_fb <- readRDS("analysis/descriptive/input/main_sample_fb.rds")
    main_sample_ig <- readRDS("analysis/descriptive/input/main_sample_ig.rds")

    # Make variables and outcome labels
    outcomes_label_dict <- make_emotion_outcomes_og_units_label_dict()
    outcomes_baseline_label_dict <- make_emotion_outcomes_og_units_baseline_dict()
    # we want mapping baseline vars --> label
    outcomes_baseline_label_dict <- setNames(
        unlist(outcomes_label_dict, use.names = FALSE),
        unlist(outcomes_baseline_label_dict, use.names = FALSE)
    )

    # calculate summary stats
    sum_stats_fb <- calc_summary_statistics(main_sample_fb, outcomes_label_dict, plat = "FB")
    sum_stats_ig <- calc_summary_statistics(main_sample_ig, outcomes_label_dict, plat = "IG")
    sum_stats_pre_fb <- calc_summary_statistics(
        main_sample_fb,
        outcomes_baseline_label_dict,
        plat = "FB",
        weights = "weights_w4",
        suffix = "_pre"
    )
    sum_stats_pre_ig <- calc_summary_statistics(
        main_sample_ig,
        outcomes_baseline_label_dict,
        plat = "IG",
        weights = "weights_w4",
        suffix = "_pre"
    )

    # create plots of average emotional state by survey by X
    main_sample <- bind_rows(
        main_sample_fb %>% mutate(plat = "FB"),
        main_sample_ig %>% mutate(plat = "IG")
    )
    surveys <- c("Baseline", "Endline", "Post-endline")
    index_std_w4_outcomes <- c("emot_pre_standardized_post", "emot_post", "emot_w5_standardized_post")
    plot_outcome_avg_sample(
        main_sample,
        index_std_w4_outcomes,
        surveys,
        y_label = "Emotional state index\n(standardized to endline)"
    )
    happy_outcomes <- c("emot_happy_pre_og", "emot_happy_post_og",  "emot_happy_w5_og")
    plot_outcome_avg_sample(
        main_sample,
        happy_outcomes,
        surveys,
        suffix = "_happy",
        y_label = "Happy"
    )
    depress_outcomes <- c("emot_depress_pre_og", "emot_depress_post_og", "emot_depress_w5_og")
    plot_outcome_avg_sample(
        main_sample, 
        depress_outcomes,
        surveys,
        suffix = "_depress",
        y_label = "Depressed"
    )
    anxious_outcomes <- c("emot_anxious_pre_og", "emot_anxious_post_og", "emot_anxious_w5_og")
    plot_outcome_avg_sample(
        main_sample,
        anxious_outcomes,
        surveys,
        suffix = "_anxious",
        y_label = "Anxious"
    )
    plot_index_avg_party(
        main_sample,
        index_std_w4_outcomes,
        surveys,
        y_label = "Emotional state index\n(standardized to endline)"
    )

    message("Finished generating outcome summary statistics and plots.")
}

calc_summary_statistics <- function(
    df,
    outcomes_label_dict,
    plat = "FB",
    weights = "weights_w4",
    suffix = ""
) {
    #' Calculates summary statistics for the main sample given a set of outcomes.
    #' Writes the results to a LaTeX table.
    #'
    #' @param df A data frame containing the main sample.
    #' @param outcomes_label_dict A named vector mapping outcome variable names to labels.
    #' @param plat A string representing the recruitment_platform_string ('FB' or 'IG'). Default is 'FB'.
    #' @param weights A string representing the weights variable. Default is 'weights_w4'.
    #' @param suffix A string representing the suffix to append to the output file name. Default is ''.
    #'
    #' @return A data frame containing the summary statistics.
    #'
    outcomes <- names(outcomes_label_dict)
    tab <- df %>%
        # restrict to analysis sample for main regression (i.e., index regression)
        filter(!is.na(emot_post)) %>% 
        select(all_of(weights), all_of(outcomes)) %>%
        summarise(
            across(all_of(outcomes),
                list(
                    mean = ~ wtd.mean(., weights = get(weights), na.rm = TRUE),
                    sd = ~ sqrt(wtd.var(., weights = get(weights), normwt = TRUE, na.rm = TRUE)),
                    min = ~ min(., na.rm = TRUE),
                    max = ~ max(., na.rm = TRUE),
                    n = ~ sum(!is.na(.))
                ),
                .names = "{col}__{fn}"
        ))
    # pivot to longer version w/ summary stats as columns
    tab <- tab %>% pivot_longer(
        cols = everything(), names_to = c("dv", ".value"), names_sep = "__"
    ) %>%
        mutate(dv_label = recode(dv, !!!outcomes_label_dict)) %>%
        select(dv_label, everything()) %>%
        select(-dv) %>%
        rename(
            Mean = mean,
            `Std. Dev.` = sd,
            Min. = min,
            Max. = max,
            N = n
        )
    names(tab)[names(tab) == "dv_label"] <- ""
    # prepare table for LaTeX
    numeric_headers <- paste0("\\multicolumn{1}{c}{(", seq(1, ncol(tab)-1), ")}", collapse = " & ")
    numeric_headers <- paste0("& ", numeric_headers)
    addtorow <- list(
        pos = list(-1),
        command = c(paste("\\\\[-1.8ex] \\hline", numeric_headers, "\\\\\n"))
    )
    print(
        xtable(tab, digits = 2, align = c("l", rep("c", ncol(tab)))),
        add.to.row = addtorow,
        include.rownames = FALSE,
        sanitize.colnames.function = identity,
        format.args = list(big.mark = ","),
        type = "latex",
        floating = FALSE,
        file = paste0(
            "analysis/descriptive/output/tables/outcomes_summary_stats",
            suffix, 
            "_",
            tolower(plat),
            ".tex"
        )
    )
    return(tab)
}


mean_ci <- function(x, weights, conf = 0.95) {
    #' Calculate the mean and confidence interval of a weighted sample.
    #'
    #' @param x A numeric vector.
    #' @param weights A numeric vector of weights.
    #' @param conf A numeric value representing the confidence level. Default is 0.95.
    #'
    #' @return A vector containing the mean, lower bound and upper bound of CI.
    #'
    # Remove NAs
    filt <- !is.na(x) & !is.na(weights)
    x <- x[filt]
    weights <- weights[filt]
    # Fit linear regression model with weights
    fit <- lm(x ~ 1, weights = weights)
    coef_summary <- summary(fit)$coefficients
    mean <- coef_summary[1, "Estimate"]
    se <- coef_summary[1, "Std. Error"]
    # Calculate confidence interval
    n <- length(x)
    alpha <- 1 - conf
    err <- qt(1 - alpha / 2, df = n - 1) * se
    lower <- mean - err
    upper <- mean + err
    return(c(mean = mean, ci_low = lower, ci_hi = upper))
}

create_mean_ci_df <- function(df, vars, weights_var, conf = 0.95) {
    #' Create a data frame containing the mean and confidence intervals of a set of variables.
    #'
    #' @param df A data frame representing the sample.
    #' @param vars A character vector of variable names of interest.
    #' @param weights_var A character representing the weights variable.
    #'
    #' @return A dataframe containing the mean and confidence intervals for the variables.
    #'
    res <- df %>%
        select(all_of(vars), all_of(weights_var)) %>%
        reframe(across(
            all_of(vars),
            ~ data.frame(
                t(mean_ci(., weights = pick(all_of(weights_var)), conf = conf))
            ),
            .names = "{col}"
        )) %>%
        # reshapes to have separate columns for mean, lower_ci, and upper_ci
        pivot_longer(cols = everything(), names_to = "variable", values_to = "stats") %>%
        unnest(stats)
    return(res)
}

plot_outcome_avg_sample <- function(df, outcomes, labels, suffix = "", y_label = "") {
    #' Create a plot of the average outcome by survey (baseline; endline; post-endline)
    #' by sample (those completing endline; those completing post-endline).
    #' Writes the results as pdf and png. 
    #'
    #' @param df A data frame representing the sample.
    #' @param outcomes A character vector of variable of interest over multiple surveys.
    #' @param labels A character vector of labels for the outcomes.
    #' @param suffix A character representing the suffix to append to the output file name. Default is ''.
    #' @param y_label A character representing the y-axis label. Default is ''.
    #'
    outcomes_label_dict <- setNames(labels, outcomes)
    # get endline, post-endline samples
    endline_df <- df %>% filter(!is.na(emot_post), experiment_group == "control")
    post_endline_df <- df %>% filter(!is.na(emot_w5), experiment_group == "control")
    # get mean, ci for each outcome in sample
    index_avg_endline <- create_mean_ci_df(endline_df, outcomes, "weights_w4")
    index_avg_post_endline <- create_mean_ci_df(post_endline_df, outcomes, "weights_w5")
    # combine estimates and add variable labels
    index_avg <- bind_rows(
        index_avg_endline %>% mutate(sample = "Endline"),
        index_avg_post_endline %>% mutate(sample = "Post-endline")
    ) %>%
        mutate(variable_name = recode(variable, !!!outcomes_label_dict))
    # create plot
    po <- plot_out_avg(index_avg, y_label = y_label, group = "sample", group_label = "Sample") +
        scale_color_manual(values = c(maroon, grey))
    # hard coding y axes to reasonable ranges
    if (suffix == "_happy") {
        po <- po + ylim(2.45, 2.65)
    } else if (suffix == "_depress") {
        po <- po + ylim(1.7, 1.5)
    } else if (suffix == "_anxious") {
        po <- po + ylim(2.4, 2.1)
    } else {
        po <- po + ylim(-0.05, 0.15)
        # extract scalars
        write_baseline_endline_scalar(index_avg)
    }
    save_plot(
        po = po,
        dir = "analysis/descriptive/output",
        fname = paste0("outcome_by_survey_by_sample", suffix),
        width = 5,
        height = 4
    )
}

write_baseline_endline_scalar <- function(res) {
    #' Writes the baseline-endline and post-endline-endline index differences to a LaTeX table.
    #'
    #' @param res A data frame containing mean, CI for the index variables. 
    #'      From `plot_outcome_avg_sample.`
    #'
    res <- res %>% filter(sample == "Endline")
    endline <- res[res$variable == "emot_post", "mean"]
    baseline_diff <- res[res$variable == "emot_pre_standardized_post", "mean"] - endline
    post_endline_diff <- res[res$variable == "emot_w5_standardized_post", "mean"] - endline
    scalars <- list(
        "BaselineEndlineDiff" = baseline_diff,
        "PostEndlineEndlineDiff" = post_endline_diff
    )
    save_scalars_tex(
        scalars,
        filename = "analysis/descriptive/output/scalars/baseline_endline_scalars.tex",
        digits = 2
    )
}


plot_index_avg_party <- function(df, outcomes, labels, y_label = "") {
    #' Creates a plot of the average index by survey by party for the endline sample.
    #' Writes the results as pdf and png. 
    #'
    #' @param df A data frame representing the sample.
    #' @param outcomes A character vector of variable of interest over multiple surveys.
    #' @param labels A character vector of labels for the outcomes.
    #' @param y_label A character representing the y-axis label. Default is ''.
    #'
    outcomes_label_dict <- setNames(labels, outcomes)
    # get endline samples for each party (R, D)
    endline_df_R <- df %>% filter(!is.na(emot_post), experiment_group == "control", partyid == "R")
    endline_df_D <- df %>% filter(!is.na(emot_post), experiment_group == "control", partyid == "D")
    # get mean, ci for each outcome in sample
    index_avg_endline_R <- create_mean_ci_df(endline_df_R, outcomes, "weights_w4")
    index_avg_endline_D <- create_mean_ci_df(endline_df_D, outcomes, "weights_w4")
    # combine estimates and add variable labels
    index_avg <- bind_rows(
        index_avg_endline_R %>% mutate(partyid = "Republicans"),
        index_avg_endline_D %>% mutate(partyid = "Democrats")
    ) %>%
        mutate(variable_name = recode(variable, !!!outcomes_label_dict))
    # plot averages
    po <- plot_out_avg(index_avg, y_label, group = "partyid", group_label = "Party ID") +
        scale_color_manual(values = c("blue", "red")) + ylim(-0.3, 0.4)
    save_plot(
        po = po,
        dir = "analysis/descriptive/output",
        fname = "outcome_by_survey_by_party",
        width = 5,
        height = 4
    )
}


plot_out_avg <- function(df, y_label, group, group_label) {
    #' Creates a plot of an average outcome by survey (`variable_name`) by group.
    #'
    #' @param df A data frame representing the sample.
    #' @param y_label A character representing the y-axis label.
    #' @param group A character representing the grouping variable (for color).
    #' @param group_label A character representing the label for the grouping variable.
    #'
    ggplot(df, aes(x = variable_name, y = mean, color = get(group))) +
        geom_point(shape = 16, position = position_dodge(width = 0.5)) +
        geom_errorbar(
            aes(ymin = ci_low, ymax = ci_hi),
            width = 0.25,
            position = position_dodge(width = 0.5)
        ) +
        theme_bw() +
        theme(
            axis.title = element_text(size = rel(0.9)),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.border = element_blank(),
            axis.text.x = element_text(color = "black"),
            axis.text.y = element_text(color = "black")
        ) +
        labs(x = "Survey", y = y_label, color = group_label)
}


## Execute
main()
