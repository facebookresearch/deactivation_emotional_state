# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

#############################################
###   descriptive - responses_by_date.R   ###
#############################################

library(lubridate)
library(ggplot2)
library(tidyr)
library(dplyr)
library(xtable)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")

devtools::load_all("~/alethea")
theme_set(theme_alethea())

source("lib/constants.R")
source("lib/prepare_outputs.R")

main <- function() {
    attriters <- readRDS("analysis/descriptive/input/attriters.rds")
    weighted_data_ids <- readRDS("analysis/descriptive/input/weighted_data_ids.rds")
    main_sample_fb <- readRDS("analysis/descriptive/input/main_sample_fb.rds")
    main_sample_ig <- readRDS("analysis/descriptive/input/main_sample_ig.rds")
    passive_tracking_sample <- readRDS("analysis/descriptive/input/passive_tracking_users.rds")

    ## Count people at each stage (Table 1)
    # Note: weighted_data_ids also contains IDs not in uncut or
    # main sample which are filtered out otherwise
    stage_sample_sizes <- get_stage_sample_sizes(
        data_ids = weighted_data_ids,
        attriters = attriters,
        main_sample_fb = main_sample_fb,
        main_sample_ig = main_sample_ig
    )
    sample_size_scalars(stage_sample_sizes, main_sample_fb, main_sample_ig, passive_tracking_sample)

    plot_completes_by_date(sample = main_sample_fb, plat = "FB")
    plot_completes_by_date(sample = main_sample_ig, plat = "IG")
}


get_stage_sample_sizes <- function(data_ids, attriters, main_sample_fb, main_sample_ig) {
    #' Creates table with sample sizes at each stage of the study.
    #' Note: Includes hard-coded numbers from NORC.
    #' Writes table to a LaTeX file.
    #'
    #' @param data_ids A data frame containing the full set of ids, weights, and sample binaries.
    #' @param attriters A data frame containing individuals who attrited from the survey.
    #' @param main_sample_fb A data frame containing the main Facebook sample.
    #' @param main_sample_ig A data frame containing the main Instagram sample.
    #'
    #' @return A matrix containing sample size at each stage of the study.
    #'
    # identify those who withdrew / attrited from survey
    sample_id_treat <- bind_rows(main_sample_fb, main_sample_ig) %>% select(participant_user_id, experiment_group)
    data_ids <- data_ids %>% left_join(sample_id_treat, by = c("participant_user_id"))
    completed_w2_ids <- unlist(data_ids$participant_user_id[data_ids$completed_w2])
    attriters <- attriters %>%
        filter(study_experiment_group %in% c("deactivation", "deactivation_control")) %>%
        filter(has_withdrawn)
    # calculate completions at each stage after baseline
    completions <- data_ids %>%
        group_by(recruitment_platform_string) %>%
        summarize(
            w2 = sum(completed_w2),
            w2_plat = sum(completed_w2 & has_platform_data),
            w2_primary = sum(completed_w2 & has_platform_data & above_15min_usage),
            w2_primary_deact = sum(
                completed_w2 & has_platform_data & above_15min_usage & (experiment_group == "test")
            ),
            w4 = sum(completed_w2 & completed_w4 & has_platform_data & above_15min_usage),
            w5 = sum(
                completed_w2 & completed_w4 & completed_w5 & has_platform_data & above_15min_usage
            )
        )
    withdrawals_fb <- nrow(attriters %>% filter(recruitment_platform_string == "FB"))
    withdrawals_ig <- nrow(attriters %>% filter(recruitment_platform_string == "IG"))
    stage_sample_sizes <- matrix(
        c(
            # Hard-coded numbers from NORC
            10597957, 2633479, # shown feed banner
            673388, 319271, # clicked feed banner
            76980, 71769, # willing to deactivate
            52821, 51955, # consented
            43249, 42658, # completed survey 1
            25484, 23494, # completed survey 2
            completions[completions$recruitment_platform_string == "FB", ]$w2 + withdrawals_fb, # confirmed survey 2
            completions[completions$recruitment_platform_string == "IG", ]$w2 + withdrawals_ig, # confirmed survey 2
            completions[completions$recruitment_platform_string == "FB", ]$w2_plat, # in recruitment_platform_string data
            completions[completions$recruitment_platform_string == "IG", ]$w2_plat, # in recruitment_platform_string data
            completions[completions$recruitment_platform_string == "FB", ]$w2_primary, # >15 min baseline usage
            completions[completions$recruitment_platform_string == "IG", ]$w2_primary, # >15 min baseline usage
            completions[completions$recruitment_platform_string == "FB", ]$w4, # completed survey 4
            completions[completions$recruitment_platform_string == "IG", ]$w4, # completed survey 4
            completions[completions$recruitment_platform_string == "FB", ]$w5, # completed survey 5
            completions[completions$recruitment_platform_string == "IG", ]$w5 # completed survey 5
        ),
        ncol = 2,
        byrow = TRUE
    )
    colnames(stage_sample_sizes) <- c("Facebook", "Instagram")
    rownames(stage_sample_sizes) <- c(
        "Shown feed banner",
        "Clicked feed banner",
        "Willing to deactivate",
        "Consented",
        "Completed enrollment survey",
        "Completed baseline survey",
        "Confirmed on baseline survey",
        "In platform data & did not withdraw",
        "Use >15 minutes/day (primary analysis sample)",
        "Completed endline survey",
        "Completed post-endline survey"
    )
    addtorow <- list(
        pos = list(0, 0),
        command = c("& (1) & (2) \\\\\n", "& Facebook & Instagram \\\\\n")
    )
    stage_sample_size_table <- xtable(
        stage_sample_sizes,
        type = "latex",
        digits = rep(0, 3),
        align = c("l", rep("c", 2))
    )
    print(
        stage_sample_size_table,
        add.to.row = addtorow,
        include.colnames = FALSE,
        format.args = list(big.mark = ","),
        file = "analysis/descriptive/output/tables/stage_sample_size_table.tex"
    )
    # Extend table for scalar computation
    primary_sample_deact <- matrix(
        c(
            completions[completions$recruitment_platform_string == "FB", ]$w2_primary_deact,
            completions[completions$recruitment_platform_string == "IG", ]$w2_primary_deact
        ),
        ncol = 2,
        byrow = TRUE
    )
    colnames(primary_sample_deact) <- c("Facebook", "Instagram")
    rownames(primary_sample_deact) <- c("Use >15 minutes/day, deactivation")
    stage_sample_sizes <- rbind(stage_sample_sizes, primary_sample_deact)
    return(stage_sample_sizes)
}


plot_completes_by_date <- function(sample, plat = "FB") {
    #' Creates a plot of the cumulative endline completion rate by date for a given recruitment_platform_string.
    #' Writes plot as a pdf and png file.
    #'
    #' @param sample A data frame containing the main sample for a given recruitment_platform_string.
    #' @param plat A string indicating the recruitment_platform_string to plot. Default is 'FB'.
    #'
    w4_dates <- sample %>%
        select(participant_user_id, recruitment_platform_string, experiment_group, starts_with("ENDDT_")) %>%
        mutate(
            completed = !is.na(ENDDT_W2) & !is.na(ENDDT_W4),
            enddate = replace_na(ymd(ENDDT_W4), ymd("2020-01-01"))
        ) %>%
        group_by(recruitment_platform_string, experiment_group, enddate) %>%
        summarise(
            n_completed = sum(enddate > ymd("2020-01-01")),
            total = n()
        ) %>%
        mutate(
            sum_completed = cumsum(n_completed),
            pct_completed = n_completed / sum(total),
            cum_pct_completed = sum_completed / sum(total)
        ) %>%
        filter(enddate > ymd("2020-01-01")) %>%
        ungroup()
    df_plot <- w4_dates %>%
        mutate(enddate = day(enddate)) %>%
        mutate(experiment_group = case_when(
            experiment_group == "control" ~ "Control",
            experiment_group == "test" ~ "Deactivation",
            TRUE ~ as.character(NA)
        ))
    po <- ggplot(
            df_plot,
            aes(
                x = enddate,
                y = cum_pct_completed, 
                color = experiment_group,
                shape = experiment_group,
                group = experiment_group
            )
        ) +
        geom_point() +
        geom_line() +
        ylim(0.45, 1) +
        labs(
            x = "Date (November)",
            y = "Endline survey completion rate"
        ) +
        theme(
            axis.title = element_text(size = 13),
            axis.text = element_text(size = 12),
            legend.position = "bottom"
        ) +
        scale_color_manual(name = "", values = c(grey, maroon)) +
        scale_shape_manual(name = "", values = c(15, 18))
    save_plot(
        po,
        dir = "analysis/descriptive/output",
        fname = paste0("completes_by_date_", tolower(plat)),
        width = 6.5,
        height = 4.5
    )
}

sample_size_scalars <- function(
    stage_sample_sizes,
    main_sample_fb,
    main_sample_ig,
    passive_tracking_sample
) {
    #' Writes scalars from sample size table to a LaTeX file.
    #'
    #' @param stage_sample_sizes A matrix containing sample size at each stage of the study.
    #'      From `get_stage_sample_sizes`.
    #' @param main_sample_fb A data frame containing the main Facebook sample.
    #' @param main_sample_ig A data frame containing the main Instagram sample.
    #' @param passive_tracking_sample A data frame containing data from passive tracking users.
    #'
    # Scalars stored as integers
    clicked_row <- "Clicked feed banner"
    clicked_size_fb <- stage_sample_sizes[c(clicked_row), c("Facebook")]
    clicked_size_ig <- stage_sample_sizes[c(clicked_row), c("Instagram")]

    consented_row <- "Consented"
    consented_size_fb <- stage_sample_sizes[c(consented_row), c("Facebook")]
    consented_size_ig <- stage_sample_sizes[c(consented_row), c("Instagram")]

    enrollment_row <- "Completed enrollment survey"
    enrollment_size_fb <- stage_sample_sizes[c(enrollment_row), c("Facebook")]
    enrollment_size_ig <- stage_sample_sizes[c(enrollment_row), c("Instagram")]

    primary_sample_row <- "Use >15 minutes/day (primary analysis sample)"
    primary_sample_size_fb <- stage_sample_sizes[c(primary_sample_row), c("Facebook")]
    primary_sample_size_ig <- stage_sample_sizes[c(primary_sample_row), c("Instagram")]

    completed_w4_row <- "Completed endline survey"
    endline_survey_size_fb <- stage_sample_sizes[c(completed_w4_row), c("Facebook")]
    endline_survey_size_ig <- stage_sample_sizes[c(completed_w4_row), c("Instagram")]

    scalars_int <- list(
        ClickedSizeFB = clicked_size_fb,
        ClickedSizeIG = clicked_size_ig,
        ConsentedSizeFB = consented_size_fb,
        ConsentedSizeIG = consented_size_ig,
        EnrollmentSizeFB = enrollment_size_fb,
        EnrollmentSizeIG = enrollment_size_ig,
        SampleSizePrimaryFB = primary_sample_size_fb,
        SampleSizePrimaryIG = primary_sample_size_ig,
        EndlineSurveySizeFB = endline_survey_size_fb,
        EndlineSurveySizeIG = endline_survey_size_ig
    )
    scalars_int <- lapply(scalars_int, function(x) format(x, nsmall = 0, big.mark = ","))
    scalars_int_dig <- rep(0, length(scalars_int))

    # Scalars stored with 1 decimal value
    shown_feed_row <- "Shown feed banner"
    shown_feed_tot_fb <- stage_sample_sizes[c(shown_feed_row), c("Facebook")] / 10**6
    shown_feed_tot_ig <- stage_sample_sizes[c(shown_feed_row), c("Instagram")] / 10**6
    scalars_dec <- list(
        ShownSizeMillionFB = shown_feed_tot_fb,
        ShownSizeMillionIG = shown_feed_tot_ig
    )
    scalars_dec_dig <- rep(1, length(scalars_dec))

    # IG + FB passive tracking scalars
    passive_tracking_users <- passive_tracking_sample %>% pull(participant_user_id) %>% unique()
    main_sample_fb <- main_sample_fb %>%
        mutate(is_passive_tracking_user = ifelse(participant_user_id %in% passive_tracking_users, 1, 0))
    main_sample_ig <- main_sample_ig %>%
        mutate(is_passive_tracking_user = ifelse(participant_user_id %in% passive_tracking_users, 1, 0))
    scalars_pct <- list(
        PassiveTrackingPctFB = 100 * mean(main_sample_fb$is_passive_tracking_user),
        PassiveTrackingPctIG = 100 * mean(main_sample_ig$is_passive_tracking_user)
    )
    scalars_pct_dig <- rep(0, length(scalars_pct))

    save_scalars_tex(
        scalar_list = c(scalars_int, scalars_dec, scalars_pct),
        digits = c(scalars_int_dig, scalars_dec_dig, scalars_pct_dig),
        filename = "analysis/descriptive/output/scalars/sample_size_scalars.tex"
    )
}

## Execute
main()
