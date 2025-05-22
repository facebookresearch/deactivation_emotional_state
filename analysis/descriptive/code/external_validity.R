# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

#############################################
###   descriptive - external_validity.R   ###
#############################################

devtools::load_all("~/alethea")
library(dplyr)
library(forcats)
library(stringr)
library(tidyr)
library(xtable)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")

main <- function() {
    pop_stats <- readRDS("analysis/descriptive/input/fb_ig_pop_stats.rds")
    main_sample_fb <- readRDS("analysis/descriptive/input/main_sample_fb.rds")
    main_sample_ig <- readRDS("analysis/descriptive/input/main_sample_ig.rds")

    fb_pop_stats <- create_pop_comparison_table(main_sample_fb, pop_stats, plat = "FB")
    write_pop_comparison_table(fb_pop_stats, plat = "FB")

    ig_pop_stats <- create_pop_comparison_table(main_sample_ig, pop_stats, plat = "IG")
    write_pop_comparison_table(ig_pop_stats, plat = "IG")

    message("Finished population comparison tables.")
}

create_pop_comparison_table <- function(sample, pop_stats, plat = "FB") {
    #' Generates a table comparing the sample and population demographics for a given recruitment_platform_string.
    #'
    #' @param sample A data frame containing the main sample data.
    #' @param pop_stats A data frame containing the population-level summary statistics.
    #' @param plat A string representing the recruitment_platform_string to compare. Default is 'FB'.
    #'
    #' @return A data frame containing the user-day data for FB, IG use and vpvs.
    #'
    platl <- tolower(plat)
    if (plat == "FB") {
        survey_stats <- sample %>%
            mutate(
                ideo_coarse = fct_recode(
                    participant_political_ideology,
                    "C" = "Very conservative",
                    "C" = "Somewhat conservative",
                    "M" = "Middle of the road",
                    "L" = "Very liberal",
                    "L" = "Somewhat liberal"
                )) %>%
            select(
                recruitment_platform_string, ideo_coarse, fb_ts_base_wtd_base_win,
                facebook_friend_count, participant_number_of_active_days_aug25_sept24, civic_page_like_count
            ) %>%
            summarize(
                ideo_coarse_C = 100 * sum(ifelse(ideo_coarse == "C", 1, 0), na.rm = TRUE) / n(),
                ideo_coarse_M = 100 * sum(ifelse(ideo_coarse == "M", 1, 0), na.rm = TRUE) / n(),
                ideo_coarse_L = 100 * sum(ifelse(ideo_coarse == "L", 1, 0), na.rm = TRUE) / n(),
                fb_ts = mean(fb_ts_base_wtd_base_win, na.rm = TRUE),
                facebook_friend_count = mean(facebook_friend_count, na.rm = TRUE),
                participant_number_of_active_days_aug25_sept24 = mean(participant_number_of_active_days_aug25_sept24, na.rm = TRUE),
                civic_pages_cnt = mean(civic_page_like_count, na.rm = TRUE)
            ) %>%
            gather(feature, sample_avg)
    } else {
        survey_stats <- sample %>%
            select(ig_ts_base_wtd_base_win, follower_count, follow_count, participant_number_of_active_days_aug25_sept24) %>%
            summarize(
                ig_ts = mean(ig_ts_base_wtd_base_win, na.rm = TRUE),
                follower_count = mean(follower_count, na.rm = TRUE),
                follow_count = mean(follow_count, na.rm = TRUE),
                participant_number_of_active_days_aug25_sept24 = mean(participant_number_of_active_days_aug25_sept24, na.rm = TRUE)
            ) %>%
            gather(feature, sample_avg)
    }
    pop_stats <- pop_stats %>% filter(str_detect(pop, platl)) %>% rename(pop_avg = avg)
    comparison_stats <- pop_stats %>%
        select(feature, pop_avg) %>%
        inner_join(survey_stats, by = "feature") %>%
        mutate(
            pop_avg = if_else(plat == "FB" & grepl("ideo", feature), 100 * pop_avg, pop_avg),
            feature = if (plat == "FB") {
                recode_factor(feature,
                    participant_number_of_active_days_aug25_sept24             = "Days active in previous month",
                    fb_ts           = "Normalized time spent on Facebook",
                    facebook_friend_count    = "Friend count",
                    ideo_coarse_C   = "Percent Conservative",
                    ideo_coarse_M   = "Percent Moderate",
                    ideo_coarse_L   = "Percent Liberal",
                    civic_pages_cnt = "Civic pages followed"
                )
            } else {
                recode_factor(feature,
                    participant_number_of_active_days_aug25_sept24        = "Days active in previous month",
                    ig_ts      = "Normalized time spent on Instagram",
                    follower_count  = "Follower count",
                    follow_count = "Following count"
                )
            }
        ) %>%
        select(feature, sample_avg, pop_avg)
    return(comparison_stats)
}

write_pop_comparison_table <- function(tab, plat) {
    #' Writes the population comparison table to a LaTeX file.
    #'
    #' @param tab A data frame containing processed sample vs population comparison statistics.
    #' @param plat A string representing the recruitment_platform_string to compare. Default is 'FB'.
    #'
    platl <- tolower(plat)
    # manually set significant digits for each feature
    if (plat == "FB") {
        platform_name <- "Facebook"
        tab_digits_sig <- matrix(c(
                rep(1, 4 * 3), # percent Conservative, Liberal, Moderate
                rep(2, 4),     # Time spent
                rep(1, 4),     # civic pages
                rep(0, 4),     # friend count
                rep(1, 4)      # days active
            ),
            nrow = 7, ncol = 4, byrow = TRUE
        )
    } else if (plat == "IG") {
        platform_name <- "Instagram"
        tab_digits_sig <- matrix(c(
                rep(2, 4),     # Time spent
                rep(0, 4 * 2), # follower & friend count
                rep(1, 4)      # days active
            ),
            nrow = 4, ncol = 4, byrow = TRUE
        )
    }
    addtorow <- list()
    addtorow$pos <- list(0, 0)
    addtorow$command <- c(
         "& (1) & (2) \\\\\n",
         paste0("& Sample & ", platform_name, " users \\\\\n")
    )
    comparison_latex <- xtable(
        tab, type = "latex", digits = tab_digits_sig, align = "llcc"
    )
    print(
        comparison_latex,
        add.to.row = addtorow,
        include.colnames = FALSE,
        include.rownames = FALSE,
        caption.placement = "top",
        file = sprintf("analysis/descriptive/output/tables/pop_comparison_%s.tex", platl)
    )
}


## Execute
main()
