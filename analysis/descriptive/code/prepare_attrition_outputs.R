# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

#####################################################
###   descriptive - prepare_attrition_outputs.R   ###
#####################################################

library(dplyr)
library(xtable)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")

source("lib/constants.R")

main <- function() {
    out_dir <- "analysis/descriptive/output/"

    main_sample_fb <- readRDS("analysis/descriptive/input/main_sample_fb.rds")
    main_sample_ig <- readRDS("analysis/descriptive/input/main_sample_ig.rds")
    main_sample <- bind_rows(main_sample_fb, main_sample_ig)

    # Create differential attrition table for wave 4
    attrition_w4 <- w4_differential_attrition(main_sample)
    write_attrition_table(attrition_w4, dir = out_dir)
}

w4_differential_attrition <- function(sample) {
    #' Builds differential attrition table for wave 4.
    #' Computes average completion rates by treatment group and recruitment_platform_string, and
    #' performs t-tests for differential attrition. Compiles in table.
    #'
    #' @param sample A data frame containing the combined (across recruitment_platform_string) sample data.
    #'
    #' @return A matrix containing the attrition rates and t-test p-values for each recruitment_platform_string.
    #'
    # create sample of all individuals who completed the endline survey
    w4_completed <- sample %>%
        select(participant_user_id, recruitment_platform_string, experiment_group, starts_with("ENDDT_")) %>%
        mutate(
            completed = !is.na(ENDDT_W2) & !is.na(ENDDT_W4),
            ENDDT_W4 = as.Date(ENDDT_W4, format = "%m/%d/%y")
        )
    # calculate differential attrition, and associated p-value
    w4_completed_avg <- w4_completed %>%
        group_by(recruitment_platform_string, experiment_group) %>%
        summarise(completed = mean(completed))
    fb_test <- t.test(completed ~ experiment_group, data = filter(w4_completed, recruitment_platform_string == "FB"))
    ig_test <- t.test(completed ~ experiment_group, data = filter(w4_completed, recruitment_platform_string == "IG"))
    # compile in table
    get_est <- function(d, p, g) d[(d$recruitment_platform_string == p) & (d$experiment_group == g), ][["completed"]]
    attrition_rates <- matrix(
        c(
            get_est(w4_completed_avg, "FB", "test"),
            get_est(w4_completed_avg, "FB", "control"),
            fb_test$p.value,
            get_est(w4_completed_avg, "IG", "test"),
            get_est(w4_completed_avg, "IG", "control"),
            ig_test$p.value
        ),
        ncol = 6,
        byrow = TRUE
    )
    colnames(attrition_rates) <- c(
        "FB Deactivated",
        "FB Control",
        "t-test p-value",
        "IG Deactivated",
        "IG Control",
        "t-test p-value"
    )
    rownames(attrition_rates) <- c("Completed endline survey")
    return(attrition_rates)
}

write_attrition_table <- function(tab, dir, wave = 4) {
    #' Writes differential attrition table to a LaTeX file.
    #' 
    #' @param tab A matrix containing the attrition rates and t-test p-values for each recruitment_platform_string.
    #'      Output of `w4_differential_attrition`.
    #' @param dir The directory to write the table to.
    #' @param wave The wave number for which the table is being created.
    #'
    addtorow <- list()
    addtorow$pos <- list(0, 0, 0, 0)
    addtorow$command <- c(
        "& \\multicolumn{3}{c}{Facebook} & \\multicolumn{3}{c}{Instagram} \\\\\n",
        "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}\n",
        "& (1) & (2) & (3) & (4) & (5) & (6) \\\\\n",
        "& Deactivation & Control & $t$-test $p$-value & Deactivation & Control & $t$-test $p$-value \\\\\n"
    )
    attrition_table <- xtable(tab, type = "latex", digits = rep(3, 7), align = rep("c", 7))
    print(
        attrition_table,
        add.to.row = addtorow,
        include.colnames = FALSE,
        file = paste0(dir, sprintf("tables/attrition_rates_table_w%s.tex", wave))
    )
}


## Execute
main()
