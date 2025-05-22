# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

######################################################
###   descriptive - index_reliability_validity.R   ###
######################################################

library(forcats)
library(lubridate)
library(stringr)
library(tidyr)
library(xtable)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")

source("lib/constants.R")
source("lib/outcome_dictionaries.R")
source("lib/prepare_outputs.R")

devtools::load_all("~/alethea")
theme_set(theme_alethea())

main <- function() {
    main_sample_fb <- readRDS("analysis/descriptive/input/main_sample_fb.rds")
    main_sample_ig <- readRDS("analysis/descriptive/input/main_sample_ig.rds")
    main_sample <- bind_rows(
        main_sample_fb %>% mutate(plat = "FB"),
        main_sample_ig %>% mutate(plat = "IG")
    )

    # calculate correlation matrices
    outcomes_label_dict <- make_emotion_outcomes_label_dict()
    outcomes_end <- process_dict_to_list(outcomes_label_dict)
    outcomes_baseline_dict <- make_emotion_outcomes_baseline_dict()
    outcomes_base <- process_dict_to_list(outcomes_baseline_dict)
    write_cor_matrix_table( # FB baseline
        sample = main_sample_fb,
        vars = outcomes_base$labels,
        labels = outcomes_end$labels,
        file_name = "outcome_cor_matrix_baseline_fb.tex"
    )
    write_cor_matrix_table( # FB endline
        sample = main_sample_fb,
        vars = outcomes_end$vars,
        labels = outcomes_end$labels,
        file_name = "outcome_cor_matrix_endline_fb.tex"
    )
    write_cor_matrix_table( # IG baseline
        sample = main_sample_ig,
        vars = outcomes_base$labels,
        labels = outcomes_end$labels,
        file_name = "outcome_cor_matrix_baseline_ig.tex"
    )
    write_cor_matrix_table( # IG endline
        sample = main_sample_ig,
        vars = outcomes_end$vars,
        labels = outcomes_end$labels,
        file_name = "outcome_cor_matrix_endline_ig.tex"
    )

    # compute Cronbach's alpha
    component_vectors <- c("emot_happy_post", "emot_depress_post", "emot_anxious_post")
    cronbach_alpha_fb <- compute_cronbach_alpha(main_sample_fb, component_vectors)
    cronbach_alpha_ig <- compute_cronbach_alpha(main_sample_ig, component_vectors)
    write_reliability_scalars(cronbach_alpha_fb, cronbach_alpha_ig)

    # analyze emotional state before and after election
    res <- create_index_party_election_table(main_sample)
    write_index_party_election_table(res)
    plot_index_by_party_response_date(main_sample)

    message("Finished index reliability and validity checks.")
}

write_cor_matrix_table <- function(sample, vars, labels, file_name) {
    #' Writes the correlation matrix for a set of variables to a LaTeX file.
    #'
    #' @param sample A data frame containing the main sample data.
    #' @param vars A character vector with the variables to include in the correlation matrix.
    #' @param labels A character vector containing the labels for the variables.
    #' @param file_name A string representing the name of the file to write the table to.
    #'
    cmat <- cor(
        sample %>% select(all_of(vars)),
        use = "complete.obs"
    )
    rownames(cmat) <- labels
    colnames(cmat) <- labels
    print(
        xtable(cmat, digits = 3, align = c("l", rep("c", length(vars)))),
        file = paste0("analysis/descriptive/output/tables/", file_name),
    )
}

compute_cronbach_alpha <- function(sample, components) {
    #' Computes Cronbach's alpha for a set of components.
    #' Wraps abbreviated version of `cronbach.alpha` from the `ltm` package, see below for source
    #'
    #' @param sample A data frame containing the main sample data.
    #' @param components A character vector with the variables to include in computation.
    #'
    #' @return A scalar representing the Cronbach's alpha for the components.
    #'
    cronbach_alpha <- sample %>%
        select(all_of(components)) %>%
        cronbach.alpha(standardized = TRUE, na.rm = TRUE)
    return(cronbach_alpha$alpha)
}


cronbach.alpha <- function (data, standardized = FALSE, na.rm = FALSE) {
    #' Computes Cronbach's alpha on a dataset.
    #' Note original source is: https://rdrr.io/github/drizopoulos/ltm/src/R/cronbach.alpha.R.
    #' The original source code has been edited only for simplicity for this use case.
    #'
    #' @param sample A data frame containing the main sample data.
    #' @param components A character vector with the variables to include in computation.
    #'
    #' @return A scalar representing the Cronbach's alpha for the components.
    #'
    n <- nrow(data)
    p <- ncol(data)
    data <- data.matrix(data)
    if (!na.rm && any(is.na(data))) {
        stop("missing values in 'data'.\n")
    } else {
        alpha <- if (!standardized) {
            VarTot <- var(rowSums(data[complete.cases(data), ]))
            VarInd <- sum(apply(data, 2, sd, na.rm = TRUE)^2)
            (p / (p - 1)) * (1 - (VarInd / VarTot))
        } else {
            mat <- cor(data, use = "complete.obs")
            ave.rho <- mean(mat[upper.tri(mat)])
            (p * ave.rho) / (1 + (p - 1) * ave.rho)
        }
    }
    out <- list(alpha = alpha, n = n, p = p, standardized = standardized)
    return(out)
}

create_index_party_election_table <- function(sample) {
    #' Creates a table reporting the potential differential responses in outcome
    #' before/after the election was called, by party affiliation.
    #'
    #' @param sample A data frame containing the main sample data.
    #'
    #' @return A list containing the regression models and the joined table.
    #'
    dat <- sample %>%
        filter(partyid != "I") %>% 
        mutate(
            # 11/07 is the date that the election was called
            after_11_07 = ifelse(ENDDT_W4 >= as.Date("2020-11-07"), 1, 0),
            RmD = (partyid == "R") - (partyid == "D")
        )
    formulas <- list(
        reg1 = "emot_post ~ 0 + emot_pre + partyid + partyid:after_11_07",
        reg2 = "emot_post ~ 0 + emot_pre + partyid + RmD:after_11_07"
    )
    run_and_process_reg <- function(formula_str, data) {
        mod <- lm(as.formula(formula_str), data = data, weights = data$weights_w4)
        tbl <- regs_vals_to_df(list(mod))
        colnames(tbl) <- c("var", "est")
        tbl <- tbl %>%
            mutate(coef = ifelse(row_number() %% 2 == 0, "std", "coef"))
        return(list(model = mod, tbl = tbl))
    }
    res <- lapply(formulas, run_and_process_reg, data = dat)
    var_rename_dict <- list(
        `partyidD` = "Democrats",
        `partyidR` = "Republicans",
        `partyidD:after_11_07` = "After x Democrats",
        `partyidR:after_11_07` = "After x Republicans",
        `RmD:after_11_07` = "After x (Republicans - Democrats)"
    )
    tbl <- merge(res$reg1$tbl, res$reg2$tbl, by = c("var", "coef"), all = TRUE) %>% 
        filter(!str_detect(var, "emot_pre")) %>%
        mutate(var = recode(var, !!!var_rename_dict)) %>%
        mutate(var = fct_relevel(var, unlist(var_rename_dict, use.names = FALSE))) %>%
        arrange(var) %>%
        select(-coef)
    return(list(mods = list(res$reg1$mod, res$reg2$mod), tbl = tbl))
}

write_index_party_election_table <- function(res, dir) {
    #' Writes the table comparing outcome by party affiliation x election called date.
    #'
    #' @param res The result of the `create_index_party_election_table` function.
    #'
    n_obs <- lapply(res$mods, function(x) length(residuals(x))) %>% unlist()
    n_obs <- format(n_obs, big.mark = ",", scientific = FALSE)
    # make every other row's var blank (the standard error rows)
    tab <- res$tbl %>% mutate(var = as.character(var))
    tab$var[seq(2, nrow(tab), by = 2)] <- ""
    colnames(tab)[[1]] <- ""
    addtorow <- list(
        pos = list(0, nrow(tab), nrow(tab)),
        command = c(
            paste("&", paste0("(", seq(1, ncol(tab) - 1), ")", collapse = " & "), "\\\\ \n"),
            "\\hline \n",
            paste0("\\# obs. & ", paste(n_obs, collapse = " & "), " \\\\ \n")
        )
    )
    print(xtable(tab, align = c("l", "l", "c", "c")),
        type = "latex",
        floating = FALSE,
        add.to.row = addtorow,
        include.rownames = FALSE,
        include.colnames = FALSE,
        file = "analysis/descriptive/output/tables/index_by_party_election_date.tex"
    )
}

plot_index_by_party_response_date <- function(sample) {
    #' Creates a plot of regressing the outcome on the response date by party affiliation.
    #' Plot presents an event study, with day dummies by party.
    #'
    #' @param sample A data frame containing the main sample data.
    #'
    dat <- sample %>%
        filter(partyid != "I") %>%
        mutate(
            ENDDT_W4 = ymd(ENDDT_W4),
            endline_response_day = day(ENDDT_W4),
        )
    f <- "emot_post ~ 0 + emot_pre + partyid:as.factor(endline_response_day)"
    reg <- lm(as.formula(f), data = dat, weights = dat$weights_w4)
    reg_tbl <- summary(reg)$coefficients
    colnames(reg_tbl) <- c("est", "se", "t", "p")
    reg_tbl <- reg_tbl %>%
        as.data.frame() %>%
        mutate(
            coef = rownames(reg_tbl),
            partyid = str_extract(coef, "(?<=partyid)[A-Z]"),
            response_day = as.numeric(
                str_extract(coef, "(?<=as.factor\\(endline_response_day\\))\\d+")
            )
        ) %>%
        filter(!is.na(partyid), !is.na(response_day)) %>%
        mutate(
            ci_lo = est - 1.96 * se,
            ci_hi = est + 1.96 * se
        )
    po <- reg_tbl %>%
        ggplot(aes( x = response_day, y = est, color = partyid)) +
        geom_point() +
        geom_line() +
        geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi, fill = partyid), color = "NA", alpha = 0.1) +
        theme(
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.text.x = element_text(color = "black"),
            axis.text.y = element_text(color = "black"),
            legend.title = element_blank(),
            legend.position = "bottom"
        ) +
        labs(x = "Response date (November 2020)", y = "Emotional state index") +
        scale_color_manual(
            breaks = c("D", "R"),
            labels = c("Democrats", "Republicans"),
            values = c("blue", "red")
        ) +
        scale_fill_manual(
            breaks = c("D", "R"),
            labels = c("Democrats", "Republicans"),
            values = c("blue", "red")
        ) +
        scale_x_continuous(breaks = seq(4, 19, by = 3))
    save_plot(
        po = po,
        dir = "analysis/descriptive/output",
        fname = "outcome_by_party_by_date",
        width = 9,
        height = 6
    )
}


write_reliability_scalars <- function(cronbach_alpha_fb, cronbach_alpha_ig) {
    #' Writes the Cronbach's alpha values as scalars to a LaTeX file.
    #'
    #' @param cronbach_alpha_fb A scalar representing the Cronbach's alpha for Facebook.
    #' @param cronbach_alpha_ig A scalar representing the Cronbach's alpha for Instagram.
    #'
    scalars <- list(
        CronbachAlphaFB = cronbach_alpha_fb,
        CronbachAlphaIG = cronbach_alpha_ig
    )
    save_scalars_tex(
        scalars,
        filename = "analysis/descriptive/output/scalars/index_reliability_scalars.tex",
        digits = 2
    )
}


## Execute
main()
