# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

#' Combine dataset with attrition file
#'
#' @param d (data frame) Data frame that contains the data with a boolean variable 'treated'
#' @param dv (character) Name of the DV
#' @param covars (character vector) List of covariates for predicting outcome
#' @param original_lasso_covars (character vector) List of covariates run through
#' selection by lasso. Default NULL
#' @param completed_var (character) Name of the boolean variable for whether the
#' user completed the study or dropped out of the study.  If NULL (default), assumes instances
#' where DV is NULL represent users who dropped out of the study.
#' @param weights_var (character) Optional. Name of the weights variable. Default NULL.
#'
#' @export
combine.attrition <- function(
    d,
    covars,
    cols_to_keep = NULL,
    weights_var = NULL,
    treatment_var = 'treatment_group',
    trt,
    control,
    factor_to_bin = TRUE,
    impute = TRUE,
    deact_impute = FALSE,
    treated_bin = TRUE,
    standardize = TRUE,
    proportion = FALSE,
    completed_var = NULL,
    verbose = FALSE
) {
    fake_treatment <- ('true_treat' %in% names(d))

    demogs <- read_file(
        filename='replication-deactivation/all_users_demographics_reduced_replication.csv',
        has_header=TRUE
    )

    special_cols = c('partyid','affpol_pre','issuepol_pre','baseline_ts_fb','baseline_ts_ig')

    demogs <- demogs %>%
        select(participant_user_id, is_matched, study_type, platform, any_of(covars), any_of(cols_to_keep), any_of(weights_var), any_of(special_cols))

    if(verbose) message("Demogs vars trimmed: ", paste0(names(demogs),sep=', '))

    # join to input data
    # filter to platform & study types of matched users
    study_types <- unique(demogs %>% filter(participant_user_id %in% d$participant_user_id) %>% select(study_type))$study_type
    p.platform <- unique(demogs %>% filter(participant_user_id %in% d$participant_user_id) %>% select(platform))$platform

    if(!fake_treatment) {
        treatment <- unique(demogs %>% filter(participant_user_id %in% subset(d, treated==1)$participant_user_id) %>% select(study_type))$study_type
    }

    if (p.platform == 'FB') {
        demogs <- demogs %>%
            mutate(baseline_use_plat_win = alethea::winsorize(baseline_ts_fb, pct = 0.99))
    } else {
        demogs <- demogs %>%
            mutate(baseline_use_plat_win = alethea::winsorize(baseline_ts_ig, pct = 0.99))
    }

    # verify covariates are in dataframe
    covs <- intersect(covars, names(d))
    if(!identical(covs, covars)) {
        stop(paste0("Covariates not found in dataframe:", setdiff(covs, names(d)), " ", setdiff(names(d), covs), collapse=", "))
    }

    covars <- unique(gsub("_W[1-6]","",covars))

    # verify covariates are in pre-attrition dataframe
    covs <- intersect(covars, names(demogs))
    if(!identical(covs, covars)) {
        stop(paste0("Contact winteram; covariates not in pre-attrition dataframe:", setdiff(covs, names(demogs))))
    }

    # special imputation for affective and issue polarization baselines
    demogs <- demogs %>%
        mutate(affpol_pre   = case_when(partyid %in% c('D', 'R') & !is.na(affpol_pre) ~ affpol_pre,
                                        partyid %in% c('D', 'R') &  is.na(affpol_pre) ~ mean(affpol_pre, na.rm=TRUE),
                                        !(partyid %in% c('D', 'R')) ~ NA_real_,
                                        TRUE ~ NA_real_),
            issuepol_pre = case_when(partyid %in% c('D', 'R') & !is.na(issuepol_pre) ~ issuepol_pre,
                                        partyid %in% c('D', 'R') &  is.na(issuepol_pre) ~ mean(issuepol_pre, na.rm=TRUE),
                                        !(partyid %in% c('D', 'R')) ~ NA_real_,
                                        TRUE ~ NA_real_)
            )

    # creating binary vars for factor vars
    if (factor_to_bin){
        factor_bins     <- make_factor_bins(d = demogs, covars = covars)
        demogs          <- factor_bins$d
        lasso_covars    <- factor_bins$updated_covars
    }

    if(verbose) message("Lasso covariates: ", paste0(lasso_covars,sep=', '))

    # imputation of pre-treament covariates
    if (impute) {
        imputed_covars  <- impute_covars(demogs, covars = lasso_covars, deact_impute = deact_impute)
        demogs          <- imputed_covars$d
        lasso_covars    <- imputed_covars$updated_covars
    }


    # reduce dataframes to key variables
    demogs <- demogs %>%
        filter(study_type %in% study_types) %>%
        filter(platform == p.platform) %>%
        rename(completed = is_matched)

    if(fake_treatment) {
        demogs$treated <- rbinom(n=nrow(demogs), size=1, p=0.25)
    } else {
        demogs <- demogs %>%
            mutate(
                treated = study_type == treatment
            )
    }
    demogs[,cols_to_keep] <- NA
    demogs[,`weights_var`] <- NA
    demogs[,`treatment_var`] <- NA

    # get completed based on outcome variable, if not provided
    if(is.null(completed_var)) {
        d$completed <- TRUE
    } else {
        d <- d %>% rename(completed = `completed_var`)
    }

    demogs.trimmed <- demogs %>%
            select(participant_user_id, any_of(treatment_var), completed, treated, any_of(lasso_covars), any_of(covars), any_of(cols_to_keep), any_of(weights_var))
    d.trimmed <- d %>%
            select(participant_user_id, any_of(treatment_var), completed, treated, any_of(lasso_covars), any_of(covars), any_of(cols_to_keep), any_of(weights_var))

    if(!identical(names(demogs.trimmed), names(d.trimmed))) {
        message("Vars in demogs not in d: ", paste0(setdiff(names(demogs.trimmed), names(d.trimmed))))
        message("Vars in d not in demogs: ", paste0(setdiff(names(d.trimmed), names(demogs.trimmed))))
        stop("Mismatch in variables")
    }

    message('Added rows for attritted participants')

    # Add rows for users who withdrew & deleted data
    return(bind_rows(d, demogs.trimmed %>% filter(!completed)))
}


#' Estimate Lee (2009) bounds with covariates
#'
#' This function uses the process defined in Lee (2009) to estimate bounds on the ATE
#' given an outcome, treatment indicator, study-completion indicator, and covariates.
#' Outcomes are predicted for all rows given covariates, users are binned into n_buckets by
#' predicted outcome, and within each bucket the sorted outcomes are trimmed for the group
#' with fewer attritted users to identify the "always-takers".
#'
#' @param d (data frame) Data frame that contains the data with a boolean variable 'treated'
#' @param dv (character) Name of the DV
#' @param covars (character vector) List of covariates for predicting outcome for lee buckets
#' @param weights_var (character) Optional. Name of the weights variable. Defaul NULL.
#' @param n_buckets (numeric) Number of buckets to split predicted outcome. Must
#' be coarse enough to allow trimming of attritted users in each bucket. Default is 10.
#'
#' @export
leebounds.covar <- function(
    d,
    dv,
    covars,
    weights_var = NULL,
    n_buckets = 10
) {

    if (!require(tidyr)) {
        install.packages('tidyr')
        library(tidyr)
    }

    stopifnot("Outcome variable not in dataframe"=dv %in% names(d))
    stopifnot("Treatment variable not in dataframe"='treated' %in% names(d))

    d$completed <- !is.na(d[,dv])

    # adding tiny noise to differentiate discrete outcomes
    set.seed(2020)
    outcome.noise.factor <- 0.001*10^(floor(log10(mean(abs(d[,dv]),na.rm=T))))
    d$noised.outcome <- d[,dv] + runif(nrow(d),-outcome.noise.factor,outcome.noise.factor)

    # Model outcome using covariates
    outcome.formula <- formula(paste0('noised.outcome ~ ', paste(covars, collapse=" + ")))

    if(!is.null(weights_var)) {
        d <- d %>% rename(weights = `weights_var`)
    } else {
        d$weights = rep(1,nrow(d))
    }

    outcome.fit <- lm(outcome.formula, data=d, na.action=na.exclude, weights=d$weights)

    # Predict outcome using model
    d$predicted_out <- predict(outcome.fit, d)

    # create buckets of predicted outcome
    pred.buckets <- unique(quantile(d$predicted_out, prob=0:n_buckets/n_buckets, na.rm=TRUE))
    d <- d %>%
        mutate(
            predicted_bucket = cut(
                predicted_out,
                breaks = pred.buckets,
                labels = 1:(length(pred.buckets)-1),
                include.lowest = TRUE
            )
        )

    weight.factor <- 10000/(10^(ceiling(log10(max(d$weights,na.rm=T)))))

    # send each bucket to leebounds
    upper.bounds <- c()
    lower.bounds <- c()
    for (i in 1:n_buckets) {
        lee <- d %>%
            filter(predicted_bucket==i) %>%
            select(noised.outcome, completed, treated, weights) %>%
            rename(
                treat = treated,
                selection = completed,
                outcome = noised.outcome
            ) %>%
            tidyr::drop_na(weights) %>%
            mutate(
                weights = weights * weight.factor
            )
        tmp <- leebounds(lee)
        upper.bounds <- c(upper.bounds, tmp$upper_bound)
        lower.bounds <- c(lower.bounds, tmp$lower_bound)
    }

    bounds <- data.frame(bucket=1:n_buckets, lower=lower.bounds, upper=upper.bounds)
    lee.main <- d %>%
        group_by(treated, predicted_bucket) %>%
        summarise(
            outcome = weighted.mean(get(dv),weights,na.rm=T),
            n.obs = sum(completed)
        ) %>%
        pivot_wider(
            id_cols='predicted_bucket',
            names_from='treated',
            values_from=c('outcome','n.obs'),
            names_glue = '{.value}.{treated}'
        ) %>%
        mutate(
            ate = outcome.1 - outcome.0,
            n.obs = n.obs.1 + n.obs.0
        )
    bounds <- merge(bounds, lee.main, by.x='bucket', by.y='predicted_bucket')

    full.lee <- leebounds(
        d %>%
            select(noised.outcome, completed, treated, weights) %>%
            rename(
                treat = treated,
                selection = completed,
                outcome = noised.outcome
            ) %>%
            tidyr::drop_na(weights) %>%
            mutate(
                weights = weights * weight.factor
            )
    )

    return( 
        list(
            covar.lower=weighted.mean(bounds$lower, bounds$n.obs),
            covar.upper=weighted.mean(bounds$upper, bounds$n.obs),
            covar.ate=weighted.mean(bounds$ate, bounds$n.obs),
            full.lower=full.lee$lower_bound,
            full.upper=full.lee$upper_bound,
            bounds=bounds,
            covar.buckets = d[, c("participant_user_id", "predicted_out","predicted_bucket")]
        ) 
    )
}
