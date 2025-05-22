# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

#' Create a table with the distribution of a set of covariates from the log data,
#' comparing control and treatment groups.
#'
#' @param data (data frame) Data frame that contains survey data
#' @param var_labels (character vector) Labels for each variable
#' @param vars (character vector) Names of covariates in dataset whose
#' distribution wants to be dislayed in the table
#' @param control (character) Name of control group
#' @param control_label (character) Label for control group
#' @param trt (character vector) Name of treatment group(s)
#' @param trt_labels (character vector) Labels for treatment group(s)
#' @param digits (numeric) Number of decimal digits to display in table
#' @param proportion (logical) Whether output is a percentage or not. If \code{TRUE},
#' quantities will be multiplied by 100.
#' @param listwise_deletion (logical) Whether all metrics should be computed
#' for the same dataset; which implies that units with a missing value in any
#' of the variables will be excluded for all variables. If \code{FALSE}, each
#' metric is computed for the non-missing values of that variable.
#' @param lasso_covars (character vector) List of covariates to add to lasso
#' model for selection in covariate-adjusted p-value.
#' @param model_covars (character vector) List of covariates to add to baseline
#' model, regardless of whether they were selected by the lasso or not, in
#' order to compute covariate-adjusted p-value.
#' @param caption (character) Caption to be added to the latex table
#' @param output_file (character) Name of file where table will be stored (in
#' latex format)
#' @param append (logical) If \code{TRUE}, output table will be appended to
#' existing file. Otherwise, the file will be overwritten.
#' @param verbose (logical) If \code{TRUE}, the function will output additional
#' information, such as the proportion of units with missing values.
#'
#' @export
#'
#'
descriptive_table <- function(data, var_labels, vars, control, control_label,
                              trt, trt_labels, digits=2, proportion=FALSE,
                              listwise_deletion = TRUE,
                              lasso_covars, model_covars,
                              caption, output_file, append=FALSE, verbose=TRUE){

  if (listwise_deletion){
    # for some tables, we want to have the same set of respondents for all metrics,
    # in which case we need listwise deletion
    d <- data %>% select(treatment_group, all_of(c(vars, lasso_covars, model_covars))) %>%
      # deleting NAs / Inf values
      na.omit() %>% filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
  }
  if (!listwise_deletion){
    # in some other cases, when % missing values varies a lot, we may not want that behavior
    d <- data %>% select(treatment_group, all_of(c(vars, lasso_covars, model_covars)))
    # but we should still recode Inf values to NA
    for (var in vars){ d[,var][is.infinite(d[,var])] <- NA }
  }
  if (verbose){
    deleted <- nrow(data)-nrow(d)
    msg <- paste0(deleted, ' observations (',
                  round(100*deleted/nrow(data),2) ,'\\%) dropped by listwise deletion.')
    message(msg)
  }

  out <- list()
  i <- 1
  # multiply by 100 if variables are proportions
  p <- ifelse(proportion, 100, 1)

  for (v in 1:length(vars)){

    # first row: control group
    out[[i]] <- matrix(c(
      var_labels[v],
      control_label,
      # metrics defined in pre-registration
      round(p*quantile(d[d$treatment_group==control,vars[v]], probs=0.05, na.rm=TRUE), digits),
      round(p*quantile(d[d$treatment_group==control,vars[v]], probs=0.50, na.rm=TRUE), digits),
      round(p*quantile(d[d$treatment_group==control,vars[v]], probs=0.95, na.rm=TRUE), digits),
      round(p*mean(d[d$treatment_group==control,vars[v]], na.rm=TRUE), digits),
      round(p*sd(d[d$treatment_group==control,vars[v]], na.rm=TRUE), digits),
      sum(!is.na(d[d$treatment_group==control,vars[v]])),
      '--'
    ), ncol=9, byrow=TRUE)
    i <- i + 1

    # row 2 and beyond: each treatment group
    for (r in 1:length(trt)){
      # treatment effect estimate using pre-registered approach
      suppressMessages(mod <- baseline_model(d=d[d$treatment_group %in% c(control, trt[r]),],
                                             dv=vars[v],
                                             lasso_covars=lasso_covars,
                                             model_covars=model_covars,
                                             standardize=FALSE,
                                             weights_var=NULL,
                                             model_type="SATE",
                                             trt=trt[r], control=control, dv_label = var_labels[v]))
      # NOTE: this will compute PATE as well, but for descriptives we generally
      # only want SATE
      p.value <- mod$pval
      # creating row
      out[[i]] <- matrix(c(
        '',
        trt_labels[r],
        round(p*quantile(d[d$treatment_group==trt[r],vars[v]], probs=0.05, na.rm=TRUE), digits),
        round(p*quantile(d[d$treatment_group==trt[r],vars[v]], probs=0.50, na.rm=TRUE), digits),
        round(p*quantile(d[d$treatment_group==trt[r],vars[v]], probs=0.95, na.rm=TRUE), digits),
        round(p*mean(d[d$treatment_group==trt[r],vars[v]], na.rm=TRUE), digits),
        round(p*sd(d[d$treatment_group==trt[r],vars[v]], na.rm=TRUE), digits),
        sum(!is.na(d[d$treatment_group==trt[r],vars[v]])),
        ifelse(p.value <0.01,
               'p<0.01',
               paste0('p=', round(p.value, 2))
        )
      ), ncol=9, byrow=TRUE)
      i <- i + 1
    }
  }
  out <- do.call(rbind, out)
  colnames(out) <- c('Metric', 'Group', 'p5', 'p50', 'p95', 'Avg.', 'SD', 'N', 'diff')
  tabnote <- list(
    pos=list(nrow(out)),
    command=ifelse(
      # add % missing as table note if using listwise deletion
      listwise_deletion, paste0("\\hline\\multicolumn{8}{c}{", msg, "}\\\\"),
      # else, add just a line
      "\\hline"))
  print(xtable(out, caption=caption, label=gsub('_', '', vars[1])), type='latex', include.rownames=FALSE,
        caption.placement = "top", file=output_file, append=append,
        hline.after=c(-1,0),
        add.to.row=tabnote)
}


#' Create a table with summary statistics for each variable in data.frame,
#' for debugging purposes
#'
#' @param data (data frame) Data frame that contains relevant dataset
#' @param to_exclude (numeric vector) Columns in data frame that should be
#' excluded from table (e.g. character variables, DS, etc.)
#' @param output_file (character) Name of file where table will be stored (in
#' latex format)
#' @param verbose (logical) If \code{TRUE}, the function will output additional
#' information, such as the proportion of units with missing values.
#'
#' @export
#'
qa_table <- function(data, to_exclude, output_file, verbose=TRUE){
  # convert to data.frame format
  data <- as.data.frame(data)
  mat <- list()
  # columns for which metrics will be computed
  cols <- (1:ncol(data))[-to_exclude]
  for (i in cols){
    var <- names(data)[i]
    if(verbose) message(i, '/', ncol(data)-1, ': ', var)
    # unmap map variables
    if (grepl('_map', var)){
      summ <- list()
      map <- parse_map(data[,var])
      keys <- names(map[[1]])
      if (!all(keys == names(map[[2]]))){
        # if keys vary across rows, get all unique keys and sort them
        keys <- sort(unique(unlist(sapply(map, names))))
      }
      for (k in keys){
        dd <- unmap(map, key=k)
        summ[[k]] <- c(paste0(var, '_', k),
                       round(min(dd, na.rm=TRUE), 2),
                       round(mean(dd, na.rm=TRUE), 2),
                       round(median(dd, na.rm=TRUE), 2),
                       round(max(dd, na.rm=TRUE),0),
                       sum(dd==0, na.rm=TRUE),
                       sum(is.na(dd)))
      }
      summ <- do.call(rbind, summ)
      mat[[i]] <- summ
    }
    # for non-map variables, compute descriptives directly
    if (!grepl('_map', var)){
      dd <- data[,var]
      mat[[i]] <- c(var,
                    round(min(dd, na.rm=TRUE), 2),
                    round(mean(dd, na.rm=TRUE), 2),
                    round(median(dd, na.rm=TRUE), 2),
                    round(max(dd, na.rm=TRUE),0),
                    sum(dd==0, na.rm=TRUE),
                    sum(is.na(dd)))
    }
  }
  mat <- do.call(rbind, mat)
  # output fomat
  colnames(mat) <- c("variable", "min", "mean", "median", "max", "0s", "NAs")
  addtorow          <- list()
  addtorow$pos      <- list()
  addtorow$pos[[1]] <- c(0)
  addtorow$command  <- c(paste(
    "\\hline \n",
    "\\endhead \n",
    "\\hline \n",
    "{\\footnotesize Continued on next page} \n",
    "\\endfoot \n",
    "\\endlastfoot \n",
    sep=""))
  # create latex table
  print(xtable(mat,
               caption="Descriptive statistics"),
        type='latex',
        include.rownames=FALSE,
        caption.placement = "top",
        file=output_file,
        add.to.row = addtorow,
        floating=FALSE,
        hline.after=c(-1), tabular.environment="longtable")
}

