# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

################################################
###   comparison - prepare_ESS_6_outputs.R   ###
################################################

library(dplyr)
library(xtable)
library(ggplot2)


### Main
main <- function() {
  ess6 <- readRDS("analysis/comparison/input/ess6_data.rds")

  # Create a table reporting the correlation matrix between es_index, past_week_index, all_index
  indices_vars <- c("es_index", "past_week_index", "all_index")
  indices_names <- c("Emotional state index", "Past week index", "All questions index")
  create_ESS6_correlation_matrix(ess6, indices_vars, indices_names)
}

create_ESS6_correlation_matrix <- function(df, vars, var_names) {
  #' Create a correlation matrix table between the ESS-6 outcome indices.
  #' Writes the table to 'analysis/comparison/output/tables/ESS_cor_matrix.tex'.
  #'
  #' @param df A data frame containing the ESS-6 data.
  #' @param vars A character vector of variable names to include in the correlation matrix.
  #' @param var_names A character vector of labels for variables.
  #'
  cor_matrix <- cor(select(df, vars), use = "pairwise.complete.obs")
  colnames(cor_matrix) <- var_names
  rownames(cor_matrix) <- var_names
  addtorow <- list(
    pos = list(0, 0),
    command = c(
      paste("&", paste0("(", seq(1, ncol(cor_matrix)), ")", collapse = " & "), "\\\\ \n"),
      "& Emotional state index & Past week index & All questions index \\\\"
    )
  )
  cor_table <- xtable(cor_matrix, label = "tab:cor_matrix")
  align(cor_table) <- list(
    "l",
    ">{\\centering\\arraybackslash}p{2.5cm}",
    ">{\\centering\\arraybackslash}p{2.5cm}",
    ">{\\centering\\arraybackslash}p{2.5cm}"
  )
  print(
    cor_table,
    file = "analysis/comparison/output/tables/ESS_cor_matrix.tex",
    floating = FALSE,
    include.colnames = FALSE,
    add.to.row = addtorow
  )
}

# Execute
main()
