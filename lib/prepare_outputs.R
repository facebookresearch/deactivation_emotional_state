# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

###################################
###   lib - prepare_outputs.R   ###
###################################

library(ggplot2)
library(dplyr)
library(forcats)

source("lib/constants.R")

build_outcome_subplot <- function(
  res,
  y_lab,
  y_lim = NULL,
  y_lim_lo = NULL,
  y_lim_hi = NULL,
  width = 0.25,
  group_var = NULL,
  no_labels = FALSE
) {
  #' Builds a subplot for treatment effects for outcomes for a recruitment_platform_string.
  #' 
  #' @param res A dataframe of treatment effects for all outcomes.
  #' @param y_lab A string indicating the y-axis label.
  #' @param y_lim A double indicating the y-axis limits (if equal for both sides).
  #' @param y_lim_lo A double indicating the lower y-axis limit.
  #' @param y_lim_hi A double indicating the upper y-axis limit.
  #' @param width A double indicating the width of the error bars.
  #' @param group_var A string indicating the variable to use for grouping (for color). Default is NULL.
  #' @param no_labels A boolean indicating whether to remove labels from the plot.
  #'
  #' @return A ggplot object capturing recruitment_platform_string-specific treatment effects.
  #'
  if (is.null(y_lim_lo)) {
    y_lim_lo <- -y_lim
  }
  if (is.null(y_lim_hi)) {
    y_lim_hi <- y_lim
  }
  # make main plot object, with color group if group_var is not null
  if (!is.null(group_var)) {
    po <- ggplot(res, aes(x = forcats::fct_rev(dv_label), y = ate, color = get(group_var))) +
      geom_point(shape = 16, position = position_dodge(width = 0.5)) +
      geom_errorbar(
        aes(ymin = ci_low, ymax = ci_hi, color = get(group_var)),
        width = width,
        position = position_dodge(width = 0.5)
      )
  } else {
    po <- ggplot(res, aes(x = forcats::fct_rev(dv_label), y = ate)) +
      geom_point(shape = 16, position = position_dodge(width = 0.5), color = maroon) +
      geom_errorbar(
        aes(ymin = ci_low, ymax = ci_hi),
        width = width,
        position = position_dodge(width = 0.5),
        color = maroon
      )
  }
  po <- po +
    coord_flip() +
    scale_y_continuous(limits = c(y_lim_lo, y_lim_hi)) +
    geom_hline(yintercept = 0, color = "black") +
    theme_bw() +
    theme(
      axis.title = element_text(size = rel(0.9)),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.border = element_blank(),
      axis.text.x = element_text(color = "black"),
      axis.text.y = element_text(color = "black")
    ) +
    labs(x = "", y = y_lab)
   if (no_labels) po <- po + theme(axis.text.y = element_blank())
   return(po)
}


extract_regs_vals <- function(lm, digits = 3) {
  #' Helper function to extract and format regression coefficients and standard errors.
  #'
  #' @param lm A fitted linear model object.
  #' @param digits An integer indicating the number of decimal places to round to. Default is 3.
  #'
  #' @return A vector of formatted coefficient and standard error values.
  #'
  coef_values <- formatC(coef(lm), format = "f", digits = digits)
  se_values <- paste0("(", formatC(sqrt(diag(vcov(lm))), format = "f", digits = digits), ")")
  return(c(rbind(coef_values, se_values)))
}


regs_vals_to_df <- function(regs) {
  #' Helper function to turn a list of regression models into a dataframe.
  #' Note: RHS vars must be the same across models (and in same order).
  #'
  #' @param regs A list of regression models.
  #'
  #' @return A dataframe of regression coefficients and standard errors.
  #'
  vars <- names(coef(regs[[1]]))
  coefs <- lapply(regs, extract_regs_vals)
  formatted_data <- bind_cols(rep(vars, each = 2), coefs,
      .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE),
  )
  return(formatted_data)
}


save_plot <- function(po, dir, fname, width = 9, height = 6) {
  #' Saves a plot to a specified path as a PNG and PDF.
  #'
  #' @param po A ggplot object.
  #' @param dir A string indicating the directory to save the plot to.
  #' @param fname A string indicating the filename to save the plot as.
  #' @param width A double indicating the width of the plot in inches.
  #' @param height A double indicating the height of the plot in inches.
  #'
  file_formats <- c("png", "pdf")
  for (file_format in file_formats) {
    path <- paste0(dir, "/", file_format, "s/", fname, ".", file_format)
    ggsave(
      plot = po,
      filename = path,
      device = file_format,
      width = width,
      height = height
    )
  }
}


save_scalars_tex <- function(scalar_list, filename, scalar_names = NULL, digits = 2) {
  #' Saves a list of scalars to a LaTeX file at a desired level of precision.
  #'
  #' @param scalar_list A named list of scalars. Names will be used as variable names,
  #'  unless specified in 'scalar_names'.
  #' @param filename A string indicating the filename to save the LaTeX file as.
  #' @param scalar_names A named list of variable names to use in the LaTeX file. Default is NULL.
  #' @param digits An integer or list of integers indicating the decimal place to round scalar to.
  #'  If integer, all scalars will be rounded to the same number of digits.
  #'
  # some error checking
  if (!is.list(scalar_list)) {
    stop("'scalar_list' should be a list (even if it has only one element).")
  }
  # If digits is longer than 1, ensure it matches length of scalar_list
  if (length(digits) != 1 && length(digits) != length(scalar_list)) {
    stop("If 'digits' has length > 1, it must be the same length as 'scalar_list'.")
  }
  tex_string <- ""
  for (i in seq_along(scalar_list)) {
    x <- names(scalar_list)[i]
    name <- x
    # if scalar_names is provided, use it for name instead
    if (x %in% names(scalar_names)){
      name <- scalar_names[[x]]
    }
    # get desired number of digits to format with
    if (length(digits) == 1) {
      digits_fmt <- digits
    } else {
      digits_fmt <- digits[[i]]
    }
    if (is.character(scalar_list[[x]])) {
      digit_spec <- "%s"
    } else {
      digit_spec <- paste0("%.", digits_fmt, "f")
    }
    # add scalar to tex_string
    scalar_formatted <- sprintf(digit_spec, scalar_list[[i]])
    new_line   <- sprintf("\\newcommand{\\%s}{%s}", name, scalar_formatted)
    tex_string <- sprintf("%s\n%s", tex_string, new_line)
  }
  # saves it as a .tex file
  sink(file = filename)
  cat(tex_string)
  sink(NULL)
}
