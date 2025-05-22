# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

##############################################
###   comparison - comparison_K6_NSDUH.R   ###
##############################################

library(tidyr)
library(haven)
library(Hmisc)
library(stringr)
library(dplyr)

source("lib/compute_effects.R")
source("lib/prepare_outputs.R")
source("lib/constants.R")

main <- function() {
  secondary_HTEs_ig <- readRDS("analysis/comparison/input/secondary_HTEs_ig.rds")
  NSDUH_pooled <- readRDS("analysis/comparison/input/pooled_NSDUH.rds")

  NSDUH_K6_summary <- NSDUH_pooled %>%
    group_by(sample_year) %>%
    summarise(
      mean_group = weighted.mean(k6_avg, weight),
      std_group = Hmisc::wtd.var(k6_avg, weight) %>% sqrt()
    )
  secondary_HTEs_ig <- bind_rows(secondary_HTEs_ig)
  compare_NSDUH_effects(secondary_HTEs_ig, NSDUH_K6_summary)
}

# Function to compare primary effects to NSDUH outcomes.
compare_NSDUH_effects <- function(secondary_HTEs_ig, NSDUH_K6_index) {
  #' Compares HTE for 18-24 years olds in IG to NSDUH K6 change from 2008 to 2022.
  #' Writes scalars to LaTeX files.
  #'
  #' @param secondary_HTEs_ig A data frame containing the secondary HTEs for IG.
  #' @param NSDUH_K6_index A data frame containing the K6 index summary statistics.
  #'
  # Get Kessler-6 change from 2008 to 2022.
  mean_k6_08 <- NSDUH_K6_index %>%
    filter(sample_year == "N08") %>%
    pull(mean_group)
  mean_k6_22 <- NSDUH_K6_index %>%
    filter(sample_year == "N22") %>%
    pull(mean_group)
  diff_k6 <- abs(mean_k6_22 - mean_k6_08)

  # Compare relative to IG effect
  ig_effect <- secondary_HTEs_ig %>%
    filter((group == "age_above_med") & (subgroup == FALSE)) %>%
    pull(ate) %>%
    abs()

  benchmark_ratio <- 100 * (ig_effect / diff_k6)
  scalars_k6 <- list(
    "KesslerIndexChange" = diff_k6
  )
  scalars_k6_int <- list(
    "KesslerBenchmarkPctIG" = benchmark_ratio
  )
  scalars <- c(scalars_k6, scalars_k6_int)
  save_scalars_tex(
    scalars,
    digits = c(2, 0),
    filename = "analysis/comparison/output/scalars/k6_change_scalars.tex"
  )
}

## Execute
main()
