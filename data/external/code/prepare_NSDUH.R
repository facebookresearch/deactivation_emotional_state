# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

######################################
###   external - prepare_NSDUH.R   ###
######################################

source("lib/compute_effects.R")
source("lib/constants.R")

library(dplyr)
library(haven)

main_sample_vars <- c(
  "participant_user_id",
  "weights_w2",
  "emot_pre",
  "participant_male_or_not",
  "participant_age_6_groups",
  "participant_has_degree",
  "minority",
  "participant_income"
)

main <- function() {
  print("Preparing NSDUH data...")
  # create pooled NSDUH dataset (2008, 2009, 2021, 2022) for K6 index
  NSDUH_pooled <- create_pooled_NSDUH_df(k6_variables) 
  saveRDS(NSDUH_pooled, "data/external/output/pooled_NSDUH.rds")

  # create harmonized NSDUH 2020 for comparison with main sample
  N_20 <- load_NSDUH_yr("2020")
  N_20 <- N_20 %>%
    process_NSDUH_outcomes(k6_variables, weight_var = "ANALWTQ1Q4_C") %>%
    harmonize_NSDUH_covars() %>%
    select(participant_has_degree, income_bins, age_bins, minority, participant_male_or_not, k6_avg)

  # process main sample
  main_sample_fb <- readRDS("data/external/input/main_sample_fb.rds")
  main_sample_ig <- readRDS("data/external/input/main_sample_ig.rds")
  main_sample <- bind_rows(main_sample_fb, main_sample_ig) %>%
    select(all_of(main_sample_vars)) %>%
    harmonize_main_sample_covars()

  # Export harmonized_NSDUH_2020.csv
  saveRDS(N_20, "data/external/output/harmonized_NSDUH_2020.rds")
  saveRDS(main_sample, "data/external/output/harmonized_NSDUH_main_sample.rds")
}

load_NSDUH_yr <- function(year) {
  #' A helper function to load NSDUH data for a given year.
  #'
  #' @param year A character string representing the year of the NSDUH data to load.
  #'
  #' @return Loaded NSDUH data for the given year.
  #'
  df <- read_dta(str_c("data/external/input/NSDUH/NSDUH_", year, ".dta"))
  return(df)
}

create_pooled_NSDUH_df <- function(var_questions) { 
  #' Proccess NSDUH data for 2008, 2009, 2021, 2022 and pool them together.
  #'
  #' @param var_questions A character vector of the K6 questions to be standardized.
  #'
  #' @return Pooled NSDUH data with standardized K6 questions and index.
  #'
  N_08 <- process_df_yr_k6(
    year = "2008",
    weight_var = "MHSAWT_C",
    id_prefix = "N08",
    var_questions
  )
  N_09 <- process_df_yr_k6(
    year = "2009",
    weight_var = "ANALWT_C",
    id_prefix = "N09",
    var_questions
  )
  N_21 <- process_df_yr_k6(
    year = "2021",
    weight_var = "ANALWT2_C",
    id_prefix = "N21",
    var_questions
  )
  N_22 <- process_df_yr_k6(
    year = "2022",
    weight_var = "ANALWT2_C",
    id_prefix = "N22",
    var_questions
  )
  N_pooled <- bind_rows(N_22, N_21, N_09, N_08)
  # standardize the K6 questions using within year normalized weights
  N_pooled <- process_NSDUH_outcomes(N_pooled, var_questions, "weight")
  return(N_pooled)
}


process_df_yr_k6 <- function(year, weight_var, id_prefix, var_questions) {
  #' Process individual year NSDUH data for K6 questions, normalizing weights.
  #' Restricts to age range 18 - 25.
  #'
  #' @param year A character string representing the year of the NSDUH data to load.
  #' @param weight_var A character string representing the weight variable in the NSDUH data.
  #' @param id_prefix A character string representing the prefix to be added to the ID.
  #' @param var_questions A character vector of the K6 outcome questions of interest.
  #'
  #' @return A data frame with unique id, K6 questions, and normalized weights.
  #'
  var_select <- c("QUESTID2", "catage", weight_var, var_questions)
  var_std <- c("id", "catage", "weight", var_questions)
  dt <- load_NSDUH_yr(year) %>%
    select(all_of(var_select)) %>%
    # Filtering for age range: 18 - 25.
    filter(catage == 2) %>%
    rename_with(~var_std, everything()) %>%
    mutate(id = str_c(id_prefix, id), sample_year = id_prefix) %>%
    drop_na()
  dt <- dt %>% filter(if_all(var_questions, ~ . >= 1 & . <= 5))
  # Normalize weights within sample years by mean weight of given year.
  dt <- dt %>% mutate(weight = weight / mean(weight))
  return(dt)

}

process_NSDUH_outcomes <- function(data, var_questions, weight_var) {
  #' Standardize K6 questions and compute average on pooled sample.
  #'
  #' @param data A data frame with pooled NSDUH data.
  #' @param var_questions A character vector of the K6 outcome questions of interest.
  #' @param weight_var A character string representing the new weight variable in pooled sample.
  #'
  #' @return A data frame with standardized K6 questions and index (`k6_avg`).
  #'
  data <- data %>%
    # keep only responses in the valid range (1 - 5).
    filter(if_all(var_questions, ~ . >= 1 & . <= 5)) %>%
    # standardize individual questions
    mutate(across(
      all_of(var_questions),
      ~ standardize_survey_question(., weight = !!sym(weight_var))
    )) %>%
    # compute average and standardize
    mutate(k6_avg = rowMeans(across(all_of(var_questions)))) %>%
    mutate(k6_avg = standardize_survey_question(k6_avg, weight = !!sym(weight_var))) %>%
    select(-all_of(var_questions))
  return(data)
}

harmonize_NSDUH_covars <- function(data) {
  #' Harmonize NSDUH covariates for comparison with main sample.
  #'
  #' @param data A data frame with NSDUH data from 2020.
  #'
  #' @return An updated data frame with covariates harmonized to main sample levels for comparison.
  #'
  data <- data %>%
    mutate(
      # IREDUHIGHST2 = 11 is College graduate or higher
      participant_has_degree = ifelse(IREDUHIGHST2 == 11, 1, 0),
      participant_has_degree = case_when(
        participant_has_degree == 1 ~ "College degree",
        participant_has_degree == 0 ~ "No college degree"
      ),
      income_bins = case_when(
        # Less than $50,000
        IRPINC3 %in% 1:5 ~ "Lower income",
        # $50,000 or more
        IRPINC3 %in% 6:7 ~ "Upper and middle income"
      ),
      # Compute age using the midpoints of the ranges
      age = case_when(
        AGE2 == 1 ~ 12,
        AGE2 == 2 ~ 13,
        AGE2 == 3 ~ 14,
        AGE2 == 4 ~ 15,
        AGE2 == 5 ~ 16,
        AGE2 == 6 ~ 17,
        AGE2 == 7 ~ 18,
        AGE2 == 8 ~ 19,
        AGE2 == 9 ~ 20,
        AGE2 == 10 ~ 21,
        AGE2 == 11 ~ 22.5,
        AGE2 == 12 ~ 24.5,
        AGE2 == 13 ~ 27.5,
        AGE2 == 14 ~ 32,
        AGE2 == 15 ~ 42,
        AGE2 == 16 ~ 57,
        AGE2 == 17 ~ 75
      ),
      age_bins = case_when(
        age >= 18 & age < 35 ~ "Age 18-34",
        age > 34  ~ "Age 35+"
      ),
      # As in the paper, we define minority as Black or Hispanic
      minority = ifelse(NEWRACE2 == 2 | NEWRACE2 == 7, 1, 0),
      minority = case_when(
        minority == 1 ~ "Minority",
        minority == 0 ~ "Non-minority"
      ),
      # Create sex variable
      participant_male_or_not = ifelse(IRSEX == 1, 1, 0),
      participant_male_or_not = case_when(
        participant_male_or_not == 1 ~ "Male",
        participant_male_or_not == 0 ~ "Female"
      )
    ) %>%
    select(-c(IRSEX, CATAGE, IREDUHIGHST2, IRPINC3, AGE2, NEWRACE2))
  return(data)
}

harmonize_main_sample_covars <- function(data) {
  #' Harmonize main sample covariates for comparison with NSDUH
  #'
  #' @param data A data frame with main sample data.
  #'
  #' @return An updated data frame with covariates harmonized to NSDUH levels for comparison.
  #'
  data <- data %>%
    mutate(
      # Harmonize college variable
      participant_has_degree = as.numeric(participant_has_degree),
      participant_has_degree = case_when(
        participant_has_degree == 1 ~ "College degree",
        participant_has_degree == 0 ~ "No college degree"
      ),
      # Harmonize participant_income variable
      income_bins = case_when(
        participant_income == "lower" ~ "Lower income",
        participant_income %in% c("upper", "mid") ~ "Upper and middle income"
      ),
      # Harmonize age variable
      age_bins = case_when(
        participant_age_6_groups %in% c("18-24", "25-34") ~ "Age 18-34",
        participant_age_6_groups %in% c("35-44", "45-54", "55-64", "65+") ~ "Age 35+"
      ),
      # Harmonize minority variable
      minority = as.numeric(minority),
      minority = case_when(
        minority == 1 ~ "Minority",
        minority == 0 ~ "Non-minority"
      ),
      # Harmonize participant_gender variable
      participant_male_or_not = case_when(
        participant_male_or_not == "male" ~ "Male",
        participant_male_or_not == "not male" ~ "Female"
      )
    )
  return(data)
}


## Execute
main()
