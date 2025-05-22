# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

######################################
###   external - prepare_ESS_6.R   ###
######################################

source("lib/compute_effects.R")

es_questions <- c(
  "wrhpp", "fltdpr", "fltanx"
)

past_week_questions <- c(
  "flteeff", "slprl", "fltlnl", "enjlf",
  "fltsd", "cldgng", "enrglot", "fltpcfl",
  es_questions
)

wellbeing_questions <- c(
  "optftr", "pstvms", "flrms"
)

main <- function() {
  print("Preparing ESS-6 data...")
  ESS6 <- read.csv("data/external/input/ESS/ESS6e02_6-subset.csv")
  ESS6 <- process_E6(ESS6)
  saveRDS(ESS6, "data/external/output/ess6_data.rds")
}

process_E6 <- function(df) {
  #' Process ESS-6 data, standardizing survey questions and creating indices.
  #'
  #' @param df A data frame with raw ESS-6 data.
  #'
  #' @return An updated data frame with standardized survey questions and indices.
  #'
  all_questions <- c(past_week_questions, wellbeing_questions)
  df <- df %>%
      mutate(across(all_of(all_questions), ~ standardize_survey_question(.x, anweight)))

  # create indices
  df <- df %>%
    mutate(
      # create ES index
      es_index = rowMeans(select(df, all_of(es_questions)), na.rm = TRUE),
      es_index = standardize_survey_question(es_index, anweight),
      # create past week index
      past_week_index = rowMeans(select(df, all_of(past_week_questions)), na.rm = TRUE),
      past_week_index = standardize_survey_question(past_week_index, anweight),
      # create all index
      all_index = rowMeans(select(df, all_of(all_questions)), na.rm = TRUE),
      all_index = standardize_survey_question(all_index, anweight)
    )
  return(df)
}


## Execute
main()
