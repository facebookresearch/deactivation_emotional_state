# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

##############################################
###   sample - joint_wrangling_helpers.R   ###
##############################################

library(purrr)

issue_items_primary_pre <- c(
  "BLACKWHITE_ISSUEA_W2",
  "BLACKWHITE_ISSUEB_W2",
  "BLACKWHITE_ISSUEC_W2",
  "BLACKWHITE_ISSUED_W2",
  "SEXISM1_2A_W2",
  "SEXISM1_2B_W2"
)

jwh <- list()

jwh$scale_control <- function(item, cntrl_rows, w = NULL, item_standardize_ref = NULL) {
  #' Standardizes a survey item relative to the control group, using weighted mean and standard deviation.
  #'
  #' @param item A numeric vector of survey responses.
  #' @param cntrl_rows A logical vector indicating control group rows.
  #' @param w A numeric vector of weights (default: equal weights).
  #' @param item_standardize_ref An optional reference vector for standardization (default: uses `item`).
  #'
  #' @return A numeric vector of standardized scores for the input `item`.
  #'
  if (is.null(w)) w <- rep(1, length(item))
  item <- unlist(item)
  w <- unlist(w)
  positive_weight <- cntrl_rows[w[cntrl_rows] > 0]
  # calculate weighted mean and sd for control group
  if (!is.null(item_standardize_ref)) {
    w_mean <- weighted.mean(
      item_standardize_ref[positive_weight],
      w[positive_weight],
      na.rm = TRUE
    )
    w_sd <- sqrt(Hmisc::wtd.var(
      item_standardize_ref[positive_weight],
      w[positive_weight],
      na.rm = TRUE,
      normwt = TRUE
    ))
  } else {
    w_mean <- weighted.mean(item[positive_weight], w[positive_weight], na.rm = TRUE)
    w_sd <- sqrt(Hmisc::wtd.var(
      item[positive_weight],
      w[positive_weight],
      na.rm = TRUE,
      normwt = TRUE
    ))
  }
  out <- (item - w_mean) / w_sd
  return(out)
}

jwh$create_index <- function(
  survey,
  weights_var = NULL,
  items,
  index_standardize_ref = NULL,
  items_standardize_ref = NULL
) {
  #' Combines multiple survey items into an index, standardizes each item relative to the control group,
  #' and optionally standardizes the index relative to a reference group.
  #'
  #' @param survey A data frame containing survey responses.
  #' @param weights_var The name of the weights column (default: equal weights).
  #' @param items A character vector of column names to include in the index.
  #' @param index_standardize_ref An optional reference vector for standardizing the final index.
  #' @param items_standardize_ref An optional vector of reference columns for standardizing individual items.
  #'
  #' @return A standardized index of the set of standardized components.
  #'
  if (is.null(weights_var)) {
    survey$weights_var <- 1
    weights_var <- "weights_var"
  }
  cntrl_rows <- which(survey[, "experiment_group"] == "control" & survey[, "main_sample"])

  # standardize each item component wrt to control group
  if (!is.null(items_standardize_ref)) {
    items_std <- mapply(
      function(x, z) {
        jwh$scale_control(
          survey[[x]],
          cntrl_rows,
          w = survey[[weights_var]],
          item_standardize_ref = survey[[z]]
        )
      },
      items, items_standardize_ref
    )
  } else {
    items_std <- sapply(
      items,
      function(x) jwh$scale_control(survey[[x]], cntrl_rows, w = survey[[weights_var]])
    )
  }
  index <- rowMeans(items_std, na.rm = TRUE)

  # standardize index wrt to control group
  if (!is.null(index_standardize_ref)) {
    index_std <- jwh$scale_control(
      index,
      cntrl_rows,
      w = survey[[weights_var]],
      item_standardize_ref = index_standardize_ref
    )
  } else {
    index_std <- jwh$scale_control(index, cntrl_rows, w = survey[[weights_var]])
  }
  return(index_std)
}

jwh$pro_party_scales <- function(survey, item_var, weights_var = NULL){
  #' Generates pro-Democrat and pro-Republican versions of a scale, based on weighted means.
  #'
  #' @param survey A data frame containing survey responses.
  #' @param item_var The column name (character) of the item to recode.
  #' @param weights_var The column name (character) of the weights variable. If NULL, equal weights are used.
  #'
  #' @return A data frame with two columns:
  #'   - `pro_dem`: Scale values aligned with pro-Democrat positions.
  #'   - `pro_rep`: Scale values aligned with pro-Republican positions.
  #'
  if (is.null(weights_var)) {
    survey[["temp_weight"]] <- 1
    weights_var <- "temp_weight"
  }
  item <- unlist(survey[, item_var])
  weights   <- unlist(survey[, weights_var])

  dem_idx <- survey[["partyid"]] == "D"
  rep_idx <- survey[["partyid"]] == "R"

  mean_dem <- weighted.mean(item[dem_idx], weights[dem_idx], na.rm = TRUE)
  mean_rep <- weighted.mean(item[rep_idx], weights[rep_idx], na.rm = TRUE)

  if (mean_rep > mean_dem){
    # if higher values correspond to pro-Rep
    pro_dem <- 6 - item
    pro_rep <- item
  }
  if (mean_rep <= mean_dem){
    # if higher values correspond to pro-Dem
    pro_rep <- 6 - item
    pro_dem <- item
  }
  return(data.frame(pro_dem, pro_rep))
}

jwh$create_news_variables <- function(survey) {
  #' Creates news variables from raw survey responses.
  #'
  #' @param survey A data frame containing survey responses.
  #'
  #' @return An updated `survey` with:
  #'   - Recoded news variables in a 1-4 scale.
  #'   - Aggregated news variables by category.
  #'
  recode_news_var <- function(x) {
    case_match(
      x,
      1 ~ 7, # every day
      2 ~ 4, # several times
      3 ~ 1, # once
      4 ~ 0, # never
      TRUE ~ NA_real_
    )
  }
  survey <- survey %>%
    mutate(
      tv_political_news_frequency_pre_election = recode_news_var(POLINFO_SOURCEA_W2),
      newspapers_political_news_frequency_pre_election = recode_news_var(POLINFO_SOURCEB_W2),
      news_websites_political_news_frequency_pre_election = recode_news_var(POLINFO_SOURCEC_W2),
      news_local_tv_pre = recode_news_var(POLINFO_SOURCED_W2),
      news_facebook_pre = recode_news_var(POLINFO_SOURCEE_W2),
      news_instagram_pre = recode_news_var(POLINFO_SOURCEF_W2),
      news_twitter_pre = recode_news_var(POLINFO_SOURCEG_W2),
      news_fox_news_pre = recode_news_var(POLINFO_SOURCEH_W2),
      news_msnbc_pre = recode_news_var(POLINFO_SOURCEI_W2),
      news_cnn_pre = recode_news_var(POLINFO_SOURCEJ_W2),
      news_talk_radio_pre = recode_news_var(POLINFO_SOURCEK_W2),
      news_public_radio_pre = recode_news_var(POLINFO_SOURCEL_W2),
      news_friends_family_pre = recode_news_var(POLINFO_SOURCEM_W2),
      news_youtube_pre = recode_news_var(POLINFO_SOURCEN_W2),

      tv_political_news_frequency_post_election = recode_news_var(POLINFO_SOA_W4),
      newspapers_political_news_frequency_post_election = recode_news_var(POLINFO_SOB_W4),
      news_websites_political_news_frequency_post_election = recode_news_var(POLINFO_SOC_W4),
      news_local_tv_post = recode_news_var(POLINFO_SOD_W4),
      news_facebook_post = recode_news_var(POLINFO_SOE_W4),
      news_instagram_post = recode_news_var(POLINFO_SOF_W4),
      news_twitter_post = recode_news_var(POLINFO_SOG_W4),
      news_fox_news_post = recode_news_var(POLINFO_SOH_W4),
      news_msnbc_post = recode_news_var(POLINFO_SOI_W4),
      news_cnn_post = recode_news_var(POLINFO_SOJ_W4),
      news_talk_radio_post = recode_news_var(POLINFO_SOK_W4),
      news_public_radio_post = recode_news_var(POLINFO_SOL_W4),
      news_friends_family_post = recode_news_var(POLINFO_SOM_W4),
      news_youtube_post = recode_news_var(POLINFO_SON_W4)
    )
  # Aggregate news covariates
  sum_or_na <- function(...) {
    vals <- c(...)
    if (all(is.na(vals))) {
      NA
    } else {
      sum(vals, na.rm = TRUE)
    }
  }
  survey <- survey %>%
    rowwise() %>%
    mutate(
      sum_cable_political_news_frequency_pre_election = sum_or_na(news_fox_news_pre, news_msnbc_pre, news_cnn_pre),
      news_radio_pre = sum_or_na(news_talk_radio_pre, news_public_radio_pre),
      sum_social_media_political_news_frequency_pre_election = sum_or_na(
        news_facebook_pre, news_instagram_pre,
        news_youtube_pre, news_twitter_pre
      ),
      news_youtube_twitter_pre = sum_or_na(news_youtube_pre, news_twitter_pre),
      sum_cable_political_news_frequency_post_election = sum_or_na(news_fox_news_post, news_msnbc_post, news_cnn_post),
      news_radio_post = sum_or_na(news_talk_radio_post, news_public_radio_post),
      sum_social_media_political_news_frequency_post_election = sum_or_na(
        news_facebook_post, news_instagram_post,
        news_youtube_post, news_twitter_post
      ),
      news_youtube_twitter_post = sum_or_na(news_youtube_post, news_twitter_post)
    ) %>%
    ungroup()
  return(survey)
}

jwh$create_emotion_variables <- function(
  survey,
  weights_var_w2 = "weights_w2",
  weights_var_w4 = "weights_w4",
  weights_var_w5 = "weights_w5"
) {
  #' Recodes and standardizes emotion variables in the survey data
  #' Processes raw emotion responses (e.g., EMOTA_W2) by:
  #'   - Recoding to a 0-4 scale for consistency;
  #'   - Aligning scales conceptually (e.g., reversing depression/anxiety scores);
  #'   - Computing aggregate indices for pre-treatment, post-treatment, and follow-up waves;
  #'   - Standardizing indices relative to the post-treatment wave.
  #'
  #' @param survey A data frame containing survey responses.
  #' @param weights_var_w2 Weight variable for wave 2 (default: "weights_w2").
  #' @param weights_var_w4 Weight variable for wave 4 (default: "weights_w4").
  #' @param weights_var_w5 Weight variable for wave 5 (default: "weights_w5").
  #'
  #' @return An updated `survey` with:
  #'   - Emotional state components in original scale (0-4 pt scale, suffix "_og").
  #'   - Emotional state components in standardized scale.
  #'   - Emotional state indices for baseline, endline, and post-endline waves.
  #'
  recode_emot <- function(x) case_match(x, 1 ~ 4, 2 ~ 3, 3 ~ 2, 4 ~ 1, 5 ~ 0)
  survey <- survey %>% mutate(
    # keep emotion variables in original scale (though recode to 0-4 pt scale)
    # such that 4 = all the time, 0 = never
    emot_happy_pre_og = EMOTA_W2 %>% recode_emot(),
    emot_happy_post_og = EMOTA_W4 %>% recode_emot(),
    emot_happy_w5_og = EMOTA_W5 %>% recode_emot(),
    emot_depress_pre_og = EMOTB_W2 %>% recode_emot(),
    emot_depress_post_og = EMOTB_W4 %>% recode_emot(),
    emot_depress_w5_og = EMOTB_W5 %>% recode_emot(),
    emot_anxious_pre_og = EMOTC_W2 %>% recode_emot(),
    emot_anxious_post_og = EMOTC_W4 %>% recode_emot(),
    emot_anxious_w5_og = EMOTC_W5 %>% recode_emot(),
    # recode outcomes to all be on the same scale for the computation of the index
    # i.e., reverse anxious/depress and recode to 1 = never, 5 = all the time
    emot_happy_pre = 6 - EMOTA_W2,
    emot_depress_pre = -1 * (6 - EMOTB_W2),
    emot_anxious_pre = -1 * (6 - EMOTC_W2),
    emot_happy_post = 6 - EMOTA_W4,
    emot_depress_post = -1 * (6 - EMOTB_W4),
    emot_anxious_post = -1 * (6 - EMOTC_W4),
    emot_happy_w5 = 6 - EMOTA_W5,
    emot_depress_w5 = -1 * (6 - EMOTB_W5),
    emot_anxious_w5 = -1 * (6 - EMOTC_W5),
  )
  emot_vars <- c("emot_happy", "emot_depress", "emot_anxious")
  survey$emot_pre <- jwh$create_index(survey, weights_var_w2, items = paste0(emot_vars, "_pre"))
  survey$emot_post <- jwh$create_index(survey, weights_var_w4, items = paste0(emot_vars, "_post"))
  survey$emot_w5 <- jwh$create_index(survey, weights_var_w5, items = paste0(emot_vars, "_w5"))
  # standardize emot_pre and emot_w5 relative to emot_post
  survey$emot_pre_standardized_post <- jwh$create_index(
    survey,
    weights_var_w2,
    items = paste0(emot_vars, "_pre"),
    index_standardize_ref = survey$emot_post,
    items_standardize_ref = paste0(emot_vars, "_post")
  )
  survey$emot_w5_standardized_post <- jwh$create_index(
    survey,
    weights_var_w5,
    items = paste0(emot_vars, "_w5"),
    index_standardize_ref = survey$emot_post,
    items_standardize_ref = paste0(emot_vars, "_post")
  )
  # standardize all emotion components
  survey$emot_happy_pre <- jwh$create_index(survey, weights_var_w2, items = c("emot_happy_pre"))
  survey$emot_happy_post <- jwh$create_index(survey, weights_var_w4, items = c("emot_happy_post"))
  survey$emot_happy_w5 <- jwh$create_index(survey, weights_var_w5, items = c("emot_happy_w5"))
  survey$emot_depress_pre <- jwh$create_index(survey, weights_var_w2, items = c("emot_depress_pre"))
  survey$emot_depress_post <- jwh$create_index(survey, weights_var_w4, items = c("emot_depress_post"))
  survey$emot_depress_w5 <- jwh$create_index(survey, weights_var_w5, items = c("emot_depress_w5"))
  survey$emot_anxious_pre <- jwh$create_index(survey, weights_var_w2, items = c("emot_anxious_pre"))
  survey$emot_anxious_post <- jwh$create_index(survey, weights_var_w4, items = c("emot_anxious_post"))
  survey$emot_anxious_w5 <- jwh$create_index(survey, weights_var_w5, items = c("emot_anxious_w5"))
  return(survey)
}

jwh$recode_issue_item <- function(survey, item) {
  #' Recodes one issue item into pro-Dem and pro-Rep scales
  #'
  #' @param survey A data frame containing respondents and their answers.
  #' @param item The column name (character) of the item to recode, e.g. "BLACKWHITE_ISSUEA_W2".
  #'
  #' @return An updated `survey` with two new columns:
  #'   1. `<item>_recoded_pol`, which assigns a scale matching each respondent's partyID
  #'   2. `<item>_recoded_rep`, which is the pro-Rep version of the scale
  #'
  wave <- sub(".*_W", "", item)
  weights_var <- sprintf("weights_w%s", wave)
  new_pol_var <- paste0(item, "_recoded_pol")
  new_rep_var <- paste0(item, "_recoded_rep")
  responses <- jwh$pro_party_scales(survey, item = item, weights_var = weights_var)
  # assign the correct direction for the respondent's own party
  new_values <- rep(NA, nrow(survey))
  new_values[survey[["partyid"]] == "D"] <- responses$pro_dem[survey[["partyid"]] == "D"]
  new_values[survey[["partyid"]] == "R"] <- responses$pro_rep[survey[["partyid"]] == "R"]
  survey[[new_pol_var]] <- new_values
  survey[[new_rep_var]] <- responses$pro_rep
  return(survey)
}


jw <- list()
jw$transform_survey_data <- function(
  survey,
  weights_var_w2 = "weights_w2",
  weights_var_w4 = "weights_w4",
  weights_var_w5 = "weights_w5"
) {
  #' Transforms survey data by processing raw responses.
  #' This includes:
  #'  - Processing demographic variables;
  #'  - creating emotional state variables from raw responses;
  #'  - creating news variables from raw responses;
  #'  - recoding issue items into pro-Dem and pro-Rep scales.
  #'
  #' @param survey A data frame containing survey responses.
  #' @param weights_var_w2 Weight variable for wave 2 (default: "weights_w2").
  #' @param weights_var_w4 Weight variable for wave 4 (default: "weights_w4").
  #' @param weights_var_w5 Weight variable for wave 5 (default: "weights_w5").
  #'
  #' @return An updated `survey` with the transformed variables. 
  #'
  survey <- survey %>%
    mutate(
      participant_political_ideology = case_when(
        IDEO1_W1 == 1 ~ "Very liberal",
        IDEO1_W1 == 2 ~ "Somewhat liberal",
        IDEO1_W1 == 3 ~ "Middle of the road",
        IDEO1_W1 == 4 ~ "Somewhat conservative",
        IDEO1_W1 == 5 ~ "Very conservative"
      ),
      # party ID variables
      participant_party_identification = case_when(
        PARTYID7 == 7 ~ "Strong Republican",
        PARTYID7 == 6 ~ "Republican",
        PARTYID7 == 5 ~ "Lean Republican",
        PARTYID7 == 4 ~ "Independent",
        PARTYID7 == 3 ~ "Lean Democrat",
        PARTYID7 == 2 ~ "Democrat",
        PARTYID7 == 1 ~ "Strong Democrat",
        str_detect(BLOCK_ID, "Independent") ~ "Independent",
        PARTYID7 == -1 ~ as.character(NA)
      ),
      partyid5 = case_when(
        participant_party_identification %in% c("Strong Democrat") ~ "Strong Democrat",
        participant_party_identification %in% c("Democrat", "Lean Democrat") ~ "Democrat or Lean Democrat",
        participant_party_identification %in% c("Independent") ~ "Independent",
        participant_party_identification %in% c("Republican", "Lean Republican") ~ "Republican or Lean Republican",
        participant_party_identification %in% c("Strong Republican") ~ "Strong Republican",
        TRUE ~ as.character(NA)
      ),
      partyid = case_when(
        str_detect(participant_party_identification, "Democrat")   ~ "D", 
        str_detect(participant_party_identification, "Republican") ~ "R", 
        str_detect(participant_party_identification, "Independent") ~ "I", 
        TRUE ~ as.character(NA) # all others: empty
      ),
      # political interest (pay attention to what's going on in govt and politics)
      self_reported_attention_to_politics = (-1 * POLINT_W2) + 6,
      # standard age brackets
      participant_age_6_groups = factor(
        AGE_W1,
        levels = 2:7,
        labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
      ), # taking mid point of each age bracket 
      median_age_of_bracket = recode(
        AGE_W1, `2` = 21, `3` = 29.5, `4` = 39.5, `5` = 49.5, `6` = 59.5, `7` = 75
      ),
      # participant_gender
      participant_gender = case_when(
        GENDER == 1 ~ "male",
        GENDER == 2 ~ "female",
        GENDER == 3 ~ "other"
      ),
      participant_male_or_not = ifelse(GENDER == 1, "male", "not male"),
      # race - operationalization 1: white vs. non-white
      race2 = case_when(
        RACETHNICITY == 1 ~ "white, non-hispanic",
        RACETHNICITY != 1 ~ "non-white or hispanic"
      ),
      # operationalization 2: non-Hispanic white, Hispanic or black
      participant_race_4_groups = case_when(
        RACETHNICITY == 1 ~ "white, non-hispanic",
        RACETHNICITY == 4 ~ "hispanic",
        RACETHNICITY == 2 ~ "black, non-hispanic",
        RACETHNICITY %in% c(3, 5, 6) ~ "other"
      ),
      # operationalization 3: non-Hispanic white, Hispanic, non-Hispanic black, AAPI, Other
      participant_race_5_groups = case_when(
        RACETHNICITY == 1 ~ "white, non-hispanic",
        RACETHNICITY == 4 ~ "hispanic",
        RACETHNICITY == 2 ~ "black, non-hispanic",
        RACETHNICITY == 6 ~ "aapi",
        RACETHNICITY %in% c(3, 5) ~ "other"
      ),
      # minority binary for hispanic or black, non-hispanic
      minority = case_when(
        (RACETHNICITY == 2 | RACETHNICITY == 4) ~ TRUE,
        RACETHNICITY %in% c(1, 3, 5, 6) ~ FALSE
      ),
      # Undecided voter
      undecided = case_when(
        VOTE_PREELEC_W2 == 77 ~ TRUE, 
        VOTE_PREELEC_W2 %in% c(1, 2, 3, 4, 5) ~ FALSE
      ),
      participant_2016_voter_string = factor(VOTE16_W1, levels = c(1, 2), labels = c("Voted", "Did not vote")),
      # education
      participant_has_degree = ifelse(participant_education %in% c(4, 5), TRUE, FALSE),
      # income
      participant_income = factor(
        INCOME_TERCILE, levels = 1:3, labels = c("lower", "mid", "upper")
      ),
      # multiple Accounts
      mult_fb_acct = case_when(
        is.na(FBACCT_MULTIPLE_W2) ~ as.logical(NA),
        TRUE ~ (FBACCT_MULTIPLE_W2 == 2) # "2 or more accounts"
      ),
      mult_ig_acct = case_when(
        is.na(INSTACCT_MULTIPLE_W2) ~ as.logical(NA),
        TRUE ~ (INSTACCT_MULTIPLE_W2 == 2) # "2 or more accounts"
      ),
      single_fb_acct = case_when(
        is.na(FBACCT_MULTIPLE_W2) ~ as.logical(NA), # "1 account"
        TRUE ~ (FBACCT_MULTIPLE_W2 == 1)
      ),
      single_ig_acct = case_when(
        is.na(INSTACCT_MULTIPLE_W2) ~ as.logical(NA), # "1 account"
        TRUE ~ (INSTACCT_MULTIPLE_W2 == 1)
      ), 
      # digital literacy scale
      digital_literacy_score = 5 + (-1 * rowMeans(
        select(survey, matches("DIGLITERACY_TERM[A-G]_W2")), na.rm = TRUE)
      ),
      political_knowledge_score_pre_election = ifelse(
        is.na(ENDDT_W2),
        NA,
        ifelse(
          KNOWLEDGE_PRE_W2 != 1,
          0,
          (coalesce(KNOW_HOUSE_W2, 0) == 1) + (coalesce(KNOW_SENATE_W2, 0) == 2)
        )
      ),
      # adding pre-treatment intention, although not exact same question
      turnout_likelihood_pre_election = 5 - VOTE_LIKELY_W2,
      turnout_likelihood_post_election = case_when(
        TURNOUT_W4 == 4 ~ 1, # "I am sure I voted"
        TURNOUT_W4 %in% c(1, 2, 3) ~ 0 # all others
      ),
      # self-reported political participation
      answered_pre =
        POLPART_1_W2 |
        POLPART_2_W2 |
        POLPART_3_W2 |
        POLPART_4_W2 |
        POLPART_5_W2 |
        POLPART_6_W2 |
        POLPART_7_W2,
      answered_post =
        POLPART_1_W4 |
        POLPART_2_W4 |
        POLPART_3_W4 |
        POLPART_4_W4 |
        POLPART_5_W4 |
        POLPART_6_W4 |
        POLPART_7_W4,
      polpart_protest_pre     = ifelse(answered_pre,  POLPART_1_W2, NA),
      polpart_contribute_pre  = ifelse(answered_pre,  POLPART_2_W2, NA),
      polpart_petition_pre    = ifelse(answered_pre,  POLPART_3_W2, NA),
      polpart_convinced_pre   = ifelse(answered_pre,  POLPART_4_W2, NA),
      polpart_online_pre      = ifelse(answered_pre,  POLPART_5_W2, NA),
      polpart_talked_pre      = ifelse(answered_pre,  POLPART_6_W2, NA),
      polpart_count_pre       =
        polpart_protest_pre + polpart_contribute_pre + polpart_petition_pre +
        polpart_convinced_pre + polpart_online_pre + polpart_talked_pre,
      # voted for trump
      votechoice_pre_election = case_when(
        VOTE_LIKELY_W2 %in% c(3,4) ~ 0, # unlikely to vote
        VOTE_PREELEC_W2 == 2 ~ 1, # Trump
        VOTE_PREELEC_W2 == 1 ~ -1, # Biden
        is.na(ENDDT_W2) ~ as.numeric(NA),
        TRUE ~ 0
      ),
    )
  # create index for political participation
  survey$weighted_index_political_participation_pre_election <- jwh$create_index(survey, weights_var_w2, items = c("polpart_count_pre"))
  # add news usage variables
  survey <- jwh$create_news_variables(survey)
  # add issue item variables
  survey <- purrr::reduce(
    issue_items_primary_pre,
    ~ jwh$recode_issue_item(.x, item = .y),
    .init = survey
  )
  survey$weighted_index_issue_polarization_pre_election  <- jwh$create_index(
    survey,
    weights_var_w2,
    items = paste0(issue_items_primary_pre, "_recoded_rep")
  )
  # create emotional state variables
  survey <- jwh$create_emotion_variables(survey, weights_var_w2, weights_var_w4, weights_var_w5)
  # recode empty strings to NA
  survey <- survey %>% mutate(partyid = na_if(partyid, ""))
  return(survey)
}


jw$run_factor_bin_routine <- function(sample, lasso_covars, model_covars, response_date_covars) {
  #' Factorizes lasso, model, and response_date covariates in the sample data
  #'
  #' @param survey A data frame containing survey responses.
  #' @param lasso_covars Vector of covariates for lasso regression.
  #' @param model_covars Vector of model covariates for the block ID for randomization.
  #' @param response_date_covars Vector of covariates for date of response.
  #'
  #' @return A list of updated objects:
  #'   1. `sample`, which is the updated sample data frame with factorized covariates
  #'   2. `lasso_covars`, which is the updated (factorized) lasso covariates
  #'   3. `model_covars`, which is the updated (factorized) model covariates
  #'   4. `response_date_covars`, which is the updated (factorized) response date covariates
  #'
  # creating binary vars for factor vars
  factor_bins  <- alethea::make_factor_bins(d = sample, covars = lasso_covars)
  sample       <- factor_bins$d
  lasso_covars <- factor_bins$updated_covars

  factor_bins  <- alethea::make_factor_bins(d = sample, covars = model_covars)
  sample       <- factor_bins$d
  model_covars <- factor_bins$updated_covars

  factor_bins  <- alethea::make_factor_bins(d = sample, covars = response_date_covars)
  sample       <- factor_bins$d
  response_date_covars <- factor_bins$updated_covars

  return(
    list(
      "sample" = sample,
      "lasso_covars" = lasso_covars,
      "model_covars" = model_covars,
      "response_date_covars" = response_date_covars
    )
  )
}
