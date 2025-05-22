# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

########################################
###   lib - outcome_dictionaries.R   ###
########################################

# dictionary helper functions
process_dict_to_list <- function(dict) {
  #' Prorocesses a list mapping variables to labels into a list of: 
  #'   - `all`: a list of lists with the variable and label;
  #'   - `vars`: a vector of variables;
  #'   - `labels`: a vector of labels.
  #'
  #' @param dict A list mapping variables to labels.
  #'
  #' @return A list with three elements: `all`, `vars`, and `labels`.
  #'
  vars <- names(dict)
  labels <- unlist(dict, use.names = FALSE)
  res <- mapply(list, vars, labels, SIMPLIFY = FALSE)
  return(list(
    all = res, 
    vars = vars, 
    labels = labels
  ))
}

# emotion outcomes dictionary
make_emotion_outcomes_label_dict <- function(outcomes = "all") {
  if (outcomes == "index") {
    emotion_outcomes_label_dict <- list(
      "emot_post" = "Emotional state index"
    )
  } else if (outcomes == "components") {
    emotion_outcomes_label_dict <- list(
      "emot_happy_post" = "Happy",
      "emot_depress_post" = "Depressed x (-1)",
      "emot_anxious_post" = "Anxious x (-1)"
    ) 
  } else {
    emotion_outcomes_label_dict <- list(
      "emot_post" = "Emotional state index",
      "emot_happy_post" = "Happy",
      "emot_depress_post" = "Depressed x (-1)",
      "emot_anxious_post" = "Anxious x (-1)"
    )
  }
  return(emotion_outcomes_label_dict)
}
emotion_outcomes_order <- unlist(make_emotion_outcomes_label_dict(), use.names = FALSE)

make_emotion_outcomes_baseline_dict <- function(outcomes = "all") {
  if (outcomes == "index") {
    emotion_outcomes_baseline_dict <- list(
      "emot_post" = "emot_pre"
    )
  } else if (outcomes == "components") {
    emotion_outcomes_baseline_dict <- list(
      "emot_happy_post" = "emot_happy_pre",
      "emot_depress_post" = "emot_depress_pre",
      "emot_anxious_post" ="emot_anxious_pre"
    ) 
  } else {
    emotion_outcomes_baseline_dict <- list(
      "emot_post" = "emot_pre",
      "emot_happy_post" = "emot_happy_pre",
      "emot_depress_post" = "emot_depress_pre",
      "emot_anxious_post" = "emot_anxious_pre"
    )
  }
  return(emotion_outcomes_baseline_dict)
}

make_emotion_outcomes_og_units_label_dict <- function() {
  emotion_outcomes_label_dict <- list(
      "emot_happy_post_og" = "Happy",
      "emot_depress_post_og" = "Depressed",
      "emot_anxious_post_og" = "Anxious"
  )
}

make_emotion_outcomes_og_units_baseline_dict <- function() {
  emotion_outcomes_label_dict <- list(
      "emot_happy_post_og" = "emot_happy_pre_og",
      "emot_depress_post_og" = "emot_depress_pre_og",
      "emot_anxious_post_og" = "emot_anxious_pre_og"
  )
}


## SUBSTITUTION OUTCOMES
make_substitution_custom_apps_dict <- function() { 
  dict <- list(
    "time_spent_facebook" = "Facebook",
    "time_spent_instagram" = "Instagram",
    "time_spent_snapchat" = "Snapchat",
    "time_spent_twitter" = "Twitter",
    "time_spent_tiktok" = "TikTok",
    "time_spent_youtube" = "YouTube",
    "time_spent_facebookmessenger" = "Facebook Messenger",
    "time_spent_browsers" = "Browsers",
    "time_spent_othersocialmedia" = "Other social media",
    "time_spent_othermedia_video" = "Other media & video",
    "time_spent_othermessenger_mail" = "Other messenger & mail",
    "time_spent_music_audio" = "Music & audio",
    "time_spent_other" = "Other"
  )
  return(dict)
}

make_substitution_custom_apps_social_media_dict <- function() {
  dict <- list(
    "time_spent_facebook" = "Facebook",
    "time_spent_instagram" = "Instagram",
    "time_spent_snapchat" = "Snapchat",
    "time_spent_twitter" = "Twitter",
    "time_spent_tiktok" = "TikTok",
    "time_spent_youtube" = "YouTube",
    "time_spent_facebookmessenger" = "Facebook Messenger",
    "time_spent_othersocialmedia" = "Other social media"
  )
  return(dict)
}

make_substitution_outcomes_baseline_dict <- function(){ 
  min.fb <- list(
    "time_spent_ig" = "",
    "time_spent_social_media_no_fb" = "",
    "time_spent_news" = ""
  )
  min.ig <- list(
    "time_spent_fb" = "",
    "time_spent_social_media_no_ig" = "",
    "time_spent_news" = ""
  )
  visits.fb <- list(
    "web_visits_ig" = "",
    "web_visits_social_media_no_fb" = "",
    "web_visits_news" = ""
  )
  visits.ig <- list(
    "web_visits_fb" = "",
    "web_visits_social_media_no_ig" = "",
    "web_visits_news" = ""
  )
  # note: times is same for fb and ig
  times <- list(
    "tv_political_news_frequency_post_election" = "tv_political_news_frequency_pre_election", 
    "news_local_tv_post" = "news_local_tv_pre", 
    "sum_cable_political_news_frequency_post_election" = "sum_cable_political_news_frequency_pre_election", 
    "newspapers_political_news_frequency_post_election" = "newspapers_political_news_frequency_pre_election",
    "news_radio_post" = "news_radio_pre",
    "news_friends_family_post" = "news_friends_family_pre",
    "news_websites_political_news_frequency_post_election" = "news_websites_political_news_frequency_pre_election", 
    "news_youtube_twitter_post" = "news_youtube_twitter_pre"
  )
  return(list(
    fb = list(min = min.fb, visits = visits.fb, times = times), 
    ig = list(min = min.ig, visits = visits.ig, times = times)
  ))
}

## VOTE OUTCOMES  
make_vote_outcomes_baseline_dict <- function() {
  dict <- list(
    "trumpvote_post" = "trumpvote_pre",
    "bidenvote_post" = "bidenvote_pre",
    "neithervote_post" = "neithervote_pre",
    "trumpshare_post" = "trumpshare_pre"
  )
  return(dict)
}

## MODERATORS
make_primary_moderators <- function() {
  group_labels_fb <- list(
    gender_age_above_med = "Gender x age",
    baseline_use_above_med = "Baseline use",
    emot_pre_above_med = "Baseline emotional state",
    polpart_pre_above_med = "Baseline political participation",
    civic_page_fan_count_above_med = "Civic pages followed"
  )
  group_labels_ig <- list(
    gender_age_above_med = "Gender x age",
    baseline_use_above_med = "Baseline use",
    emot_pre_above_med = "Baseline emotional state",
    polpart_pre_above_med = "Baseline political participation"
  )
  subgroup_labels_fb <- list(
    gender_age_above_med = list(
      "female_FALSE" = "Female - Aged 18-34",
      "female_TRUE" = "Female - Aged 35+",
      "male_FALSE" = "Male - Aged 18-34",
      "male_TRUE" = "Male - Aged 35+"
    ), 
    baseline_use_above_med = list(
      "TRUE" = "Above median",
      "FALSE" = "Below median"
    ),
    emot_pre_above_med = list(
      "TRUE" = "Above median",
      "FALSE" = "Below median"
    ),
    polpart_pre_above_med = list(
      "TRUE" = "Above median",
      "FALSE" = "Below median"
    ),
    civic_page_fan_count_above_med = list(
      "TRUE" = "Above median",
      "FALSE" = "Below median"
    )
  )
  subgroup_labels_ig <- list(
    gender_age_above_med = list(
      "female_FALSE" = "Female - Aged 18-24", 
      "female_TRUE" = "Female - Aged 25+", 
      "male_FALSE" = "Male - Aged 18-24", 
      "male_TRUE" = "Male - Aged 25+"
    ), 
    baseline_use_above_med = list(
      "TRUE" = "Above median", 
      "FALSE" = "Below median"
    ),
    emot_pre_above_med = list(
      "TRUE" = "Above median", 
      "FALSE" = "Below median"
    ),
    polpart_pre_above_med = list(
      "TRUE" = "Above median",
      "FALSE" = "Below median"
    )
  )
  return(list(group_fb = group_labels_fb,
              group_ig = group_labels_ig,
              subgroup_fb = subgroup_labels_fb, 
              subgroup_ig = subgroup_labels_ig))
}

make_secondary_moderators <- function(plat = "FB") {
  group_labels <- list(
    partyid = "Party ID",
    partyid5 = "Party ID",
    participant_male_or_not = "Gender",
    age_above_med = "Age",
    undecided = "Undecided voters", 
    participant_has_degree = "Education",
    URBAN_RURAL = "Urban status", 
    BATTLEGROUND_BLOCK = "Battleground state",
    minority = "Black or hispanic"
  )
  subgroup_labels <- list(
    partyid = list(
        "I" = "Independents",
        "R" = "All Republicans",
        "D" = "All Democrats"
    ),
    partyid5 = list(
      "Strong Republican" = "Strong Republicans",
      "All Republican" = "All Republicans",
      "Independent" = "Independents",
      "All Democrat" = "All Democrats",
      "Strong Democrat" = "Strong Democrats"
    ), 
    participant_male_or_not = list(
      "male" = "Male",
      "not male" = "Female"
    ),
    age_above_med = list(
      "FALSE" = if (plat == "FB") "Aged 18-34" else "Aged 18-24",
      "TRUE" = if (plat == "FB") "Aged 35+" else "Aged 25+"
    ),
    undecided = list(
      "TRUE" = "Yes", 
      "FALSE" = "No"
    ),
    participant_has_degree = list(
      "TRUE" = "College", 
      "FALSE" = "No college"
    ), 
    URBAN_RURAL = list(
      "1" = "Urban", 
      "2" = "Suburban", 
      "3" = "Rural"
    ),
    BATTLEGROUND_BLOCK = list(
      "yes" = "Yes", 
      "no" = "No"
    ),
    minority = list(
      "TRUE" = "Yes", 
      "FALSE" = "No"
    )
  )
  return(list(group = group_labels, subgroup = subgroup_labels))
}

make_alt_weights_labels_dict <- function(plat = "FB") {
  dict <- list(
    weight_var_win95 = "Winsorize at 95th",
    weight_var_win90 = "Winsorize at 90th",
    weight_alt_not_educ = "Not Educ",
    weight_alt_not_party = "Not Party",
    weight_alt_not_race = "Not Race",
    weight_alt_not_l30 = "Not L30"
  )
  if (plat == "FB") {
    dict[["weight_alt_not_ideo"]] <- "Not Ideo (FB) / Not Swing (IG)"
    dict[["weight_alt_not_time"]] <- "Not Time"
    dict[["weight_alt_not_civic"]] <- "Not Civic (FB) / Not No. Following (IG)"
  } else if (plat == "IG") {
    dict[["weight_alt_not_swing"]] <- "Not Ideo (FB) / Not Swing (IG)"
    dict[["weight_alt_not_time"]] <- "Not Time"
    dict[["weight_alt_not_follow"]] <- "Not Civic (FB) / Not No. Following (IG)"
  }

  return(dict)
}
