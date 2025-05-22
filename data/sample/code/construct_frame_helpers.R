# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

##############################################
###   sample - construct_frame_helpers.R   ###
##############################################

cf <- list(
  construct_weighted_data_ids_frame = function(survey, plat_fb, plat_ig, weights) {
    # get unique ids for recruitment_platform_string data, survey responses
    survey_ids <- unique(survey$participant_user_id)
    survey_fb_ids <- unique(survey$participant_user_id[survey$recruitment_platform_string == "FB"])
    survey_ig_ids <- unique(survey$participant_user_id[survey$recruitment_platform_string == "IG"])
    plat_ids   <- unique(c(plat_fb$participant_user_id, plat_ig$participant_user_id))

    ids <- unique(c(survey_ids, plat_ids))
    fb_ids <- unique(c(plat_fb$participant_user_id, survey_fb_ids))
    ig_ids <- unique(c(plat_ig$participant_user_id, survey_ig_ids))

    # survey completion status
    completed_w2 <- survey$participant_user_id[!is.na(survey$ENDDT_W2)]
    completed_w4 <- survey$participant_user_id[!is.na(survey$ENDDT_W4)]
    completed_w5 <- survey$participant_user_id[!is.na(survey$ENDDT_W5)]

    # build frame with ids
    ids_frame <- data.frame(
      participant_user_id = ids,
      has_survey_data = ids %in% survey_ids,
      has_platform_data = ids %in% plat_ids,
      completed_w2 = ids %in% completed_w2,
      completed_w4 = ids %in% completed_w4,
      completed_w5 = ids %in% completed_w5,
      in_fb_data = ids %in% fb_ids,
      in_ig_data = ids %in% ig_ids
    )

    # join to survey weights and create uncut_sample and main_sample binaries
    ids_frame <- ids_frame %>%
      left_join(weights, by = c("participant_user_id")) %>%
      mutate(
          above_15min_usage = weights_w2 > 0,
          uncut_sample = has_platform_data & has_survey_data,
          main_sample  = has_platform_data & has_survey_data & above_15min_usage,
          recruitment_platform_string = case_when(
            in_fb_data ~ "FB",
            in_ig_data ~ "IG"
          )
      )
    return(ids_frame)
  },
  construct_aux_frame = function(
    filename
  ) {
    aux_frame <- alethea::read_file(
      filename = filename,
      has_header = TRUE
    )
    # rename P_PSID with participant_user_id for ease of joining
    if ("P_PSID" %in% colnames(aux_frame)) {
      aux_frame <- aux_frame %>% rename(participant_user_id = P_PSID)
    }
    return(aux_frame)
  }
)
