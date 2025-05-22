# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

devtools::load_all('~/alethea')

# Read input table:
input_data_fb_users_str_0808_replication_combined <- read_file(
    filename='/data/us2020/emotional_state_replication/data/dim_us2020_de_fb_users_20220808_combined_replication_emotional_state.csv',
    has_header=TRUE
)

# Divide into expected input tables:
input_data_dim_us2020_de_fb_users_20220808_replication = input_data_fb_users_str_0808_replication_combined %>%
    filter(row_in_user_table) %>%
    select(
        participant_user_id,
        instagram_account_is_connected,
        experiment_group,
        follower_count,
        facebook_friend_count,
        participant_number_of_active_days_aug25_sept24,
        participant_number_of_active_days_aug27_sept24,
        civic_page_like_count,
        daily_time_spent_on_facebook_relative_to_baseline,
        daily_time_spent_on_instagram_relative_to_baseline,
        view_count_posts_per_day
    )

input_data_baseline_time_spent_fb_replication = input_data_fb_users_str_0808_replication_combined %>%
    filter(row_in_time_spent_baseline_table) %>%
    select(
        participant_user_id,
        participant_time_spent_relative_to_baseline_fb
    )

# Read input table:
input_data_ig_users_str_0808_replication_combined <- read_file(
    filename="/data/us2020/emotional_state_replication/data/dim_us2020_de_ig_users_20220808_combined_replication_emotional_state.csv",
    has_header=TRUE
)

# Divide into expected input tables:
input_data_dim_us2020_de_ig_users_20220808_replication = input_data_ig_users_str_0808_replication_combined %>%
    filter(row_in_user_table) %>%
    select(
        participant_user_id,
        facebook_account_is_connected,
        experiment_group,
        follower_count,
        follow_count,
        participant_number_of_active_days_aug25_sept24,
        participant_number_of_active_days_aug27_sept24,
        view_count_content_per_day,
        daily_time_spent_on_instagram_relative_to_baseline,
        daily_time_spent_on_facebook_relative_to_baseline
    )

input_data_baseline_time_spent_ig_replication = input_data_ig_users_str_0808_replication_combined %>%
    filter(row_in_time_spent_baseline_table) %>%
    select(
        participant_user_id,
        participant_time_spent_relative_to_baseline_ig
    )
