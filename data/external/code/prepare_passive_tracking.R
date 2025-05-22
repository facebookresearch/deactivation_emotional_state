# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

#################################################
###   external - prepare_passive_tracking.R   ###
#################################################

devtools::load_all("~/alethea")

main <- function() {
  passive_tracking_users <- alethea::read_file(
    filename = "/data/us2020/emotional_state_replication/data/agg_passive_app_data_by_app_daily_deactivation_replication.csv",
    has_header = TRUE
  )
  saveRDS(passive_tracking_users, "data/external/output/passive_tracking_users.rds")
}


## Execute
main()
