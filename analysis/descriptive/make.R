# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

################################
###   descriptive - make.R   ###
################################

## Empty input/output directory
unlink("analysis/descriptive/input", recursive = TRUE)
dir.create("analysis/descriptive/input")
unlink("analysis/descriptive/output", recursive = TRUE)
dir.create("analysis/descriptive/output")
dir.create("analysis/descriptive/output/tables")
dir.create("analysis/descriptive/output/pngs")
dir.create("analysis/descriptive/output/pdfs")
dir.create("analysis/descriptive/output/scalars")

file.copy("data/platform/output/fb_ig_pop_stats.rds", "analysis/descriptive/input/fb_ig_pop_stats.rds")

file.copy("data/survey/output/attriters.rds", "analysis/descriptive/input/attriters.rds")
file.copy("data/survey/output/survey.rds", "analysis/descriptive/input/survey.rds")

file.copy("data/sample/output/uncut_sample_fb.rds",   "analysis/descriptive/input/uncut_sample_fb.rds")
file.copy("data/sample/output/uncut_sample_ig.rds",   "analysis/descriptive/input/uncut_sample_ig.rds")

file.copy("data/sample/output/main_sample_fb.rds",    "analysis/descriptive/input/main_sample_fb.rds")
file.copy("data/sample/output/main_sample_ig.rds",    "analysis/descriptive/input/main_sample_ig.rds")

file.copy("data/sample/output/main_sample_ds_fb.rds", "analysis/descriptive/input/main_sample_ds_fb.rds")
file.copy("data/sample/output/main_sample_ds_ig.rds", "analysis/descriptive/input/main_sample_ds_ig.rds")

file.copy("data/sample/output/weighted_data_ids.rds", "analysis/descriptive/input/weighted_data_ids.rds")

file.copy("data/external/output/passive_tracking_users.rds", "analysis/descriptive/input/passive_tracking_users.rds")

## Execute scripts to create outputs
source("analysis/descriptive/code/prepare_attrition_outputs.R")
source("analysis/descriptive/code/responses_by_date.R")
source("analysis/descriptive/code/treatment_compliance.R")
source("analysis/descriptive/code/external_validity.R")
source("analysis/descriptive/code/balance.R")
source("analysis/descriptive/code/outcome_sum_stats.R")
source("analysis/descriptive/code/index_reliability_validity.R")

message("
  ####################################
  # DESCRIPTIVE SUBMODULE COMPLETED  #
  ####################################
")
