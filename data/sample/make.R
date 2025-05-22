# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

###########################
###   sample - make.R   ###
###########################

## Empty input/output directory
unlink("data/sample/input", recursive = TRUE)
dir.create("data/sample/input")
unlink("data/sample/output", recursive = TRUE)
dir.create("data/sample/output")

## Copy output from recruitment_platform_string and survey to input directory
file.copy("data/platform/output/plat_users_ds_fb.rds", "data/sample/input/plat_users_ds_fb.rds")
file.copy("data/platform/output/plat_users_fb.rds", "data/sample/input/plat_users_fb.rds")
file.copy("data/platform/output/plat_users_ds_ig.rds", "data/sample/input/plat_users_ds_ig.rds")
file.copy("data/platform/output/plat_users_ig.rds", "data/sample/input/plat_users_ig.rds")
file.copy("data/survey/output/survey.rds", "data/sample/input/survey.rds")
file.copy("data/survey/output/survey_weights.rds", "data/sample/input/survey_weights.rds")

## Execute scripts to create outputs
source("data/sample/code/assemble_sample_data.R")

message("
  ################################
  #  SAMPLE SUBMODULE COMPLETED  #
  ################################
")
