# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

############################
###   primary - make.R   ###
############################

## Empty input/output directory
unlink("analysis/primary/input", recursive = TRUE)
dir.create("analysis/primary/input")
unlink("analysis/primary/output", recursive = TRUE)
dir.create("analysis/primary/output")
dir.create("analysis/primary/output/tables")
dir.create("analysis/primary/output/pngs")
dir.create("analysis/primary/output/pdfs")
dir.create("analysis/primary/output/scalars")

file.copy("data/survey/output/attriters.rds", "analysis/primary/input/attriters.rds")

file.copy("data/sample/output/main_sample_fb.rds", "analysis/primary/input/main_sample_fb.rds")
file.copy("data/sample/output/main_sample_ig.rds", "analysis/primary/input/main_sample_ig.rds")

file.copy("data/sample/output/uncut_sample_fb.rds", "analysis/primary/input/uncut_sample_fb.rds")
file.copy("data/sample/output/uncut_sample_ig.rds", "analysis/primary/input/uncut_sample_ig.rds")

file.copy("data/sample/output/lasso_covars_fb.rds", "analysis/primary/input/lasso_covars_fb.rds")
file.copy("data/sample/output/lasso_covars_ig.rds", "analysis/primary/input/lasso_covars_ig.rds")

file.copy("data/sample/output/model_covars_fb.rds", "analysis/primary/input/model_covars_fb.rds")
file.copy("data/sample/output/model_covars_ig.rds", "analysis/primary/input/model_covars_ig.rds")

file.copy("data/sample/output/weighted_data_ids.rds", "analysis/primary/input/weighted_data_ids.rds")

## Execute scripts to create outputs
source("analysis/primary/code/compute_primary_effects.R")
source("analysis/primary/code/compute_lee_bounds.R")
source("analysis/primary/code/prepare_primary_outputs.R")

message("
  #################################
  #  PRIMARY SUBMODULE COMPLETED  #
  #################################
")
