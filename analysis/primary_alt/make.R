# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

################################
###   primary_alt - make.R   ###
################################

## Empty input/output directory
unlink("analysis/primary_alt/input", recursive = TRUE)
dir.create("analysis/primary_alt/input")
unlink("analysis/primary_alt/output", recursive = TRUE)
dir.create("analysis/primary_alt/output")
dir.create("analysis/primary_alt/output/tables")
dir.create("analysis/primary_alt/output/pngs")
dir.create("analysis/primary_alt/output/pdfs")

file.copy("data/sample/output/main_sample_fb.rds", "analysis/primary_alt/input/main_sample_fb.rds")
file.copy("data/sample/output/main_sample_ig.rds", "analysis/primary_alt/input/main_sample_ig.rds")

file.copy("data/sample/output/lasso_covars_fb.rds", "analysis/primary_alt/input/lasso_covars_fb.rds")
file.copy("data/sample/output/lasso_covars_ig.rds", "analysis/primary_alt/input/lasso_covars_ig.rds")

file.copy("data/sample/output/model_covars_fb.rds", "analysis/primary_alt/input/model_covars_fb.rds")
file.copy("data/sample/output/model_covars_ig.rds", "analysis/primary_alt/input/model_covars_ig.rds")

file.copy("data/sample/output/response_date_covars_fb.rds", "analysis/primary_alt/input/response_date_covars_fb.rds")
file.copy("data/sample/output/response_date_covars_ig.rds", "analysis/primary_alt/input/response_date_covars_ig.rds")

file.copy("analysis/primary/output/selected_covars_fb.rds", "analysis/primary_alt/input/selected_covars_fb.rds")
file.copy("analysis/primary/output/selected_covars_ig.rds", "analysis/primary_alt/input/selected_covars_ig.rds")

## Execute scripts to create outputs
source("analysis/primary_alt/code/compute_primary_effects_alt.R")
source("analysis/primary_alt/code/prepare_primary_outputs_alt.R")

message("
  #####################################
  #  PRIMARY ALT SUBMODULE COMPLETED  #
  #####################################
")
