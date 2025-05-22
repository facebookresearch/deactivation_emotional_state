# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

#################################
###   substitution - make.R   ###
#################################

## Empty input/output directory
unlink("analysis/substitution/input", recursive = TRUE)
dir.create("analysis/substitution/input")
unlink("analysis/substitution/output", recursive = TRUE)
dir.create("analysis/substitution/output")
dir.create("analysis/substitution/output/tables")
dir.create("analysis/substitution/output/pngs")
dir.create("analysis/substitution/output/pdfs")
dir.create("analysis/substitution/output/scalars")

file.copy("data/sample/output/main_sample_fb.rds", "analysis/substitution/input/main_sample_fb.rds")
file.copy("data/sample/output/main_sample_ig.rds", "analysis/substitution/input/main_sample_ig.rds")

file.copy("data/sample/output/lasso_covars_fb.rds", "analysis/substitution/input/lasso_covars_fb.rds")
file.copy("data/sample/output/lasso_covars_ig.rds", "analysis/substitution/input/lasso_covars_ig.rds")

file.copy("data/sample/output/model_covars_fb.rds", "analysis/substitution/input/model_covars_fb.rds")
file.copy("data/sample/output/model_covars_ig.rds", "analysis/substitution/input/model_covars_ig.rds")

file.copy("data/external/output/passive_tracking_users.rds", "analysis/substitution/input/passive_tracking_users.rds")

## Execute scripts to create outputs
source("analysis/substitution/code/compute_app_substitution_effects.R")
source("analysis/substitution/code/prepare_substitution_outputs.R")

message("
  #####################################
  # SUBSTITUTION SUBMODULE COMPLETED  #
  #####################################
")
