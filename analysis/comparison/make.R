# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

###############################
###   comparison - make.R   ###
###############################

## Empty input/output directory
unlink("analysis/comparison/input", recursive = TRUE)
dir.create("analysis/comparison/input")
unlink("analysis/comparison/output", recursive = TRUE)

dir.create("analysis/comparison/input/NSDUH")
dir.create("analysis/comparison/input/ESS")
dir.create("analysis/comparison/output")
dir.create("analysis/comparison/output/tables")
dir.create("analysis/comparison/output/pngs")
dir.create("analysis/comparison/output/pdfs")
dir.create("analysis/comparison/output/scalars")

file.copy("data/sample/output/main_sample_fb.rds", "analysis/comparison/input/main_sample_fb.rds")
file.copy("data/sample/output/main_sample_ig.rds", "analysis/comparison/input/main_sample_ig.rds")

file.copy("data/sample/output/main_sample_fb.rds", "analysis/comparison/input/main_sample_fb.rds")
file.copy("data/sample/output/main_sample_ig.rds", "analysis/comparison/input/main_sample_ig.rds")

file.copy("data/sample/output/model_covars_fb.rds", "analysis/comparison/input/model_covars_fb.rds")
file.copy("data/sample/output/model_covars_ig.rds", "analysis/comparison/input/model_covars_ig.rds")

file.copy("data/sample/output/lasso_covars_fb.rds", "analysis/comparison/input/lasso_covars_fb.rds")
file.copy("data/sample/output/lasso_covars_ig.rds", "analysis/comparison/input/lasso_covars_ig.rds")

## NSDUH and ESS data
file.copy("data/external/output/pooled_NSDUH.rds", "analysis/comparison/input/pooled_NSDUH.rds")
file.copy("data/external/output/harmonized_NSDUH_2020.rds", "analysis/comparison/input/harmonized_NSDUH_2020.rds")
file.copy("data/external/output/harmonized_NSDUH_main_sample.rds", "analysis/comparison/input/harmonized_NSDUH_main_sample.rds")
file.copy("data/external/output/ess6_data.rds", "analysis/comparison/input/ess6_data.rds")

## Copy output from primary to input directory
file.copy("analysis/primary/output/selected_covars_fb.rds", "analysis/comparison/input/selected_covars_fb.rds")
file.copy("analysis/primary/output/selected_covars_ig.rds", "analysis/comparison/input/selected_covars_ig.rds")
file.copy("analysis/primary/output/res_fb.rds", "analysis/comparison/input/emotion_ATEs_fb.rds")
file.copy("analysis/primary/output/res_ig.rds", "analysis/comparison/input/emotion_ATEs_ig.rds")

## Copy output from heterogenous to input directory
file.copy("analysis/heterogeneous/output/secondary_HTEs_ig.rds", "analysis/comparison/input/secondary_HTEs_ig.rds")

## Execute scripts to create outputs
source("analysis/comparison/code/prepare_comparison_outputs.R")
source("analysis/comparison/code/compute_non_exper_effects.R")
source("analysis/comparison/code/prepare_demographic_outputs.R")
source("analysis/comparison/code/prepare_ESS_6_outputs.R")
source("analysis/comparison/code/comparison_K6_NSDUH.R")

message("
  #####################################
  ##  COMPARISON SUBMODULE COMPLETED  ##
  #####################################
")
