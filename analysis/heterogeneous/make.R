# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

##################################
###   heterogeneous - make.R   ###
##################################

## Empty input/output directory
unlink("analysis/heterogeneous/input", recursive = TRUE)
dir.create("analysis/heterogeneous/input")
unlink("analysis/heterogeneous/output", recursive = TRUE)
dir.create("analysis/heterogeneous/output")
dir.create("analysis/heterogeneous/output/tables")
dir.create("analysis/heterogeneous/output/pngs")
dir.create("analysis/heterogeneous/output/pdfs")
dir.create("analysis/heterogeneous/output/scalars")

file.copy("data/sample/output/main_sample_fb.rds", "analysis/heterogeneous/input/main_sample_fb.rds")
file.copy("data/sample/output/main_sample_ig.rds", "analysis/heterogeneous/input/main_sample_ig.rds")

file.copy("data/sample/output/model_covars_fb.rds", "analysis/heterogeneous/input/model_covars_fb.rds")
file.copy("data/sample/output/model_covars_ig.rds", "analysis/heterogeneous/input/model_covars_ig.rds")

## Copy output from primary to input directory 
file.copy("analysis/primary/output/selected_covars_fb.rds", "analysis/heterogeneous/input/selected_covars_fb.rds")
file.copy("analysis/primary/output/selected_covars_ig.rds", "analysis/heterogeneous/input/selected_covars_ig.rds")

## Execute scripts to create outputs
source("analysis/heterogeneous/code/compute_het_effects.R")
source("analysis/heterogeneous/code/prepare_het_outputs.R")

message("
  #####################################
  # HETEROGENEOUS SUBMODULE COMPLETED #
  #####################################
")
