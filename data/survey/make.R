# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

###########################
###   survey - make.R   ###
###########################

## Empty output directory
unlink("data/survey/output", recursive = TRUE)
dir.create("data/survey/output")

## Execute scripts to create outputs
source("data/survey/code/unpack_survey_data.R")

message("
  ################################
  #  SURVEY SUBMODULE COMPLETED  #
  ################################
")
