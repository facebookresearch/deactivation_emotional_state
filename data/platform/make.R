# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

#############################
###   recruitment_platform_string - make.R   ###
#############################

## Empty output directory
unlink("data/platform/output", recursive = TRUE)
dir.create("data/platform/output")

## Execute scripts to create outputs
source("data/platform/code/unpack_platform_data.R")

message("
   ################################
   # PLATFORM SUBMODULE COMPLETED #
   ################################
")
