# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

#############################
###   external - make.R   ###
#############################


## Empty input/output directory
unlink("data/external/input", recursive = TRUE)
dir.create("data/external/input")
unlink("data/external/output", recursive = TRUE)

dir.create("data/external/input/NSDUH")
dir.create("data/external/input/ESS")
dir.create("data/external/output")

file.copy("data/sample/output/main_sample_fb.rds", "data/external/input/main_sample_fb.rds")
file.copy("data/sample/output/main_sample_ig.rds", "data/external/input/main_sample_ig.rds")

## Copy NSDUH data
file.copy("data/raw/NSDUH/NSDUH_2008.dta", "data/external/input/NSDUH/NSDUH_2008.dta")
file.copy("data/raw/NSDUH/NSDUH_2009.dta", "data/external/input/NSDUH/NSDUH_2009.dta")
file.copy("data/raw/NSDUH/NSDUH_2020.dta", "data/external/input/NSDUH/NSDUH_2020.dta")
file.copy("data/raw/NSDUH/NSDUH_2021.dta", "data/external/input/NSDUH/NSDUH_2021.dta")
file.copy("data/raw/NSDUH/NSDUH_2022.dta", "data/external/input/NSDUH/NSDUH_2022.dta")
file.copy("data/raw/ESS/ESS6e02_6-subset.csv", "data/external/input/ESS/ESS6e02_6-subset.csv")

## Execute scripts to create outputs
source("data/external/code/prepare_passive_tracking.R")
source("data/external/code/prepare_NSDUH.R")
source("data/external/code/prepare_ESS_6.R")

message("
  ################################
  # EXTERNAL SUBMODULE COMPLETED #
  ################################
")
