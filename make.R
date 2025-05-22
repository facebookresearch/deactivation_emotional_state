# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

##################################
####  MAKE - EMOTIONAL STATE  ####
##################################

setwd("~/replication-emotional-state")
source("install_required_packages.R")
print("##############################")
print("####         DATA         ####")
print("##############################")
source("load_data_files.R")
source("download_raw_files.R")
source("data/make.R")
print("##############################")


print("##############################")
print("####       ANALYSIS       ####")
print("##############################")
source("analysis/make.R")
print("##############################")
