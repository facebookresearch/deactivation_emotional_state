# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

#############################
###   analysis - make.R   ###
#############################

print("####  PRIMARY   ####")
source("analysis/primary/make.R")
print("##############################")

print("####  PRIMARY ALT   ####")
source("analysis/primary_alt/make.R")
print("##############################")

print("####  DESCRIPTIVE   ####")
source("analysis/descriptive/make.R")
print("##############################")

print("####  SUBSTITUTION   ####")
source("analysis/substitution/make.R")
print("##############################")

print("####  HETEROGENEOUS   ####")
source("analysis/heterogeneous/make.R")
print("##############################")

print("####  COMPARISON   ####")
source("analysis/comparison/make.R")
print("##############################")
