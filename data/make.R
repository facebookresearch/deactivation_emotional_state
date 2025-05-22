# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

#########################
###   data - make.R   ###
#########################

print("####  PLATFORM   ####")
source("data/platform/make.R")
print("##############################")

print("####  SURVEY   ####")
source("data/survey/make.R")
print("##############################")

print("####  SAMPLE   ####")
source("data/sample/make.R")
print("##############################")

print("####  EXTERNAL   ####")
source("data/external/make.R")
print("##############################")
