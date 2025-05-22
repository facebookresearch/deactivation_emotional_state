# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

#############################
###   lib - constants.R   ###
#############################

# iv variables
iv_fb <- "D_i"
iv_ig <- "D_i"

# experiment key dates
exp_dates <- list(
  # baseline period - wave 2 survey
  base = list(start = "2020-09-15", end = "2020-09-21"),
  # initial deactivation period (for all treatment groups)
  base_ctrl = list(start = "2020-09-23", end = "2020-09-29"),
  # endline period - wave 4 survey (deactivation for test group)
  end = list(start = "2020-09-30", end = "2020-11-03"),
  # post-endline period, wave 5 survey
  post = list(start = "2020-11-04", end = "2020-11-17")
)

# colors
maroon <- '#94343c'
grey   <- '#848484'
orange <- '#F77737'
navy   <- 'dodgerblue4'
green  <- "#234f1e" 

partyid_colors <- c("blue", "skyblue1", "grey", "indianred2", "red")

k6_variables <- c("DSTNRV30", "DSTHOP30", "DSTRST30", "DSTCHR30", "DSTEFF30", "DSTNGD30")
