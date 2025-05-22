# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

#' Display numbers in a pretty format
#' @param num (numeric vector) Numbers to display
#' @param pct (Boolean) Whether number is a proportion/percentage
#' @param digits (numeric) Number of decimal digits to display
#' @export
display <- function(num, pct=FALSE, digits=3){
  if (!pct) return(sprintf(paste0("%.", digits, "f"), num))
  if (pct) return(sprintf(paste0("%.", digits, "f"), num*100))
}
