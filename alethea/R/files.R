# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

# -*- coding: utf-8 -*-
#' Read file from ICPSR's data enclave

#' @param filename (character) The name of the file to download
#' @param has_header Whether the file has a header or not
#' @param dlm (character) The delimiter to split the columns.  If ‘guess’ it uses the
#'  end of the file (‘.tsv’,’.csv’) to infer the delimiter
#' @param col_types (character) Column types in file. \code{NULL} will impute
#' from the first 1000 rows in the input. See documentation for \code{read.csv}
#' for additional information.
#' @param show_progress (logical) Whether to show a progress bar
#'
#' @export

read_file <- function(filename, has_header=FALSE, dlm='guess',
                      col_types=NULL, show_progress=FALSE){
  if (show_progress){ message("Reading object to memory...")}
  d <- fread(filename, sep=ifelse(dlm=='guess', 'auto', dlm),
              header=has_header, colClasses=col_types, showProgress=show_progress,
              encoding='UTF-8')
  if (show_progress){ message("Done!")}
  return(as.data.frame(d))
}
