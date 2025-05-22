# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

#' Helper function to convert a map column to a list

#' @param json (character) Vector with column that contains data uploaded from Facebook
#' in map format, and which needs to be converted to a list.
#'
#'@examples
#'\dontrun{
#'d <- read_file(bucket='sandbox', filename='dim_op_fb_users.csv', has_header=TRUE)
#'d$vpv_count_category_map <- parse_map(d$vpv_count_category_map)
#'}
#' @export

parse_map <- function(json){
  # replacing NAs with empty JSON
  json[is.na(json) | json == ""] <- "{}"
  # replacing None values with Nulls
  json <- gsub("None", 'null', json)
  # fixing escaped characters
  json <- gsub("\'", '\"', json)
  # parsing JSON to a list
  parsed_json <- lapply(json, jsonlite::fromJSON)
  return(parsed_json)
}

#' Helper function to extract a value from a map column

#' @param map (list) Column within a data frame that corresponds to a map variable
#' @param key (character) Name of the key whose value needs to be extracted
#'
#'@examples
#'\dontrun{
#' # Compute average VPVs of civic content
#'d <- read_file(bucket='sandbox', filename='dim_op_fb_users.csv', has_header=TRUE)
#'d$vpv_count_category_map <- parse_map(d$vpv_count_category_map)
#'mean(unmap(d$vpv_count_category_map, 'civic'), na.rm=TRUE)
#'}
#' @export

unmap <- function(map, key){
  # extracting value from map
  values <- sapply(map, function(x) x[[key]])
  # replacing any NULL with NA
  values[which(sapply(values, is.null))] <- NA
  # returning values as a vector
  return(unlist(values))

}
