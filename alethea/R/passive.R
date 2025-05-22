# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

#' Parse raw passive monitoring data
#'
#' Read raw passive monitoring files (for web visits and app usage)
#' and output aggregated daily tables, with different levels of
#' granularity, which will then be read by downstream functions.
#'
#' Note that the domain-level dataset for web visits will only include domains
#' that were visited by 20 or more unique panelists. This k-anonymization
#' procedure was recommended by Privacy Engineering to reduce the risk of
#' re-identification of our panelists.
#'
#' This function can only be run by Facebook researchers, since it requires
#' access to the visit-level passive monitoring data.
#'
#' @param min_date (character) start date for data collection, in YYYY-MM-DD format
#' @param max_date (character) end date for data collection, in YYYY-MM-DD format
#' @param verbose (logical) If \code{TRUE} (default), show informative
#' messages indicating function progress
#'
#' @export

parse_passive_data <- function(min_date, max_date, verbose=TRUE){

  # checking format of dates
  min_date <- as.Date(min_date, format='%Y-%m-%d')
  max_date <- as.Date(max_date, format='%Y-%m-%d')
  if (is.na(min_date)){
    stop("Error: min_date not properly formatted.")
  }
  if (is.na(max_date)){
    stop("Error: max_date not properly formatted.")
  }

  # read list of files with passive monitoring data
  fls <- list_bucket_files(bucket = 'staging', prefix = 'FB/Passive')

  # exclude files that are duplicated / deprecated
  toexclude <- grep(
    paste(c('FB/Passive/ERC/Passive/11.19.2020_Data_Delivery/app/erp2020_app_data_2020_11_13',
            'FB/Passive/ERC/Passive/11.19.2020_Data_Delivery/web/erp2020_web_data_2020_11_13',
            'FB/Passive/ERC/Passive/11.19.2020_Data_Delivery/app/erp2020_app_data_2020_11_19',
            'FB/Passive/ERC/Passive/11.19.2020_Data_Delivery/web/erp2020_web_data_2020_11_19'),
          collapse="|"), fls$Key)
  fls <- fls[-toexclude,]
  fls <- fls[fls$Size>0,]

  # add file covariates to data frame so that it's easier to subset files
  fls$date <- gsub('.*_data_([0-9_]{10}).*', fls$Key, replacement="\\1") %>%
    gsub('_', '-', .) %>% as.Date()
  fls$source <- ifelse(grepl('web_data', fls$Key), 'web', 'app')
  fls <- fls %>% filter(!is.na(date))

  #######################
  ### PARSE WEB FILES ###
  #######################

  # select relevant files
  web_files <- fls %>% filter(source=="web")

  # dfs to be filled with data in loop below
  web_data <- list()
  cat_data <- list()
  user_data <- list()

  # selecting relevant dates
  dates <- seq(min_date, max_date, by=1)
  if (verbose) pb <- txtProgressBar(min = 0, max = length(dates), style = 3)

  for (i in 1:length(dates)){

    date_files <- web_files %>% filter(date == dates[i])

    date_data <- lapply(date_files$Key, function(x)
      read_file(bucket='staging', filename=x, has_header=TRUE, col_types=col_types_passive('web')))

    pd <- do.call(rbind, date_data)

    # data at date/panelist/category/domain level
    web_data[[i]] <- pd %>%
      rename(date = Date, domain=PrivateDomain, domain_category=CategoryV2) %>%
      group_by(date, P_PSID, domain_category, domain) %>%
      summarise(
        web_visits = n(),
        .groups = 'keep') %>%
      ungroup()
    web_data[[i]] <- web_data[[i]] %>% filter(!is.na(domain))

    # creating multiple rows for domains with 1+ categories
    cats <- sapply(web_data[[i]]$domain_category, strsplit, split=", ")
    web_data[[i]]$domain_category_1 <- unname(unlist(lapply(cats, function(x) x[1])))
    web_data[[i]]$domain_category_2 <- unname(unlist(lapply(cats, function(x) x[2])))
    web_data[[i]]$domain_category_3 <- unname(unlist(lapply(cats, function(x) x[3])))
    dup_data <- web_data[[i]] %>%
      select(-domain_category) %>%
      melt(id.vars=c("date", "P_PSID", "domain", "web_visits")) %>%
      filter(!is.na(value)) %>%
      rename(domain_category = value) %>%
      select(date, P_PSID, domain_category, domain, web_visits)

    # deleting new columns that we no longer need
    web_data[[i]] <- web_data[[i]] %>%
      select(date, P_PSID, domain_category, domain, web_visits)

    # data at date/panelist/category level
    cat_data[[i]] <- dup_data %>%
      group_by(date, P_PSID, domain_category) %>%
      summarise(
        web_visits = sum(web_visits),
        .groups = 'keep') %>%
      ungroup()

    # data at date/panelist level
    user_data[[i]] <- web_data[[i]] %>%
      group_by(date, P_PSID) %>%
      summarise(
        web_visits = sum(web_visits),
        .groups = 'keep')  %>%
      ungroup()

    if (verbose) setTxtProgressBar(pb, i)
  }

  if (verbose) message("Aggregating web files...")
  web_data <- do.call(rbind, web_data)
  cat_data <- do.call(rbind, cat_data)
  user_data <- do.call(rbind, user_data)

  if (verbose) message("Applying k-anonymization on domain-level dataset...")
  summ <- web_data %>%
    group_by(domain) %>%
    summarise(unique_panelists = length(unique(P_PSID)))
  kdomains <- summ$domain[summ$unique_panelists>=20]
  web_data <- web_data[web_data$domain %in% kdomains,]

  if (verbose) message("Saving web files to sandbox bucket...")
  write_file(df=web_data, bucket='sandbox', filename='agg_passive_web_data_by_domain_daily.csv')
  write_file(df=cat_data, bucket='sandbox', filename='agg_passive_web_data_by_category_daily.csv')
  write_file(df=user_data, bucket='sandbox', filename='agg_passive_web_data_by_panelist_daily.csv')

  if (verbose) message("Done.")

  #######################
  ### PARSE APP FILES ###
  #######################

  # select relevant files
  app_files <- fls %>% filter(source=="app")

  # dfs to be filled with data
  app_data <- list()
  cat_data <- list()
  user_data <- list()

  # selecting relevant dates
  dates <- seq(min_date, max_date, by=1)
  if (verbose) pb <- txtProgressBar(min = 0, max = length(dates), style = 3)

  for (i in 1:length(dates)){
    date_files <- app_files %>% filter(date == dates[i])

    date_data <- lapply(date_files$Key, function(x)
      read_file(bucket='staging', filename=x, has_header=TRUE, col_types=col_types_passive('app')))

    pd <- do.call(rbind, date_data)

    # data at date/panelist/category/app level
    app_data[[i]] <- pd %>%
      rename(date = StartDate, app_category=AppCategory, app_name=AppName) %>%
      group_by(date, P_PSID, app_category, app_name) %>%
      summarise(
        app_opens = n(),
        time_spent = sum(SessionDuration),
        .groups = 'keep') %>%
      ungroup()

    # data at date/panelist/category level
    cat_data[[i]] <- app_data[[i]] %>%
      group_by(date, P_PSID, app_category) %>%
      summarise(
        app_opens = sum(app_opens),
        time_spent = sum(time_spent),
        .groups = 'keep') %>%
      ungroup()

    # data at date/panelist level
    user_data[[i]] <- cat_data[[i]] %>%
      group_by(date, P_PSID) %>%
      summarise(
        app_opens = sum(app_opens),
        time_spent = sum(time_spent),
        .groups = 'keep')  %>%
      ungroup()


    if (verbose) setTxtProgressBar(pb, i)
  }

  if (verbose) message("Aggregating app files...")
  app_data <- do.call(rbind, app_data)
  cat_data <- do.call(rbind, cat_data)
  user_data <- do.call(rbind, user_data)

  if (verbose) message("Saving app files to sandbox bucket...")
  write_file(df=app_data, bucket='sandbox', filename='agg_passive_app_data_by_app_daily.csv')
  write_file(df=cat_data, bucket='sandbox', filename='agg_passive_app_data_by_category_daily.csv')
  write_file(df=user_data, bucket='sandbox', filename='agg_passive_app_data_by_panelist_daily.csv')

  if (verbose) message("Done.")
}

#' Create column specification for passive monitoring data, to be used
#' internally by \code{read_csv} to properly parse survey files.
#'
#' @param source (character) "web" or "app"
#'
#' @export
#'
col_types_passive <- function(source){
  if (source=='web'){
    return(c(
      GroupName = "character",
      P_PSID = "character",
      ParentClientId = "double",
      ClientId = "double",
      ClientKey = "character",
      OsName = "character",
      OsVersion = "character",
      DeviceType = "character",
      SessionStartTime = "POSIXct",
      StartTimeUtc = "POSIXct",
      Date = "Date",
      Time = "POSIXct",
      PageDomain = "character",
      BrowserVersion = "character",
      RefDomain = "character",
      ContentType = "character",
      ContentLength = "double",
      PrivateDomain = "character",
      CategoryV2 = "character"
    ))
  }
  if (source=='app'){
    return(c(
      GroupName = "character",
      P_PSID = "character",
      ParentClientId = "double",
      ClientId = "double",
      ClientKey = "character",
      OsName = "character",
      OsVersion = "character",
      DeviceType = "character",
      AppCategory = "character",
      AppName = "character",
      SessionStartTime = "POSIXct",
      SessionEndTime = "POSIXct",
      StartTimeUtc = "POSIXct",
      EndTimeUtc = "POSIXct",
      StartDate = "Date",
      StartTime = "POSIXct",
      EndDate = "Date",
      EndTime = "POSIXct",
      SessionDuration = "double",
      BundleId = "character"
    ))
  }
}


#' Read aggregate passive monitoring files
#'
#' This function reads the aggregate passive data files (for web visits and app usage)
#' and return a panelist-level dataset with counts of web visits or app opens
#' and time spent on apps, according to a set of filters.
#'
#' Note that \code{study_group} for now will only work with platform intervention
#' groups (not deactivation).
#'
#' @param source (character) 'web' or 'app', depending on the source of data
#' that needs to be aggregated
#' @param min_date (character) start date for data aggregation, in YYYY-MM-DD format
#' @param max_date (character) end date for data aggregation, in YYYY-MM-DD format
#' @param study_group (character vector) study groups for which respondent data
#' will be returned. Names of study groups must match those in database.
#' @param study_platform (character vector) study platforms for which respondent
#' data will be returned. Either \code{IG} or \code{FB}.
#' @param domain_dictionary (list) A name list where each key is a category
#' (e.g. 'misinfo') and the value is a vector with domains that fall in that
#'  category. Only if \code{granularity = 'domain'}.
#' @param app_dictionary (list) A name list where each key is a category
#' (e.g. 'misinfo') and the value is a vector with aop names that fall in that
#'  category. Only if \code{granularity = 'app'}
#' @param verbose (logical) If \code{TRUE} (default), show informative
#' messages indicating function progress
#'
#' @export

read_passive_data <- function(source, study_group='all',
                              study_platform=c('FB', 'IG'),
                              min_date='2020-09-12', max_date='2020-12-24',
                              domain_dictionary, app_dictionary, verbose=TRUE){
 if (source=='web'){
   if (verbose) message("Reading passive web data...")
   domain_data <- read_file(
                            filename='Passive/agg_passive_web_data_by_domain_daily.csv',
                            has_header=TRUE, show_progress=verbose)
   if (min_date != '2020-09-12' | max_date != '2020-12-24'){
      if (verbose) message("Filtering by date...")
     domain_data <- domain_data[domain_data$date >= min_date &
                                  domain_data$date <= max_date,]
   }
   if (all(study_group != 'all')){
     if (verbose) message("Filtering by study group...")
     assig <- read_file(
                        filename="Passive/treatment-assignment.csv",
                        has_header=TRUE)
     panelist_ids <- assig$tpid[assig$platform %in% study_platform &
                                  assig$treatment_group %in% study_group]
     domain_data <- domain_data[domain_data$P_PSID %in% panelist_ids,]
   }
   if (verbose) message("Applying domain dictionary...")
   # initialize data frame for all panelists in selected study groups
   out <- domain_data %>%
     group_by(P_PSID) %>%
     summarise(web_visits_total = sum(web_visits)) %>%
     ungroup() %>% as.data.frame()
   for (key in names(domain_dictionary)){
     if (verbose) message("Dictionary key: ", key)
     # count domains that match values of dictionary for each key
     summ <- domain_data %>%
       group_by(P_PSID) %>%
       filter(domain %in% domain_dictionary[[key]]) %>%
       summarise(count = sum(web_visits)) %>%
       ungroup()
     names(summ)[2] <- paste0('web_visits_', key)
     # join to data frame
     out <- left_join(out, summ, by = 'P_PSID')
     # NAs (respondents with 0 visits) are replaced with 0
     out[,paste0('web_visits_', key)] <- coalesce(out[,paste0('web_visits_', key)], 0)
   }
 }

  if (source=='app'){
    if (verbose) message("Reading passive app data...")
    app_data <- read_file(
                          filename='Passive/agg_passive_app_data_by_app_daily.csv',
                          has_header=TRUE, show_progress=TRUE)
    if (min_date != '2020-09-12' | max_date != '2020-12-24'){
      if (verbose) message("Filtering by date...")
      app_data <- app_data[app_data$date >= min_date &
                             app_data$date <= max_date,]
    }
    if (all(study_group != 'all')){
      if (verbose) message("Filtering by study group...")
      assig <- read_file(
                      filename="Passive/treatment-assignment.csv",
                      has_header=TRUE)
      panelist_ids <- assig$tpid[assig$platform %in% study_platform &
                                   assig$treatment_group %in% study_group]
      app_data <- app_data[app_data$P_PSID %in% panelist_ids,]
    }
    if (verbose) message("Applying app dictionary...")
    # initialize data frame for all panelists in selected study groups
    out <- app_data %>%
      group_by(P_PSID) %>%
      summarise(time_spent_total = sum(time_spent),
                app_opens_total = sum(app_opens)) %>%
      ungroup() %>% as.data.frame()
    for (key in names(app_dictionary)){
      if (verbose) message("Dictionary key: ", key)
      # count app that match values of dictionary for each key
      summ <- app_data %>%
        group_by(P_PSID) %>%
        filter(app_name %in% app_dictionary[[key]]) %>%
        summarise(count = sum(time_spent), opens = sum(app_opens)) %>%
        ungroup()
      names(summ)[2] <- paste0('time_spent_', key)
      names(summ)[3] <- paste0('app_opens_', key)
      # join to data frame
      out <- left_join(out, summ, by = 'P_PSID')
      # NAs (respondents with 0 time spent or app opens) are replaced with 0
      out[,paste0('time_spent_', key)] <- coalesce(out[,paste0('time_spent_', key)], 0)
      out[,paste0('app_opens_', key)] <- coalesce(out[,paste0('app_opens_', key)], 0)
    }
  }
   return(out)
  }
