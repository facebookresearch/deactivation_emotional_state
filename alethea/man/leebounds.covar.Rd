% Copyright (c) Meta Platforms, Inc. and affiliates.
% All rights reserved.

% This source code is licensed under the license found in the
% LICENSE file in the root directory of this source tree.

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lee_bounds_covar.R
\name{leebounds.covar}
\alias{leebounds.covar}
\title{Estimate Lee (2009) bounds with covariates}
\usage{
leebounds.covar(
  d,
  dv,
  covars,
  completed_var = NULL,
  weights_var = NULL,
  n_buckets = 10,
  include.table = TRUE
)
}
\arguments{
\item{d}{(data frame) Data frame that contains the data with a boolean variable 'treated'}

\item{dv}{(character) Name of the DV}

\item{covars}{(character vector) List of covariates for predicting outcome}

\item{completed_var}{(character) Name of the boolean variable for whether the
user completed the study or dropped out of the study.  If NULL (default), assumes instances
where DV is NULL represent users who dropped out of the study.}

\item{weights_var}{(character) Optional. Name of the weights variable. Defaul NULL.}

\item{n_buckets}{(numeric) Number of buckets to split predicted outcome. Must
be coarse enough to allow trimming of attritted users in each bucket. Default is 10.}

\item{include.table}{(boolean) Whether to include the table of estimates for each bucket
in the output. Default TRUE.}
}
\description{
This function uses the process defined in Lee (2009) to estimate bounds on the ATE
given an outcome, treatment indicator, study-completion indicator, and covariates.
Outcomes are predicted for all rows given covariates, users are binned into n_buckets by
predicted outcome, and within each bucket the sorted outcomes are trimmed for the group
with fewer attritted users to identify the "always-takers".
}
