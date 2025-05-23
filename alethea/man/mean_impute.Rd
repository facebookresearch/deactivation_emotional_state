% Copyright (c) Meta Platforms, Inc. and affiliates.
% All rights reserved.

% This source code is licensed under the license found in the
% LICENSE file in the root directory of this source tree.

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis.R
\name{mean_impute}
\alias{mean_impute}
\title{Single imputation of missing values}
\usage{
mean_impute(x, seed = 2020, deact_impute = FALSE)
}
\arguments{
\item{x}{(numeric or character) Vector that contains data with missing values
that need to be imputed}

\item{seed}{(integer) Random number generator state}

\item{deact_impute}{(boolean) Whether to use mean imputation protocol outlined in
deactivation PAP}
}
\description{
Function to impute missing values in a covariate to the mode (categorical) or
mean (continuous).
}
