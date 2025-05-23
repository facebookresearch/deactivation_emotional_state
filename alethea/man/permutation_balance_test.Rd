% Copyright (c) Meta Platforms, Inc. and affiliates.
% All rights reserved.

% This source code is licensed under the license found in the
% LICENSE file in the root directory of this source tree.

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis.R
\name{permutation_balance_test}
\alias{permutation_balance_test}
\title{Permutation Test of Covariate Balance}
\usage{
permutation_balance_test(
  data,
  control,
  trt,
  covars,
  treatment_var = "treatment_group",
  sims = 1000
)
}
\arguments{
\item{data}{(data frame) Data frame that contains survey data}

\item{control}{(character) Name of control group}

\item{trt}{(character) Name of treatment group}

\item{covars}{(character vector) Names of covariates in survey dataset that
will be used for the covariate balance test.}

\item{treatment_var}{(character) Name of treatment group assignment variable.
Default "treatment_group"}

\item{sims}{(numeric) Number of simulated samples for permutation test}
}
\description{
Similar to \code{balance_f_test()}, but here we use a permutation test
(randomization inference) to calculate the p-value associated with the
Wald statistic.
#'
}
