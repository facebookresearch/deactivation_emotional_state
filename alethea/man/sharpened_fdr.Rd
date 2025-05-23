% Copyright (c) Meta Platforms, Inc. and affiliates.
% All rights reserved.

% This source code is licensed under the license found in the
% LICENSE file in the root directory of this source tree.

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis.R
\name{sharpened_fdr}
\alias{sharpened_fdr}
\title{Implement sharpened FDR adjustment (Benjamini, Krieger, and Yekutieli 2006)
to adjust for multiple comparisons.}
\usage{
sharpened_fdr(pval)
}
\arguments{
\item{pval}{(numeric vector) Set of p values that need to be adjusted}
}
\description{
This code generates BKY (2006) sharpened two-stage q-values as described in
Anderson (2008), "Multiple Inference and Gender Differences in the Effects
of Early Intervention: A Reevaluation of the Abecedarian, Perry Preschool,
and Early Training Projects", Journal of the American Statistical
Association, 103(484), 1481-1495
BKY (2006) sharpened two-stage q-values are introduced in Benjamini, Krieger,
and Yekutieli (2006), "Adaptive Linear Step-up Procedures that Control the
False Discovery Rate", Biometrika, 93(3), 491-507
}
