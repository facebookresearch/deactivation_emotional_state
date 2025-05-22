# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

install.packages("https://cloud.r-project.org/src/contrib/00Archive/qvalue/qvalue_1.26.0.tar.gz", repos=NULL, type="source", verbose=FALSE)
require(qvalue)

renv::use(
    # Dependencies of required packages
    "purrr@1.0.2",
    "broom@1.0.5",
    "doBy@4.6.26",
    "utf8@1.2.2",
    "matrixStats@1.3.0",
    "stringi@1.7.12",
    "nloptr@2.0.0",
    "ps@1.7.7",
    "fs@1.6.2",
    "microbenchmark@1.4.10",
    "curl@5.2.1",
    "openssl@2.2.1",
    "xml2@1.3.3",
    "sys@3.4",
    "rbibutils@2.2.16",
    "SparseM@1.81",
    "quantreg@5.96",
    "gert@1.9.2",
    "httr2@1.0.2",
  
    # Required packages
    "devtools@2.4.2",
    "dplyr@1.1.2",
    "ggplot2@3.5.1",
    "cowplot@1.1.1",
    "ggpubr@0.6.0",
    "lubridate@1.9.2",
    "tidyverse@1.3.2",
    "xtable@1.8-4",
    "stringr@1.5.0",
    "tibble@3.2.1",
    "car@3.1-2",
    "patchwork@1.2.0",
    "magrittr@2.0.3",
    "tidyr@1.3.0",
    "forcats@1.0.0",
    "plm@2.6-4",
    "gridExtra@2.3",
    "data.table@1.15.4",
    "aws.s3@0.3.21",
    "reticulate@1.30",
    "scales@1.3.0",
    "reshape2@1.4.4",
    "Hmisc@4.8-0",
    "readr@2.1.4",
    "psych@2.4.6.26",
    "glmnet@4.1-2",
    "estimatr@1.0.0",
    "AER@1.2-10",
    "fastDummies@1.6.3",
    "janitor@2.2.1",
    "haven@2.5.4",
    "binsreg@1.0",

    # Additional dependencies:
    "stringmagic@1.1.1",
    "tweenr@2.0.2",
    "systemfonts@1.2.2",

    # Additional required packages:
    "fixest@0.11.1",
    "ggforce@0.4.2",
    "kableExtra@1.3.4",
    attach=TRUE
)
