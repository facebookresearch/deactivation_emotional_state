alethea (deact): Modified alethea functions for the Deactivation paper of the US 2020 Election Research Project
---------

This package is an altered version of `alethea` that modifies and adds functionalities to the original package. The purpose is to ensure the `deactivation` analysis uses the `alethea` structure, while maintaining decisions that deviate from the main package implementation.

This package contains functions to read and clean survey responses, log data, and passive monitoring records, conduct standard analysis routines, and format output tables and graphs as part of the US 2020 Election Research Project.

As a rule of thumb, any code that needs to be run in more than one paper should go here, to make sure that we are making identical decisions in the data cleaning and analysis steps across the entire project.

# Divergences from the main `alethea` package
TBD

# How to install and load the package in R

If this is the first time you’re installing the package, you’ll need to do the following.

Copy the contents of GitHub folder `deactivation_emotional_state/alethea` to `~/alethea`. You should now be able to load the R package by running:

```
devtools::load_all('~/alethea')
```

# How to test and build the package after code changes

Clone the GitHub repository to your local machine or devserver, make changes, and then run:

```
devtools::document()
devtools::check(document=FALSE, cran=FALSE)
devtools::build(path='~/git/alethea')
```

This will (1) create the documentation using roxygen, (2) check the package for errors, and (3) build it. You should only need to do this if you are writing new code in the package; not if you're only using it interactively.

# File manipulation functions (`files.R`)

## `read_file`

Download a specific file from a bucket and read it as a data.frame. Returns an error if the file is not found.

Usage:

```
w1_data  <- get_file('roberts_example.csv', has_header=True,
	dlm=',', col_types='ccc')
```
Parameters:

- `filename` (character) The name of the file to download
- `has_header` (logical) Whether the file has a header or not
dlm (str): The delimiter to split the columns.  If 'guess' it uses the end of the file ('.tsv','.csv') to infer the delimiter
- `col_types` (character) Column types in file. NULL will impute from the first 1000 rows in the input. See documentation for read.csv for additional information.
- `show_progress` (logical) Whether to show a progress bar.

Return type:
- data frame

# Passive monitoring data functions (`passive.R`)

## `parse_passive_data`

Read raw passive monitoring files (for web visits and app usage) and output aggregated daily tables, with different levels of granularity, which will then be read by downstream functions.

Produces the files `agg_passive_{SOURCE}_data_by_{AGGREGATION}_daily.csv`, which are stored in the sandbox bucket, where `{SOURCE}` is either 'app' or 'web'; and `{AGGREGATION}` is either 'app', 'domain', 'category' or 'panelist'.

For additional information, see passive monitoring update slide deck.

Parameters:

- `min_date` (character) start date for data collection, in YYYY-MM-DD format
- `max_date` (character) end date for data collection, in YYYY-MM-DD format
- `verbose` (logical) If \code{TRUE} (default), show informative messages indicating function progress

## `read_passive_data`

Read aggregate passive monitoring files (for web visits and app usage) and return a panelist-level dataset with counts of web visits or app opens and time spent on apps, according to a set of filters.

Note that `study_group` for now will only work with platform intervention groups (not deactivation).

Usage:

```
psv <- read_passive_data(source='web', min_date='2020-10-01', max_date='2020-11-30',
    study_group=c('product_intervention_control', 'product_intervention_ranking_chrono_feed'),
    study_platform='FB',
    domain_dictionary=list('news' = c('nytimes.com', 'foxnews.com')))
```

Parameters:

- `source` (character) 'web' or 'app'
- `min_date` (character) start date for data to be returned. Default is 2020-09-12
- `max_date` (character) end data for data to be returned. Default is 2020-12-24
- `study_group` (character vector) vector with study groups for which respondent data will be returned. Names of study groups must match those in dataset:
	* deactivation
	* deactivation_control
	* product_intervention_control
	* product_intervention_holdout_misinfo_ro
	* product_intervention_holdout_political_ads
	* product_intervention_holdout_reshared_content
	* product_intervention_holdout_targeted_political_ads
	* product_intervention_ranking_chrono_feed
	* product_intervention_ranking_demotion_politically_likeminded
	* product_intervention_ro_control
- `study_platform` (character vector) study platforms for which respondent data will be returned. Either `IG` or `FB`.
- `domain_dictionary` (named list) a dictionary where each key is a category (e.g. 'news') and the value is a vector with domains that fall in that category. Only if `source` = 'web'
If not NULL, it will create a new column that indicates the new category for each domain
- `app_dictionary` (named list) a dictionary where each key is a category (e.g. 'news') and the value is a vector with apps that fall in that category. Only if `source` = 'app'
If not NULL, it will create a new column that indicates the new category for each app


# Log data functions (`logdata.R`)

Functions to read and clean log data imported from Hive

## `parse_map`

Helper function to convert a map column to a list

Usage:

```
d <- read_file(bucket='sandbox', filename='dim_op_fb_users.csv', has_header=TRUE)
d$vpv_count_category_map <- parse_map(d$vpv_count_category_map)
```

Parameters:

- `json` (character) Vector with column that contains data uploaded from Facebook in map format, and which needs to be converted to a list.

## `unmap`

Helper function to extract a value from a map column.

Usage:

```
# Compute average VPVs of civic content
d <- read_file(bucket='sandbox', filename='dim_op_fb_users.csv', has_header=TRUE)
d$vpv_count_category_map <- parse_map(d$vpv_count_category_map)
mean(unmap(d$vpv_count_category_map, 'civic'), na.rm=TRUE)
```

Parameters:

- `map` (list) Column within a data frame that corresponds to a map variable
- `key` (character) Name of the key whose value needs to be extracted

# Summary functions (`summary.R`)

Functions to create summary statistics for all datasets and other summarization functions.

# Analysis functions (`analysis.R`)

Functions to conduct standard analysis routines, such as methods described in Standard Operating Procedures (defined as anything that needs to be done in more than one PAP. Examples: covariate balance checks, recoding missing values, winsorization, lasso on covariates, ITT/CACE estimates, multiple tests adjustment)

## winsorize

Winsorize raw count at a given percentile threshold

Parameters:
- `pct` (numeric) Percentile at which to winsorize

## create_composite_scale

Create composite scale from multiple survey items using Principal Component Analysis with varimax rotation.

Parameters:

- `svy`(data frame) Data frame that contains survey data
- `items`	(character vector) List of variables that will be jointly scaled.
- `control_group` (character) Value of the treatment_group variable in the svy data frame that corresponds to the control group in the study. If NULL, the composite scale will be computed using all observations in the survey dataset.
- `nfactors`(numeric) Number of components to be extracted. This is only for exploratory purposes; the function will always return a single score.
- `impute` (Boolean) If TRUE, impute missing values using the median.
- `impute_function` (character) Either 'median' or 'mean'. Function used to replace missing values when imputing.
- `missingness_report` (Boolean) If TRUE, returns report with details about missing values in each item and across respondents.
- `verbose` (Boolean) If TRUE (default), print factor loadings.

## balance_f_test

Fit OLS regression (one per treatment arm, with a sample that includes the control and that treatment arm) of the treatment indicators on a set of covariates (dummy variables for blocks will automatically be included) and calculate a heteroskedasticity-robust Wald statistic for the hypothesis that all the coefficients on the covariates (excluding block dummies) are zero.

Parameters:

- `data` (data frame) Data frame that contains survey data
- `control` (character) Name of control group
- `trt` (character) Name of treatment group
- `covars` (character vector) Names of covariates in survey dataset that will be used for the covariate balance test.

## balance_table

Create standard balance tables in which covariate proportions are listed in rows and columns correspond to the control and treatment arms. An additional column contains the treatment-control difference and a p-value for a t-test of the difference in means.

Parameters:

- `data` (data frame) Data frame that contains survey data
- `control` (character) Name of control group
- `trt` (character) Name of treatment group
- `covars` (character vector) Names of covariates in survey dataset that will be used for the covariate balance test.
- `output_file`(character) Name of file where table will be stored (in latex format)

## sharpened_fdr

Implement sharpened FDR adjustment (Benjamini, Krieger, and Yekutieli 2006) to adjust for multiple comparisons.

Parameters:

- `pValues` (numeric vector) Set of p values that need to be adjusted

## baseline_lasso

A simple function to select variables for baseline_model using Lasso

- `d` (data frame) Data frame that contains the data
- `dv` (character) Name of the DV
- `dv_pre` (character) Name of the pre-treatment DV. \code{NULL} if not available
- `lasso_covars` (character vector) List of covariates to add to lasso model for selection
- `treatment_var` (character) Name of treatment group assignment variable. Default "treatment_group"
- `trt` (character) Name of treatment group
- `control` (character) Name of control group
- `seed` (numeric) Seed for random numbers for repeatability.  Default 2020.

## leebounds_covar

Estimate Lee (2009) bounds with covariates

This function uses the process defined in Lee (2009) to estimate bounds on the ATE
given an outcome, treatment indicator, study-completion indicator, and covariates.
Outcomes are predicted for all rows given covariates, users are binned into n_buckets by
predicted outcome, and within each bucket the sorted outcomes are trimmed for the group
with fewer attritted users to identify the "always-takers".

- `d` (data frame) Data frame that contains the data with a boolean variable 'treated'
- `dv` (character) Name of the DV
- `covars` (character vector) List of covariates for predicting outcome
- `completed_var` (character) Name of the boolean variable for whether the user completed the study or dropped out of the study.  If NULL (default), assumes instances where DV is NULL represent users who dropped out of the study.
- `weights_var` (character) Optional. Name of the weights variable. Defaul NULL.
- `n_buckets` (numeric) Number of buckets to split predicted outcome. Must be coarse enough to allow trimming of attritted users in each bucket. Default is 10.
- `include`.table (boolean) Whether to include the table of estimates for each bucket in the output. Default TRUE.

## baseline_model

Estimate treatment effects using a baseline model with lasso covariate selection.

This function follows a two-step procedure. First, it estimates a lasso model where the DV is regressed on its pre-treatment measure (if available), as well as a set of covariates measured prior to treatment. Then, it fits a linear model that includes the treatment indicator, the covariates selected by the lasso model and additional covariates. The function returns weighted (PATE), instrumented (LATE), and unweighted (SATE) treatment effects.


- `d` (data frame) Data frame that contains the data
- `dv` (character) Name of the DV
- `dv_pre` (character) Name of the pre-treatment DV. \code{NULL} if not available. Default NULL.
- `lasso_covars` (character vector) List of covariates to add to lasso model for selection
- `model_covars` (character vector) List of covariates to add to baseline model, regardless of whether they were selected by the lasso or not.
- `treatment_var` (character) Name of treatment group assignment variable. Default 'treatment_group'
- `trt` (character) Name of treatment group
- `control` (character) Name of control group
- `dv_label` (character) Label for the DV, which can then be used to create tables or graphs. Default is same as \code{dv}.
- `model_type` (character) Model type(s) to return.  Must be one of 'SATE', 'LATE', 'PLATE', 'PATE', or 'all' (default).
- `weights_var` (character) Name of the weights variable in the dataset. Must be included if \code{model_type} is 'all', 'PATE', or 'PLATE'.
- `iv` (character) Name in the dataset of the variable instrumented by the treatment. Must be included if \code{model_type} is 'all' or 'LATE'.
- `standardize` (logical) If \code{TRUE}, the DV will be standardized prior to fit the models, and the returning treatment effect estimate will be a standardized coefficient.
- `proportion` (logical) If \code{TRUE}, will multiply the DV by 100 prior to fitting the model. Useful if the DV is a percentage; so that the output is easier to read.
- `digits` (numeric) Number of decimal digits to display in output.
- `se_type` (character) Type of standard errors for treatment effect estimates. This is inherited from estimatr. Valid types are: "HC0", "HC1", "HC2", and "HC3". Default is "HC2"
- `impute` (boolean) Whether to use mean imputation for missing values of pretreatment covariates. Default is set to TRUE.
- `leebounds` (boolean) Whether to include Lee bounds for the estimates.
- `verbose` (boolean) Whether to print messages with estimated effects.

## lin_model

Estimate treatment effects using a Lin (2013) regression model with lasso covariate selection

This function follows a two-step procedure. First, it estimates a lasso model where the DV is regressed on its pre-treatment measure (if available), as well as a set of covariates measured prior to treatment. Then, it fits a Lin (2013) model that includes the treatment indicator, the covariates selected by the lasso model and additional covariates. The function returns both weighted (PATE) and unweighted (SATE) treatment effects.

- `d` (data frame) Data frame that contains the data
- `dv` (character) Name of the DV
- `dv_pre` (character) Name of the pre-treatment DV. NULL if not available
- `lasso_covars` (character vector) List of covariates to add to lasso model for selection
- `model_covars` (character vector) List of covariates to add to baseline model, regardless of whether they were selected by the lasso or not.
- `trt` (character) Name of treatment group
- `control` (character) Name of control group
- `dv_label` (character) Label for the DV, which can then be used to create tables or graphs
- `weights_var` (character) Name of the weights variable in the dataset
- `standardize` (logical) If TRUE, the DV will be standardized prior to fit the models, and the returning treatment effect estimate will be a standardized coefficient.
- `proportion` (logical) If TRUE, will multiply the DV by 100 prior to fitting the model. Useful if the DV is a percentage; so that the output is easier to read.
- `digits` (numeric) Number of decimal digits to display in output.

# Figure formatting functions (`figures.R`)

## theme_alethea

Add default theming to ggplots.  Includes default color and fill optimized to be colorblind-friendly.

Usage:

```
theme_set(theme_alethea())
```

# Heterogeneous treatment effects analysis (`hte.R`)

Functions to conduct heterogeneous treatment analysis

# Table formatting functions (`tables.R`)

Functions to format output tables (e.g. apsrtable)
