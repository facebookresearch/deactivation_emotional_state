## Emotional-State

## License
Attribution-NonCommercial 4.0 International

## File directory structure

* data (*Code for processing the data*)
* analysis (*Code for analyzing the data*)

Simplify execute `make.R`, which will: 
1. Download all external files;
2. Copy the content of the GitHub repo `deactivation_emotional_state` to `~/replication_emotional_state` or modify the `setwd` command on line 11 in `deactivation_emotional_state/make.R` and line 12 in `deactivation_emotional_state/make.ipynb` to match the local location of the repo.
3. Copy the contents of GitHub folder `deactivation_emotional_state/alethea` to a local location of `~/alethea`.
4. Execute the `data` module, creating the main analysis sample; 
5. Execute the `analysis` module, performing all of the analyses in the paper.
6. Note that this repo *does not* contain the data files used to produce the analyses in the paper, "The Effect of Deactivating Facebook and Instagram on Users’ Emotional State." Reproducing the analyses and outputs of this code requires applying to ICPSR for data access: https://www.icpsr.umich.edu/web/about/cms/5024


Environment specs: 
* R version 4.3.3 (2024-02-29)
* Platform: x86_64-conda-linux-gnu (64-bit)
* Running under: Ubuntu 22.04.5 LTS
* "aws.s3" = "0.3.21",
* "binsreg" = "1.0",
* "car" = "3.1.-",
* "cowplot" = "1.1.1",
* "data.table" = "1.15.4",
* "dplyr" = "1.1.4",
* "fastDummies" = "1.7.4",
* "fixest" = "0.11.1",
* "forcats" = "1.0.0",
* "ggplot2" = "3.5.1",
* "ggpubr" = "0.4.0",
* "ggforce" = "0.4.2",
* "gridExtra" = "2.3",
* "haven" = "2.5.4",
* "Hmisc" = "5.2-1",
* "janitor" = "2.2.1",
* "kableExtra" = "1.3.4",
* "lubridate" = "1.9.4",
* "patchwork" = "1.3.0",
* "purrr" = "1.0.2",
* "scales" = "1.3.0",
* "stringr" = "1.5.1",
* "tibble" = "3.2.1",
* "tidyr" = "1.3.1",
* "xtable" = "1.8-4"
