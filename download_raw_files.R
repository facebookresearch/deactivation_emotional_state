# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

############################
###  DOWNLOAD RAW FILES  ###
############################

devtools::load_all('~/alethea')

main <- function() {

   file.copy('/data/us2020/emotional_state_replication/data/ESS.zip', 'ESS.zip')
   file.copy('/data/us2020/emotional_state_replication/data/NSDUH.zip', 'NSDUH.zip')

    # Move zip files to data/raw
    file.rename('ESS.zip', 'data/raw/ESS.zip')
    file.rename('NSDUH.zip', 'data/raw/NSDUH.zip')

    # Unzip files
    unzip('data/raw/ESS.zip', exdir='data/raw/ESS')
    unzip('data/raw/NSDUH.zip', exdir='data/raw/NSDUH')

}

## Execute
main()
