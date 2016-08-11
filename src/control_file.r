install.packages("R.utils")
install.packages("tidyr")
install.packages("dplyr")
install.packages('data.table')
options(scipen=999)
require(R.utils)
require(dplyr)
require(tidyr)
require(data.table)

path_code = "C:/Users/bloh356/Documents/GitHub/fuzzy-waffle/src/"
path_data = "C:/Users/bloh356/Documents/GitHub/fuzzy-waffle/data/"
#path_data = "/Users/bloh356/Documents/fuzzy-waffle/data/"
# Local path to the cloned data and code folders in the repository

source(paste0(path_code,"mapping_860_overnight_R.r"))
source(paste0(path_code, "ca_almanac_R.r"))
