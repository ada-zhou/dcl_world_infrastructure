# DT.ODA.ODAT.CD
# Reads in, wrangles, writes out data on child mortality
# Source: World Bank

# Author: Ada Zhou
# Version: 2020-02-01

# Libraries
library(tidyverse)
library(WDI)

# Indicator
INDICATORS <- 
  c(
    "development_aid" = "DT.ODA.ODAT.CD"
  )
# Countries
COUNTRY <- "all"
# Years Range
START <- NULL
END <- NULL


file_name <- "development_aid"
file_out <- here::here(str_glue("c01-own/data/{file_name}.rds"))
file_raw <- here::here(str_glue("c01-own/data-raw/{file_name}.csv"))

#===============================================================================

# wb(country = COUNTRY, indicator = INDICATORS, start = START, end = END) %>% 
#   write_rds(file_out)
WDI(country = COUNTRY, indicator = INDICATORS, start = START, end = END) %>% 
  write_rds(file_out)
