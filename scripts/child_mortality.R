# SH.DYN.MORT
# Reads in, wrangles, writes out data on child mortality
# Source: World Bank

# Author: Ada Zhou
# Version: 2020-02-01

# Libraries
library(tidyverse)
library(WDI)
library(wbstats)


# Parameters


# Indicator
INDICATORS <- 
  c(
    "child_mortality" = "SH.DYN.MORT"
  )
# Countries
COUNTRY <- "all"
# Years Range
START <- NULL
END <- NULL

# Raw data download (service not available)
url_data <-
  "http://api.worldbank.org/v2/en/indicator/EN.POP.SLUM.UR.ZS?downloadformat=csv"

file_name <- "child_mortality"
file_out <- here::here(str_glue("c01-own/data/{file_name}.rds"))
file_raw <- here::here(str_glue("c01-own/data-raw/{file_name}.csv"))

#===============================================================================

# wb(country = COUNTRY, indicator = INDICATORS, start = START, end = END) %>% 
#   write_rds(file_out)
WDI(country = COUNTRY, indicator = INDICATORS, start = START, end = END) %>% 
  write_rds(file_out)
