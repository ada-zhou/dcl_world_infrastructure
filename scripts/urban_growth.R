# Reads in, wrangles, writes out data on Urban Population Percentage from World Bank
# Source: 

# Author: Ada Zhou
# Version: 2020-02-01

# Libraries
library(tidyverse)
library(WDI)

# Parameters
# Indicator
INDICATOR <- c("urban_growth" = "SP.URB.GROW")
# Countries
COUNTRY <- "all"
# Years Range
START <- NULL
END <- NULL

# Raw data download (service not available)
# url_data <-
#   "http://api.worldbank.org/v2/en/indicator/SP.URB.TOTL.IN.ZS?downloadformat=csv"

file_name <- "urban_growth"
file_out <- here::here(str_glue("c01-own/data/{file_name}.rds"))
file_raw <- here::here(str_glue("c01-own/data-raw/{file_name}.csv"))

#===============================================================================

WDI(country = COUNTRY, indicator = INDICATOR, start = START, end = END) %>% 
  write_rds(file_out)

# Download raw data as backup
# url_data %>% 
#   download.file(file_raw)
