# Reads in, wrangles, writes out data on Infrastructure from World Bank
# Source: World Bank

# Author: Ada Zhou
# Version: 2020-02-01

# Libraries
library(tidyverse)
library(WDI)


# Parameters
# Indicator
INDICATORS <- 
  c(
    "electricity_urban" = "EG.ELC.ACCS.UR.ZS",
    "electricity_rural" = "EG.ELC.ACCS.RU.ZS",
    "water_urban" = "SH.H2O.BASW.UR.ZS",
    "water_rural" = "SH.H2O.BASW.RU.ZS",
    "sanitation_urban" = "SH.STA.BASS.UR.ZS",
    "sanitation_rural" = "SH.STA.BASS.RU.ZS"
  )
# Countries
COUNTRY <- "all"
# Years Range
START <- NULL
END <- NULL

# Raw data download (service not available)
url_data <-
  "http://api.worldbank.org/v2/en/indicator/EN.POP.SLUM.UR.ZS?downloadformat=csv"

file_name <- "infrastructure"
file_out <- here::here(str_glue("c01-own/data/{file_name}.rds"))
file_raw <- here::here(str_glue("c01-own/data-raw/{file_name}.csv"))

#===============================================================================

WDI(country = COUNTRY, indicator = INDICATORS, start = START, end = END) %>% 
  write_rds(file_out)

# Download raw data as backup
# url_data %>% 
#   download.file(file_raw)
