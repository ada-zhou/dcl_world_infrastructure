# Joins infrastructure and urban growth data 

# Source: World Bank

# Author: Ada Zhou
# Version: 2020-02-18

# Libraries
library(tidyverse)

# Parameters
# File with city boundaries
file_infrastructure <-
  here::here(str_glue("c01-own/data/infrastructure.rds"))
# File with evictions data
file_urban_growth <- 
  here::here(str_glue("c01-own/data/urban_growth.rds"))
# Output file
file_out <- 
  here::here(
    str_glue("c01-own/data/infrastructure_urban_growth.rds")
  )

#===============================================================================

read_rds(file_infrastructure) %>%
  left_join(
    read_rds(file_urban_growth),
    by = c("iso2c", "country", "year")
  ) %>%
  write_rds(file_out)
