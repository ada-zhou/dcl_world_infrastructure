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
file_child_mortality <- 
  here::here(str_glue("c01-own/data/child_mortality.rds"))
# Output file
file_out <- 
  here::here(
    str_glue("c01-own/data/infrastructure_child_mortality.rds")
  )

#===============================================================================

read_rds(file_infrastructure) %>%
  left_join(
    read_rds(file_child_mortality),
    by = c("iso2c", "country", "year")
  ) %>%
  write_rds(file_out)
