Infrastructure and Urban Growth
================
Ada Zhou
2020-02-18

  - [Correlation](#correlation)

``` r
# Libraries
library(tidyverse)

# Parameters
file_data <- here::here("c01-own/data/infrastructure_urban_growth.rds")

world_regions <- 
  c(
    "8S",
    "ZG",
    "Z4",
    "ZQ",
    "Z7",
    "ZJ",
    "XU"
  )

group_codes <- 
  c(
    "1A",
    "S3",
    "B8",
    "V2",
    "Z4",
    "4E",
    "T4",
    "XC",
    "Z7",
    "7E",
    "T7",
    "EU",
    "F1",
    "XE",
    "XD",
    "XF",
    "ZT",
    "XH",
    "XI",
    "XG",
    "V3",
    "ZJ",
    "XJ",
    "T2",
    "XL",
    "XO",
    "XM",
    "XN",
    "ZQ",
    "XQ",
    "T3",
    "XP", #MIDDLE INCOME
    "XU",
    "XY",
    "OE",
    "S4",
    "S2",
    "V4",
    "V1",
    "S1",
    "8S",
    "T5",
    "ZG",
    "ZF",
    "T6",
    "XT", #UPPER MIDDLE INCOME
    "1W"
  )

#===============================================================================

df <- read_rds(file_data)
```

### Correlation

``` r
df %>% 
  filter(
    !(iso2c %in% group_codes),
    year == "2017"
  ) %>% 
  select(-contains("rural")) %>% 
  pivot_longer(
    cols = contains("_urban"),
    names_to = "infrastructure",
    values_to = "infrastructure_pop_percent"
  ) %>% 
  drop_na(urban_growth, infrastructure_pop_percent) %>% 
  ggplot(
    aes(urban_growth, infrastructure_pop_percent, color = infrastructure)
  ) +
  geom_point(alpha = 0.5) +
  facet_grid(cols = vars(infrastructure))
```

![](infrastructure_urban_growth_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
df %>% 
  filter(
    !(iso2c %in% group_codes),
    year == "2017"
  ) %>% 
  select(-contains("_urban")) %>% 
  pivot_longer(
    cols = contains("rural"),
    names_to = "infrastructure",
    values_to = "infrastructure_pop_percent"
  ) %>% 
  drop_na(urban_growth, infrastructure_pop_percent) %>% 
  ggplot(
    aes(urban_growth, infrastructure_pop_percent, color = infrastructure)
  ) +
  geom_point(alpha = 0.5) +
  facet_grid(cols = vars(infrastructure))
```

![](infrastructure_urban_growth_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->
