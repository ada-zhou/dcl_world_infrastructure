Infrastructure Development
================
Your Name
2019-

  - [Introduction](#introduction)
  - [WDI](#wdi)
  - [Gather and tidy data](#gather-and-tidy-data)
  - [Initial, general overview](#initial-general-overview)

``` r
# Libraries
library(tidyverse)
library(WDI)
library(dcl)

# Parameters

  # year min and max for decade change analysis
YEAR_MIN <- 2007
YEAR_MAX <- 2017

  # file of data
file_data <- here::here("")

  # ISOC2 codes of the world broken up into 7 regions
  # East Asia & Pacific, Europe & Central Asia, Latin America & Caribbean, Middle East & North Africa, North America, South Asia, Sub-Saharan Africa
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

  # ISOC2 codes of all grouped regions of the world
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
    "XP",
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
    "XT",
    "1W"
  )

  # File for downloaded answers
file_answers <- "~/answers.rds"

#===============================================================================

# Read in answers
if (str_length(file_answers) > 0) {
  answers <- read_rds(file_answers) 
}
```

### Introduction

Currently the World Bank estimates that over 55% of the worlds 4+
million population live in urban areas. Additionally more than 80% of
global GDP is generated in cities. But now are people’s quality of lives
as urbanization increases?

The World Bank estimates that around 90% of urban expansion in
developing countries are being built through informal and unplanned
settlements. In this challenge, you will look at data on urban and rural
population access to three main infrastructures: electricity, water and
sanitation. What are some stories of successful urbanization or
successful rural life in terms of accessibility to basic infrastructure
for populations? What are some stories of struggle?

### WDI

You will use the data from the World Development Indicators Dataset. The
“World Development Indicators (WDI) is the primary World Bank collection
of development indicators, compiled from officially recognized
international sources. It presents the most current and accurate global
development data available, and includes national, regional and global
estimates.”

You can access WDI using the WDI API which is loaded for you above.
Documentation for using the WDI API can be found
[here](https://cran.r-project.org/web/packages/WDI/WDI.pdf).

### Gather and tidy data

**q1** Pull indicators for access to electricity (% of urban
population), access to electricity (% of rural population), access to
water (% of urban population), access to water (% of rural population),
access to sanitation (% of urban population) and access to sanitation (%
of rural population) from all available years and all available
countries using a WDI API call. You can look for indicator codes using
`WDIsearch` which is also covered in the WDI API documentation
[above](https://cran.r-project.org/web/packages/WDI/WDI.pdf).

If you are having trouble with reading in the indicators with the WDI,
there is a `infrastructure.rds` file in the \[box\]
(<https://stanford.app.box.com/folder/104836386745>) is what it should
be.

Save the data into a tibble `q1`.

``` r
# Compare result with answer
if (exists("q1")) compare(answers$q1, q1)
```

**q2** Tidy and clean data so that there is a column for
`infrastructure`, `u_r` (urban or rural) and `population_percent`. Save
your cleaned data in a tibble `q2`.

``` r
# Compare result with answer
if (exists("q2")) compare(answers$q2, q2)
```

### Initial, general overview

**q3** Graph the access to electricity, water and sanitation in urban
and rural areas for every world region using the vector `world_regions`
which contains all iso2c codes of the 7 world regions used by the World
Bank. What information does the graphs tell you about the state of
infrastructure access? Which infrastructure looks to be lagging behind?

``` r
# Print results
if (exists("q3")) q3
```

#### Top Ten and Bottom Ten

Now let’s take a closer look at the top ten most improved and the bottom
ten least improved infrastructure situations in the decade between
2007-2017 for each infrastructure type.

**q4.1** First write a function named `decade_top` that takes in three
variables - `infrastructure`, `u_r` and `n` - that will return the top
`n` countries in terms of population percent access change between
2007-2017 for the infrastructure and population type. Resulting tibble
should have `iso2c` and `decade_change` as columns.

Find the top ten decade change in infrastructure situations for
electricity urban, electricity rural, water urban, water rural,
sanitation urban, and sanitation rural and bottom ten decade change in
infrastructure situations for electricity urban, electricity rural,
water urban, water rural, sanitation urban, and sanitation rural (code
provided) with your function.

Answers for what each resulting tibble should be are provided for you to
use to compare if desired.

Hints: Use the vector `group_codes` to select only country-level data.
Filter out any decade changes that are equal to zero.

``` r
# Use this to help you compare
if (exists("decade_top")) {
  q4_sanitation_rural_top <- decade_top("sanitation", "rural", 10)
  q4_sanitation_urban_top <- decade_top("sanitation", "urban", 10)
  q4_water_rural_top <- decade_top("water", "rural", 10)
  q4_water_urban_top <- decade_top("water", "urban", 10)
  q4_electricity_rural_top <- decade_top("electricity", "rural", 10)
  q4_electricity_urban_top <- decade_top("electricity", "urban", 10)
  
  q4_sanitation_rural_bottom <- decade_top("sanitation", "rural", -10)
  q4_sanitation_urban_bottom <- decade_top("sanitation", "urban", -10)
  q4_water_rural_bottom <- decade_top("water", "rural", -10)
  q4_water_urban_bottom <- decade_top("water", "urban", -10)
  q4_electricity_rural_bottom <- decade_top("electricity", "rural", -10)
  q4_electricity_urban_bottom <- decade_top("electricity", "urban", -10)
}


# Compare result with answer
if (exists("q4_electricity_urban_bottom")) 
  compare(answers$q4_electricity_urban_bottom, q4_electricity_urban_bottom)

if (exists("q4_sanitation_rural_top")) 
  compare(answers$q4_sanitation_rural_top, q4_sanitation_rural_top)
```

**q4.2** Write a function to visualize each country and its change in
population access percent between 2007-2017. Function should take in at
least these three variables - `infrastructure`, `u_r` and `n`.

Use the function to create 12 visualizations: top ten decade change in
infrastructure situations for electricity urban, electricity rural,
water urban, water rural, sanitation urban, and sanitation rural and
bottom ten decade change in infrastructure situations for electricity
urban, electricity rural, water urban, water rural, sanitation urban,
and sanitation rural (code provided).

Hints: Use function written in `q4.1` in this function. E09 Viz

``` r
if (exists("plot_decade_change")) {
  # Run these to plot
  plot_decade_change("sanitation", "rural", 10)
  plot_decade_change("sanitation", "urban", 10)
  plot_decade_change("water", "rural", 10)
  plot_decade_change("water", "urban", 10)
  plot_decade_change("electricity", "rural", 10)
  plot_decade_change("electricity", "urban", 10)
  
  plot_decade_change("sanitation", "rural", -10)
  plot_decade_change("sanitation", "urban", -10)
  plot_decade_change("water", "rural", -10)
  plot_decade_change("water", "urban", -10)
  plot_decade_change("electricity", "rural", -10)
  plot_decade_change("electricity", "urban", -10)
}
```

#### Focusing on individual country data

**q5.1** Write a function to graph all the data (urban and rural
population, electricity, sanitation and water infrastructures) across
all available years on an individual country with `country` as a
parameter.

``` r
graph_country <- function(country) {
  
}
```

**q5.2** Use the function from `q5.1` to graph each countries data in
the bottom 10 of change in sanitation access for urban populations
between 2007-2017.

What do you notice about each country? Does the decrease in santiation
access mean anything for electricity or water access?

Hint: Use the function from q4.1 and q2
