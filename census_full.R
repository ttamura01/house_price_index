R.version.string
Sys.info()[["machine"]]

options(pkgType = "binary")

install.packages(c("e1071", "units", "sf"), dependencies = TRUE)


install.packages("tidycensus")
install.packages(c("tidycensus", "tidyverse"))
library(tidycensus)
library(tidyverse)

# 1) Set your Census API key (get one free from Census)
tidycensus::census_api_key("5e2eaead41e7b67352def4e07983a19d5d58f417.", install = TRUE)
# readRenviron("~/.Renviron")  # restart R after install=TRUE

# 2) ACS variable IDs (county-level)
# - Total population: B01003_001
# - Median household income: B19013_001
# - Median value of owner-occupied housing units: B25077_001
# These are ACS Detailed Tables variables. :contentReference[oaicite:1]{index=1}
vars <- c(
  pop        = "B01003_001",
  med_income = "B19013_001",
  med_value  = "B25077_001"
)

# 3) Pull county data (ACS 5-year)
# Try year=2024 first (2020–2024 ACS 5-year).
# If that errors on your machine due to package/API availability, set year=2023.
acs_year <- 2024

df_county_top100 <- get_acs(
  geography = "county",
  variables = vars,
  year      = acs_year,
  survey    = "acs5",
  output    = "wide",
  cache_table = TRUE
) %>%
  transmute(
    GEOID,
    county = NAME,
    population = popE,
    median_household_income = med_incomeE,
    median_home_value = med_valueE,
    # optional: margins of error (MOE)
    population_moe = popM,
    income_moe = med_incomeM,
    home_value_moe = med_valueM
  ) %>%
  arrange(desc(population)) %>%
  slice_head(n = 100)

df_county_top100