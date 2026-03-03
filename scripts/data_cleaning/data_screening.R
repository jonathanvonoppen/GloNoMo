# GloNoMo - Global status, trends, drivers and impacts of non-native plant species in mountain ecosystems

# Data exploration script

# script by Jonathan von Oppen  //  jonathan.vonoppen@unibas.ch
# last updated 13 Mar 2026


# [0] Dependencies ----
pacman::p_load(
  tidyverse
)

# Ecology group functions
source(file.path("P:", "common", "scripts", "EcologyGroup_functions_library.R"))

data_dir <- file.path("data")
miren_data_dir <- file.path(data_dir, "external", "MIREN")


# [1] Load data ----

## >> roads ----
miren_roads <- readr::read_csv(file.path(miren_data_dir, "MIREN_road_survey_(2007-2023)_zen_v3_002507.csv"))

# n unique regions
miren_roads %>% pull(Region) %>% unique() %>% length()
#> 26

# n unique roads
miren_roads %>% pull(Road) %>% unique() %>% length()
#> 75

## >> trails ----
miren_trails <- readxl::read_xlsx(file.path(miren_data_dir, "MIREN_trail_data_final.xlsx")
                                  , na = c("", "NA"))

# n unique regions
miren_trails %>% pull(Region) %>% unique() %>% length()
#> 10

# n unique roads
miren_trails %>% pull(Trail) %>% unique() %>% length()
#> 53