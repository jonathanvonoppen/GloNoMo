# GloNoMo - Global status, trends, drivers and impacts of non-native plant species in mountain ecosystems

# Data integration 

# script by Jonathan von Oppen  //  jonathan.vonoppen@unibas.ch
# last updated 10 Mar 2026


# [0] Dependencies ----
pacman::p_load(
  tidyverse
)

# Ecology group functions
source(file.path("P:", "common", "scripts", "EcologyGroup_functions_library.R"))

# specific read function to account for special characters, decimal , and . grouping mark in GLORIA files:
read_gloria_file <- function(file) {
  readr::read_delim(file = file,
                    , delim = ";"
                    , locale = locale(decimal_mark = ",",
                                      grouping_mark = ".",
                                      encoding = "latin1"
                    ))
}

# Directories
data_dir <- file.path("data")
miren_data_dir <- file.path(data_dir, "external", "MIREN")
gloria_data_dir <- file.path(data_dir, "external", "GLORIA")


# [1] Load data ----

## MIREN ----

### >> roads ----
miren_road_species <- readr::read_csv(file.path(miren_data_dir, "MIREN_road_survey_(2007-2023)_zen_v3_002507.csv"))

### >> trails ----
miren_trail_plot_metadata <- readxl::read_xlsx(file.path(miren_data_dir, "MIREN_trail_data_final.xlsx")
                                               , na = c("", "NA"))


## GLORIA ----

# plot metadata
gloria_plot_metadata <- read_gloria_file(file.path(gloria_data_dir, "fld_plot.txt"))

# plot cover data
gloria_plot_cover <- read_gloria_file(file.path(gloria_data_dir, "fld_plot_protocols.txt"))

# additional plot-level data
gloria_plot_data_add <- read_gloria_file(file.path(gloria_data_dir, "fld_plot_additional_infos.txt"))

# species data
gloria_species <- read_gloria_file(file.path(gloria_data_dir, "fld_plot_species.txt")) 

# species reference list
gloria_species_reference <- read_gloria_file(file.path(gloria_data_dir, "lut_species.txt")
                                              , delim = ";")

# plot-level topologies
gloria_plot_topologies <- read_gloria_file(file.path(gloria_data_dir, "fld_plot_topologies.txt"))

# summit-level data
gloria_summit_data <- read_gloria_file(file.path(gloria_data_dir, "fld_summit.txt")
                                        , delim = ";")
