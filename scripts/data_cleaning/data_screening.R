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

## MIREN ----

### >> roads ----
miren_road_species <- readr::read_csv(file.path(miren_data_dir, "MIREN_road_survey_(2007-2023)_zen_v3_002507.csv"))

    # n unique regions
    miren_road_species %>% pull(Region) %>% unique() %>% length()  #> 26
    
    # n unique roads
    miren_road_species %>% pull(Road) %>% unique() %>% length()  #> 75

### >> trails ----
miren_trail_plot_metadata <- readxl::read_xlsx(file.path(miren_data_dir, "MIREN_trail_data_final.xlsx")
                                  , na = c("", "NA"))

    # n unique regions
    miren_trail_plot_metadata %>% pull(Region) %>% unique() %>% length()  #> 10
    
    # n unique roads
    miren_trail_plot_metadata %>% pull(Trail) %>% unique() %>% length()  #> 53


## GLORIA ----

gloria_data_dir <- file.path(data_dir, "external", "GLORIA")

# plot metadata
gloria_plot_metadata <- readr::read_delim(file.path(gloria_data_dir, "fld_plot.txt"))

    # n unique regions
    gloria_plot_metadata %>% pull(region) %>% unique() %>% length()  #> 27
    # n unique plots
    gloria_plot_metadata %>% tidyr::unite(col = "plot_unique", region, summit, plot) %>% pull(plot_unique) %>% unique() %>% length()  #> 2600

# plot cover data
gloria_plot_cover <- readr::read_delim(file.path(gloria_data_dir, "fld_plot_protocols.txt"))

# additional plot-level data
gloria_plot_data_add <- readr::read_delim(file.path(gloria_data_dir, "fld_plot_additional_infos.txt"))

# species data
gloria_species <- readr::read_delim(file.path(gloria_data_dir, "fld_plot_species.txt"))  
  #> species coded numerically; species_code corresponds to column $id in gloria_species_reference (file *lut_species.txt*)

# species reference list
gloria_species_reference <- readr::read_delim(file.path(gloria_data_dir, "lut_species.txt")
                                              , delim = ";")

# plot-level topologies
gloria_plot_topologies <- readr::read_delim(file.path(gloria_data_dir, "fld_plot_topologies.txt"))

# summit-level data
gloria_summit_data <- readr::read_delim(file.path(gloria_data_dir, "fld_summit.txt")
                                        , delim = ";")
