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

## (a) plot data ----

### MIREN ----

#### >> roads ----
miren_road_species <- readr::read_csv(file.path(miren_data_dir, "MIREN_road_survey_(2007-2023)_zen_v3_002507.csv"))

#### >> trails ----
miren_trail_plot_metadata <- readxl::read_xlsx(file.path(miren_data_dir, "MIREN_trail_data_final.xlsx")
                                               , na = c("", "NA"))


### GLORIA ----

# plot metadata
gloria_plot_metadata <- read_gloria_file(file.path(gloria_data_dir, "fld_plot.txt"))

# plot cover data
gloria_plot_cover <- read_gloria_file(file.path(gloria_data_dir, "fld_plot_protocols.txt"))

# additional plot-level data
gloria_plot_data_add <- read_gloria_file(file.path(gloria_data_dir, "fld_plot_additional_infos.txt"))

# species data
gloria_species <- read_gloria_file(file.path(gloria_data_dir, "fld_plot_species.txt")) 

# species reference list
gloria_species_reference <- read_gloria_file(file.path(gloria_data_dir, "lut_species.txt"))

# plot-level topologies
gloria_plot_topologies <- read_gloria_file(file.path(gloria_data_dir, "fld_plot_topologies.txt"))

# summit-level data
gloria_summit_data <- read_gloria_file(file.path(gloria_data_dir, "fld_summit.txt"))


## (b) reference data ----

database_dir <- file.path("P:", "common", "data", "Databases")

### geographical ----
countries <- rnaturalearthhires::countries10 %>% 
  dplyr::select(NAME_EN, geometry) %>% 
  sf::st_make_valid()

### WCVP ----
wcvp_species <- readr::read_delim(file.path(database_dir, "WCVP", "wcvp_names.csv")) %>% 
  tidylog::filter(taxon_status %in% c("Accepted", "Synonym")) %>% 
  dplyr::mutate(taxon_name_long = paste0(taxon_name, " ", taxon_authors))

wcvp_distribution <- readr::read_delim(file.path(database_dir, "WCVP", "wcvp_distribution.csv")
                                       , delim = "|")

### TDWG ----
tdwg_l3 <- sf::read_sf(file.path("P:", "common", "data", "GISData", "ecological_boundaries", "ecological_regions", "TDWG_Geographical_scheme_Global", "level3", "level3.shp")) %>% 
  sf::st_make_valid()

### GloNAF ----


# [2] Prepare data ----

## MIREN ----


## GLORIA ----

### >> locations ----
gnm_gloria_locations <- gloria_plot_metadata %>% 
  tidylog::left_join(gloria_summit_data %>% 
                       dplyr::select(-c(id,
                                        in_glonomo_eu))
                     , by = c("region", "summit")) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = "epsg:4326") %>% 
  sf::st_join(countries) %>% 
  dplyr::rename(summit_altitude = altitude,
                country = NAME_EN) %>% 
  dplyr::relocate(country, .after = "summit_name") %>% 
  # add approximate plot-level elevation
  tidylog::mutate(plot_elevation_approx = dplyr::case_when(
    stringr::str_detect(plot, "[NESW]05") ~ summit_altitude - 2.5,  # 2.5m below summit as middle point to 5-m lower boundary
    stringr::str_detect(plot, "[NESW]10") ~ summit_altitude - 5,  # 5m below summit as middle point to 10-m lower boundary
    stringr::str_detect(plot, "^[NESW13]+$") ~ summit_altitude - 5  # ca 5m below summit (though in practice slightly higher up. See protocol.)
    , TRUE ~ NA_real_
  )) #%>% 
  # add plot centre coordinates
  #TODO

### >> species ----

gnm_gloria_species <- gloria_species %>% 
  # limit species to vascular 
  tidylog::left_join(gloria_species_reference
                     , by = c("species_code" = "id")) %>% 
  tidylog::filter(plant_type == "V") %>% 
  # add summit points
  pipeMessage("Adding summit coordinates ...") %>% 
  tidylog::left_join(gnm_gloria_locations %>% 
                       dplyr::select(region, summit, plot, geometry)
                     , by = c("region", "summit", "plot")) %>% 
  sf::st_as_sf() %>% 
  sf::st_make_valid() %>% 
  # add botanical regions
  pipeMessage("Adding TDWG botanical regions ...") %>% 
  sf::st_join(tdwg_l3 %>% 
                dplyr::select(area_code_l3 = LEVEL3_COD, geometry)) %>% 
  # add WCVP species codes
  pipeMessage("Adding WCVP species codes ...") %>% 
  tidylog::left_join(wcvp_species %>% 
                       dplyr::select(taxon_name_long, plant_name_id)
                     , by = c("species_name" = "taxon_name_long")) %>% # TODO optimise matching - GBIF codes for WCVP?
  # add WCVP nativeness status
  pipeMessage("Adding *introduced* column ...") %>% 
  tidylog::left_join(wcvp_distribution %>% 
                       dplyr::select(plant_name_id, area_code_l3, introduced)
                     , by = c("plant_name_id", "area_code_l3"))

