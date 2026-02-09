# GloNoMo - Global status, trends, drivers and impacts of non-native plant species in mountain ecosystems

# Fieldwork preparation: site selection

# script by Jonathan von Oppen  //  jonathan.vonoppen@unibas.ch
# last updated 09 Feb 2026


# [0] Dependencies ----
pacman::p_load(
  # osmextract,  # vignette at https://docs.ropensci.org/osmextract/articles/osmextract.html
  osmdata,  # vignette at https://github.com/ropensci/osmdatasf,
  terra,
  tidyverse
)

data_dir <- file.path("data")
miren_planning_dir <- file.path(data_dir, "fieldwork_planning", "MIREN_sites_SNP+")


# [1] Load data ----
  # boundaries & road data from OpenStreetMap

## > Graubünden boundary ----


## > roads ----

# Passstrasse Stelvio

# Ofenpassstrasse

# Passstrasse Flüela

fluela_road <- osmdata::opq("Flüelapass") %>% 
  osmdata::add_osm_feature(key = "destination", value = "mountain_pass") %>% 
  osmdata::osmdata_sf() %>% 
  .$osm_multilines %>% 
  dplyr::filter(osm_id == 19043623)

## > swisstopo DEM ----


## > plot target elevations ----
plot_elevs <- readxl::read_excel(file.path(miren_planning_dir, "Site_setup.xlsx")) %>% 
  dplyr::rename("plot_id" = 1) %>% 
  tidyr::pivot_longer(cols = -plot_id
                      , names_to = "road"
                      , values_to = "elevation") %>% 
  dplyr::arrange(road)


# [2] Derive potential sampling zones/sites ----

getMIRENsites <- function(line_geometry  # road/trail
                          , plot_dimensions
                          , buffer_width  # width of buffer zone
){
  # read 
}
# 