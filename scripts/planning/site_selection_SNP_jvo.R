# GloNoMo - Global status, trends, drivers and impacts of non-native plant species in mountain ecosystems

# Fieldwork preparation: site selection

# script by Jonathan von Oppen  //  jonathan.vonoppen@unibas.ch
# last updated 05 Mar 2026


# [0] Dependencies ----
pacman::p_load(
  # osmextract,  # vignette at https://docs.ropensci.org/osmextract/articles/osmextract.html
  osmdata,  # vignette at https://github.com/ropensci/osmdata
  sf,
  terra,
  tidyverse
)

# specific functions
source(file.path("scripts", "planning", "MIREN_site_selection_functions.R"))

# Ecology group functions
source(file.path("P:", "common", "scripts", "EcologyGroup_functions_library.R"))

data_dir <- file.path("data")
miren_planning_dir <- file.path(data_dir, "fieldwork_planning", "MIREN_sites_SNP+")
input_data_dir <- file.path(miren_planning_dir, "input_data")


# [1] Load data ----
  # boundaries & road data from OpenStreetMap 
  # (OSM roads more precisely matching SwissTopo Landeskarte than swissTLMregio roads)

## > Communal boundaries ----
tlm_regio_boundaries_dir <- "P:/common/data/GISData/admin/admin_areas/switzerland/swissTLMRegio_2025_boundaries_LV95"
snpp_boundaries <- sf::read_sf(file.path(tlm_regio_boundaries_dir, "swissTLMRegio_HOHEITSGEBIET_LV95.shp")) %>% 
  dplyr::filter(stringr::str_detect(NAME, "Zernez|Val Müstair")) %>% 
  sf::st_transform(crs = "epsg:2056")
  
zernez_boundary <- snpp_boundaries %>% 
  dplyr::filter(NAME == "Zernez")

müstair_boundary <- snpp_boundaries %>% 
  dplyr::filter(NAME == "Val Müstair")

## > roads ----

# tlm_regio_transportation_dir <- "P:/common/data/GISData/geographic_features/Switzerland/swissTLMRegio_2025_Product_LV95/Transportation"
# snpp_roads <- sf::st_read(file.path(tlm_regio_transportation_dir, "swissTLMRegio_Road.shp")) %>% 
#   sf::st_filter(snpp_boundaries)
# #> Check in QGIS: not accurate!! Go with OSM:

# Passstrasse Stelvio
umbrail_road <- osmdata::opq("Umbrail-Passstrasse") %>% 
  osmdata::add_osm_feature(key = "type", value = "route") %>% 
  osmdata::osmdata_sf() %>% 
  .$osm_multilines %>% 
  sf::st_as_sf() %>% 
  dplyr::filter(osm_id == 2726972) %>% 
  sf::st_transform(crs = "epsg:2056") %>% 
  select_if(~ !any(is.na(.)))
stelvio_road <- osmdata::opq(sf::st_bbox(c(xmin = 10.432849, ymin = 46.528605, xmax = 10.453202, ymax = 46.539934), crs = "epsg:4326")) %>% 
  osmdata::add_osm_feature(key = "highway", value = "secondary") %>% 
  osmdata::osmdata_sf() %>% 
  .$osm_lines %>% 
  sf::st_as_sf() %>% 
  dplyr::filter(osm_id %in% c(1429965884, 238798011, 238798015, 434566256, 172951992, 73261783, 184815465, 537938655, 406043336)) %>% 
  sf::st_union() %>% 
  sf::st_transform(crs = "epsg:2056")
  # join
umbrail_stelvio_road_complete <- sf::st_union(umbrail_road,
                                              stelvio_road)
sf::st_write(umbrail_stelvio_road_complete, dsn = file.path(input_data_dir, "umbrail_stelvio_road_osm_2056.shp"))

# Ofenpassstrasse
ofenpass_road <- osmdata::opq("Ofenpass") %>% 
  osmdata::add_osm_feature(key = "destination", value = "mountain_pass") %>% 
  osmdata::osmdata_sf() %>% 
  .$osm_multilines %>% 
  sf::st_as_sf() %>% 
  dplyr::filter(osm_id == 19043688) %>% 
  sf::st_transform(crs = "epsg:2056") %>% 
  select_if(~ !any(is.na(.)))
zernez_road <- osmdata::opq("Scheschna") %>% 
  osmdata::add_osm_feature(key = "highway", value = "primary") %>% 
  osmdata::osmdata_sf() %>% 
  .$osm_lines %>% 
  sf::st_as_sf() %>% 
  dplyr::filter(osm_id %in% c(1305648772, 505666332, 38028750, 1277655924, 29011648, 695619048)) %>% 
  sf::st_union() %>% 
  sf::st_transform(crs = "epsg:2056")
  # join
ofenpass_road_complete <- sf::st_union(ofenpass_road,
                                       zernez_road) 
  # cut off Val Müstair part beyond pass longitude
ofenpass_coord_x <- c(10.2921855, 46.6397744) %>% 
  sf::st_point() %>% 
  sf::st_sfc(crs = "epsg:4326") %>% 
  sf::st_transform("epsg:2056") %>% 
  sf::st_coordinates() %>% 
  .[,1]
ofenpass_road_bbox <- sf::st_bbox(ofenpass_road_complete)
ofenpass_road_bbox["xmax"] <- ofenpass_coord_x
ofenpass_road_complete_ascent <- ofenpass_road_complete %>% 
  sf::st_crop(ofenpass_road_bbox)
sf::st_write(ofenpass_road_complete_ascent, dsn = file.path(input_data_dir, "ofenpass_road_osm_2056.shp"))

# Passstrasse Flüela
# Eastern side (Engadin)
flüela_e_road <- osmdata::opq("Flüelapass") %>% 
  osmdata::add_osm_feature(key = "destination", value = "mountain_pass") %>% 
  osmdata::osmdata_sf() %>% 
  .$osm_multilines %>% 
  sf::st_as_sf() %>% 
  dplyr::filter(osm_id == 19043623) %>% 
  sf::st_transform(crs = "epsg:2056") %>% 
  sf::st_intersection(zernez_boundary) %>% 
  .[,1:10] %>%  # exclude fields added from zernez_boundary
  select_if(~ !any(is.na(.)))
sf::st_write(flüela_e_road
             , dsn = file.path(input_data_dir, "flüela_e_road_osm_2056.shp")
             , layer_options = "SHPT=ARCZ")  # to enable saving 3D MULTILINESTRING: https://stackoverflow.com/q/74315261/17268298
# Western side (Davos)
flüela_w_road <- osmdata::opq("Flüelapass") %>% 
  osmdata::add_osm_feature(key = "destination", value = "mountain_pass") %>% 
  osmdata::osmdata_sf() %>% 
  .$osm_multilines %>% 
  sf::st_as_sf() %>% 
  dplyr::filter(osm_id == 19043623) %>% 
  sf::st_transform(crs = "epsg:2056") %>% 
  sf::st_difference(zernez_boundary %>% sf::st_buffer(-150)) %>%  # Zernez communal boundary crosses road some 150 m before the pass
  .[,1:10] %>%  # exclude fields added from zernez_boundary
  select_if(~ !any(is.na(.)))
sf::st_write(flüela_w_road
             , dsn = file.path(input_data_dir, "flüela_w_road_osm_2056.shp")
             , layer_options = "SHPT=ARCZ")  # to enable saving 3D MULTILINESTRING: https://stackoverflow.com/q/74315261/17268298


## > swisstopo DEM ----

# get DEMs
target_roads <- c("flüela_w", "ofenpass", "umbrail_stelvio")
purrr::map_chr(target_roads, download_road_dem)

# flüela_w_dem <- terra::rast(file.path(input_data_dir, "flüela_w_road_dem_swissALTI3D_0.5m_2056.tif"))
# ofenpass_dem <- terra::rast(file.path(input_data_dir, "ofenpass_road_dem_swissALTI3D_0.5m_2056.tif"))
# umbrail_dem <- terra::rast(file.path(input_data_dir, "umbrail_stelvio_road_dem_swissALTI3D_0.5m_2056.tif"))


# [2] Run function for multiple roads/tracks ----
glonomo_sites_snpp <- purrr::map(c(
  "Flüela_W"
  # , "Ofenpass"
  # , "Umbrail_Stelvio"
), ~getMIRENsites(target_road = .x, 
                  plot_width = 2, plot_length = 105, 
                  resolution = 1, 
                  slope_mask_res = 4, 
                  slope_threshold_value = 30,
                  site_elev_dist_threshold = 10, 
                  input_data_dir = file.path(miren_planning_dir, "input_data"),
                  site_elevations_file = "Site_setup.xlsx", 
                  spatial_objects_filter = c("slope", "road", "water", "infrastructure"),
                  output_dir = file.path(miren_planning_dir, "output_target_sites", .x),
                  write_output = c("points", "polygons", "hulls")
)
)
