# GloNoMo - Global status, trends, drivers and impacts of non-native plant species in mountain ecosystems

# Fieldwork preparation: site selection

# script by Jonathan von Oppen  //  jonathan.vonoppen@unibas.ch
# last updated 09 Feb 2026


# [0] Dependencies ----
pacman::p_load(
  # osmextract,  # vignette at https://docs.ropensci.org/osmextract/articles/osmextract.html
  osmdata,  # vignette at https://github.com/ropensci/osmdata
  sf,
  terra,
  tidyverse
)

data_dir <- file.path("data")
miren_planning_dir <- file.path(data_dir, "fieldwork_planning", "MIREN_sites_SNP+")


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
sf::st_write(umbrail_road, dsn = file.path(miren_planning_dir, "umbrail_road_osm_2056.shp"))

# Ofenpassstrasse
ofenpass_road <- osmdata::opq("Ofenpass") %>% 
  osmdata::add_osm_feature(key = "destination", value = "mountain_pass") %>% 
  osmdata::osmdata_sf() %>% 
  .$osm_multilines %>% 
  sf::st_as_sf() %>% 
  dplyr::filter(osm_id == 19043688) %>% 
  sf::st_transform(crs = "epsg:2056") %>% 
  select_if(~ !any(is.na(.)))
sf::st_write(ofenpass_road, dsn = file.path(miren_planning_dir, "ofenpass_road_osm_2056.shp"))

# Passstrasse Flüela
fluela_road <- osmdata::opq("Flüelapass") %>% 
  osmdata::add_osm_feature(key = "destination", value = "mountain_pass") %>% 
  osmdata::osmdata_sf() %>% 
  .$osm_multilines %>% 
  sf::st_as_sf() %>% 
  dplyr::filter(osm_id == 19043623) %>% 
  sf::st_transform(crs = "epsg:2056") %>% 
  sf::st_intersection(zernez_boundary) %>% 
  .[,1:10] %>%  # exclude fields added from zernez_boundary
  select_if(~ !any(is.na(.)))
sf::st_write(fluela_road
             , dsn = file.path(miren_planning_dir, "flüela_road_osm_2056.shp")
             , layer_options = "SHPT=ARCZ")  # to enable saving 3D MULTILINESTRING: https://stackoverflow.com/q/74315261/17268298


## > swisstopo DEM ----

# downloading function
download_road_dem <- function(target_road){
  download_links <- readr::read_lines(list.files(miren_planning_dir, pattern = paste0(target_road, "_road_ch.swisstopo.swissalti3d.*.csv")
                                                 , full.names = T))
  dem_outfile <- file.path(miren_planning_dir, paste0(target_road, "_road_dem_swissALTI3D_0.5m_2056.tif"))
  if(!file.exists(dem_outfile)){
    # print message
    cat(paste0("Downloading ", stringr::str_to_title(target_road), " road DEM ...\n"))
    
    # load tiles to list
    road_dem <- purrr::map(download_links, terra::rast) %>% 
      # mosaic tiles
      terra::sprc() %>% 
      terra::mosaic()
    
    # save raster
    terra::writeRaster(road_dem
                       , dem_outfile)
    
    # print message
    cat(paste0(stringr::str_to_title(target_road), " road DEM successfully saved.\n"))
    return(paste0(stringr::str_to_title(target_road), " DEM: ", T))
  } else {
    # print message
    cat(paste0(stringr::str_to_title(target_road), " road DEM already present.\n"))
    return(paste0(stringr::str_to_title(target_road), " DEM: ", T))
  }
}

# get DEMs
target_roads <- c("flüela", "ofenpass", "umbrail")
purrr::map_chr(target_roads, download_road_dem)

flüela_dem <- terra::rast(file.path(miren_planning_dir, "flüela_road_dem_swissALTI3D_0.5m_2056.tif"))
ofenpass_dem <- terra::rast(file.path(miren_planning_dir, "ofenpass_road_dem_swissALTI3D_0.5m_2056.tif"))
umbrail_dem <- terra::rast(file.path(miren_planning_dir, "umbrail_road_dem_swissALTI3D_0.5m_2056.tif"))


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