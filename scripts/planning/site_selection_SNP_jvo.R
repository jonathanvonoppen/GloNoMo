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

getMIRENsites <- function(target_road  # road/trail
                          , plot_dimensions
                          , buffer_width  # width of buffer zone
){
  target_road <- "flüela" # test
  
  ## > Determine target zones ----
  # load geometry
  road_geom <- sf::read_sf(file.path(miren_planning_dir, paste0(target_road, "_road_osm_2056.shp")))
  
  # add buffer to geometry
  road_geom_buffer <- road_geom %>% 
    sf::st_buffer(dist = 105
                  , endCapStyle = "ROUND")
  
  ## > Load DEM ----
  # load DEM
  road_dem <- terra::rast(file.path(miren_planning_dir, paste0(target_road, "_road_dem_swissALTI3D_0.5m_2056.tif"))) %>% 
    setNames("elevation_m")
  
  # check that DEM has data for all of road buffer
  NAs_in_road_buffer <- road_dem %>% 
    terra::extract(road_geom_buffer %>% terra::vect()
                   , touches = TRUE
                   , ID = FALSE) %>% 
    dplyr::pull(1) %>% 
    anyNA()
  if(NAs_in_road_buffer) {
    stop(paste0("DEM tiles not yet covering the whole potential plot area along the "
                , stringr::str_to_title(target_road)
                , " pass road. Download additional tiles."))
  }

  
  ## > Limit road vector to ideal site elevation ----
  # set elevation distance threshold for sites
  site_elev_dist_threshold <- 5
  
  site_elev <- readxl::read_excel(file.path(miren_planning_dir, "Site_setup.xlsx")) %>% 
    dplyr::rename(plot_id = 1) %>% 
    dplyr::select(plot_id, 
                  dplyr::contains(target_road, ignore.case = TRUE)) %>% 
    dplyr::rename("elevation_m" = 2) %>% 
    # filter out summary stats at the bottom
    dplyr::filter(stringr::str_detect(plot_id, "Plot [0-9]")) %>% 
    # create lower- and higher-elevation threshold columns for each plot group
    dplyr::mutate(elev_threshold_low = elevation_m - site_elev_dist_threshold,
                  elev_threshold_high = elevation_m + site_elev_dist_threshold) 
  
  # # transform into classification matrix for masking
  # site_elev_dist_threshold <- 5
  # site_elev_zones <- matrix(site_elev$elevation_m - site_elev_dist_threshold,
  #                           site_elev$elevation_m + site_elev_dist_threshold,
  #                           rep(1, nrow(site_elev))
  #                           , ncol = 3
  #                           , byrow = FALSE) %>% 
  #   rbind(matrix(c())) # add masked elevation zones?
  
  # create high-res segmentised version of road line
  road_geom_highres <- road_geom %>% 
    sf::st_segmentize(dfMaxLength = 1)
  
  # get road as vertices
  road_vertices <- road_geom_highres %>% 
    sf::st_cast("POINT")
  
  # get elevation along road
  road_elev <- road_dem_masked %>% 
    terra::extract(road_vertices, ID = FALSE)
  
  # add as road_geom attribute  
  road_vertices$elevation_m <- road_elev$elevation_m
  
  # filter road_geom for eligible elevation intervals 
  road_vertices_zones_strict <- road_vertices %>% 
    dplyr::filter(
      purrr::map_lgl(elevation_m, ~ any(
        .x >= site_elev$elevation_m - site_elev_dist_threshold & 
          .x <= site_elev$elevation_m + site_elev_dist_threshold
      ))
    ) %>% 
    dplyr::mutate(id = 1:nrow(.))
  
  
  ## > Create perpendicular lines at vertices ----
  create_perpendicular_plots <- function(line_geom, point_geom, length, width) {
    # Get coordinates
    line_coords <- sf::st_coordinates(line_geom)
    point_coord <- sf::st_coordinates(point_geom)
    
    # Find the two nearest vertices on the line to get local direction
    dists <- sqrt((line_coords[, "X"] - point_coord[1])^2 + 
                    (line_coords[, "Y"] - point_coord[2])^2)
    nearest_idx <- which.min(dists)
    
    # Get direction vector (use adjacent points)
    if (nearest_idx == 1) {
      dx <- line_coords[2, "X"] - line_coords[1, "X"]
      dy <- line_coords[2, "Y"] - line_coords[1, "Y"]
    } else if (nearest_idx == nrow(line_coords)) {
      dx <- line_coords[nearest_idx, "X"] - line_coords[nearest_idx - 1, "X"]
      dy <- line_coords[nearest_idx, "Y"] - line_coords[nearest_idx - 1, "Y"]
    } else {
      # Average direction from both sides
      dx <- line_coords[nearest_idx + 1, "X"] - line_coords[nearest_idx - 1, "X"]
      dy <- line_coords[nearest_idx + 1, "Y"] - line_coords[nearest_idx - 1, "Y"]
    }
    
    # Normalize
    mag <- sqrt(dx^2 + dy^2)
    dx <- dx / mag
    dy <- dy / mag
    
    # Perpendicular vector (rotate 90 degrees)
    perp_dx <- -dy
    perp_dy <- dx
    
    # Create LEFT perpendicular line (one direction only)
    left_end <- c(point_coord[1] + perp_dx * length, 
                  point_coord[2] + perp_dy * length)
    left_line <- sf::st_linestring(rbind(point_coord, left_end))
    left_plot <- left_line %>% 
      sf::st_buffer(dist = width/2
                    , endCapStyle = "FLAT")
    
    # Create RIGHT perpendicular line (opposite direction)
    right_end <- c(point_coord[1] - perp_dx * length, 
                   point_coord[2] - perp_dy * length)
    right_line <- sf::st_linestring(rbind(point_coord, right_end))
    right_plot <- right_line %>% 
      sf::st_buffer(dist = width/2
                    , endCapStyle = "FLAT")
    
    perp_plots_lr <- list(left = left_plot, right = right_plot)
    return(perp_plots_lr)
  }
  
  # Create perpendicular lines
  perpendicular_plots_lr <- purrr::map(road_vertices_zones_strict$geometry, ~ {
    create_perpendicular_plots(road_geom_highres$geometry[[1]], .x, length = 105, width = 2)
  })
  
  # Create separate sf objects for left and right
  perp_left_sf <- sf::st_sf(
    id = road_vertices_zones_strict$id,
    side = "left",
    geometry = sf::st_sfc(purrr::map(perpendicular_plots_lr, "left"), crs = sf::st_crs(road_geom_highres))
  )
  
  perp_right_sf <- sf::st_sf(
    id = road_vertices_zones_strict$id,
    side = "right",
    geometry = sf::st_sfc(purrr::map(perpendicular_plots_lr, "right"), crs = sf::st_crs(road_geom_highres))
  )
  
  # Or combine into one sf object with a side column
  perp_both_sf <- bind_rows(perp_left_sf, perp_right_sf)
  
  
  ## > Mask DEM (slope, road) ----
  
  # create buffer version of 
  
  # calculate steep slope mask
  slope_threshold_value <- 35
  slope_masking_thresh <- matrix(c(-Inf, slope_threshold_value, 1,
                                   slope_threshold_value, Inf, NA)
                                 , ncol = 3
                                 , byrow = T)
  
  # create mask
  road_dem_slopemask <- road_dem %>% 
    terra::terrain(v = "slope"
                   , unit = "degrees") %>% 
    terra::classify(slope_masking_thresh)
  
  # mask DEM
  road_dem_masked <- road_dem %>% 
    terra::mask(road_dem_slopemask)
  
  
  ## > Filter vertices ----
  
  ### ~ no steep areas within perpendicular plot ----
  
  ### ~ perpendicular plot not overlapping with road line (in road turns) ----
  
  # if several remain within group, pick closest to ideal elevation
  
  # check if all groups have left and right available
  
  ## if yes, pick random order
  
  ## if no, ??
  
  ## pick alternative plots within groups
  
}
# 