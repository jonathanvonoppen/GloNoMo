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
flüela_road <- osmdata::opq("Flüelapass") %>% 
  osmdata::add_osm_feature(key = "destination", value = "mountain_pass") %>% 
  osmdata::osmdata_sf() %>% 
  .$osm_multilines %>% 
  sf::st_as_sf() %>% 
  dplyr::filter(osm_id == 19043623) %>% 
  sf::st_transform(crs = "epsg:2056") %>% 
  sf::st_intersection(zernez_boundary) %>% 
  .[,1:10] %>%  # exclude fields added from zernez_boundary
  select_if(~ !any(is.na(.)))
sf::st_write(flüela_road
             , dsn = file.path(input_data_dir, "flüela_road_osm_2056.shp")
             , layer_options = "SHPT=ARCZ")  # to enable saving 3D MULTILINESTRING: https://stackoverflow.com/q/74315261/17268298


## > swisstopo DEM ----

# downloading function
download_road_dem <- function(target_road, 
                              dem_dir = input_data_dir){
  # download links obtained from https://www.swisstopo.admin.ch/en/height-model-swissalti3d#swissALTI3D---Download
  download_links <- readr::read_lines(list.files(dem_dir, pattern = paste0(target_road, "_road_ch.swisstopo.swissalti3d.*.csv")
                                                 , full.names = T))
  dem_outfile <- file.path(dem_dir, paste0(target_road, "_road_dem_swissALTI3D_0.5m_2056.tif"))
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
target_roads <- c("flüela", "ofenpass", "umbrail_stelvio")
purrr::map_chr(target_roads, download_road_dem)

flüela_dem <- terra::rast(file.path(miren_planning_dir, "flüela_road_dem_swissALTI3D_0.5m_2056.tif"))
ofenpass_dem <- terra::rast(file.path(miren_planning_dir, "ofenpass_road_dem_swissALTI3D_0.5m_2056.tif"))
umbrail_dem <- terra::rast(file.path(miren_planning_dir, "umbrail_stelvio_road_dem_swissALTI3D_0.5m_2056.tif"))


# ## > plot target elevations ----
# plot_elevs <- readxl::read_excel(file.path(miren_planning_dir, "Site_setup.xlsx")) %>% 
#   dplyr::rename("plot_id" = 1) %>% 
#   tidyr::pivot_longer(cols = -plot_id
#                       , names_to = "road"
#                       , values_to = "elevation") %>% 
#   dplyr::mutate(road = dplyr::recode_values(road,
#                                             "Ofenpass" ~ "ofenpass",
#                                             "Stilfserjoch" ~ "umbrail",
#                                             "Flüelapass" ~ "flüela")) %>% 
#   dplyr::arrange(road)


# [2] Derive potential sampling zones/sites ----

getMIRENsites <- function(target_road  # road/trail
                          , plot_width
                          , plot_length  # width of buffer zone
                          , resolution  # resolution along road in m
                          , slope_mask_res  # resolution of slope mask raster
                          , slope_threshold_value  # threshold value (degrees) for masking steep slopes
                          , spatial_objects_filter = c("slope", "road", "water", "infrastructure")  # which spatial filter to apply for candidate plots
                          , site_elev_dist_threshold  # set elevation distance threshold from ideal elevation for sites
                          , input_data_dir
                          , site_elevations_file  # file with ideal elevations per site
                          , output_dir
                          , write_output = TRUE
){
  # target_road <- "flüela" # test
  # plot_width = 2
  # plot_length = 105
  # resolution = 1
  # slope_mask_res = 4
  # slope_threshold_value = 35
  # site_elev_dist_threshold = 10
  # input_data_dir = file.path(miren_planning_dir, "input_data")
  # site_elevations_file = "Site_setup.xlsx"
  # spatial_objects_filter = c("slope", "road", "water", "infrastructure")
  # output_dir = file.path(miren_planning_dir, "output_target_sites", target_road)
  # write_output = c("points", "hulls")
  
  target_road <- tolower(target_road)
  cat(paste0("Target geometry: ", stringr::str_to_title(target_road), "\n"))
  
  ## > Determine target zones ----
  
  cat(" -- determining potential starting points for plots ...\n")
  
  # load geometry
  road_geom <- sf::read_sf(file.path(input_data_dir, paste0(target_road, "_road_osm_2056.shp")))
  
  # add buffer to geometry
  road_geom_buffer <- road_geom %>% 
    sf::st_buffer(dist = plot_length
                  , endCapStyle = "ROUND")
  
  ## > Load DEM ----
  # load DEM
  road_dem <- terra::rast(file.path(input_data_dir, paste0(target_road, "_road_dem_swissALTI3D_0.5m_2056.tif"))) %>% 
    setNames("elevation_m")
  
  # check that DEM is at same or finer resolution than desired slope mask
  dem_res <- road_dem %>% 
    terra::res()
  if(!all(dem_res < slope_mask_res)) {
    stop(paste0("desired resolution of slope mask needs to be larger than of DEM. Use a higher-resolution DEM or use larger slope mask resolution."))
  }
  
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

  
  ## > Limit road vector to ideal site elevation +- max elevation distance ----
  if(stringr::str_ends(site_elevations_file, ".xlsx")) read_fun <- readxl::read_excel
  if(stringr::str_ends(site_elevations_file, ".csv")) read_fun <- readr::read_csv
  
  site_elev <- read_fun(file.path(input_data_dir, site_elevations_file)) %>% 
    dplyr::rename(plot_id = 1) %>% 
    # filter out summary stats at the bottom
    dplyr::filter(stringr::str_detect(plot_id, "Plot [0-9]+")) %>% 
    # make plot IDs numeric
    dplyr::mutate(plot_id = stringr::str_remove(plot_id, "^Site |^site |^Plot | ^plot "),
                  plot_id = as.numeric(plot_id)) %>% 
    dplyr::select(plot_id, 
                  dplyr::contains(target_road, ignore.case = TRUE)) %>% 
    dplyr::rename("elevation_m" = 2) %>% 
    # create lower- and higher-elevation threshold columns for each plot group
    dplyr::mutate(elev_threshold_low = elevation_m - site_elev_dist_threshold,
                  elev_threshold_high = elevation_m + site_elev_dist_threshold) 
  
  n_sites <- site_elev %>% 
    dplyr::pull(plot_id) %>% 
    unique() %>% 
    length()
  
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
    sf::st_segmentize(dfMaxLength = resolution)
  
  # get road as vertices
  road_vertices <- road_geom_highres %>% 
    sf::st_cast("POINT", warn = FALSE)
  
  # get elevation along road
  road_elev <- road_dem %>% 
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
    # add unique ID column
    dplyr::mutate(id = 1:nrow(.)) %>% 
    # add Plot ID from site_elev
    dplyr::mutate(plot_id = findInterval(elevation_m
                                         , site_elev$elev_threshold_low
                                         , rightmost.closed = TRUE),
                  plot_id = as.numeric(plot_id))
  
  ## > Create perpendicular lines at vertices ----
  create_perpendicular_plots <- function(line_geom, 
                                         point_geom, 
                                         length, 
                                         width
  ){
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
    
    # Calculate 2D cross product to check if left and right are reversed
    cross <- dx * perp_dy - dy * perp_dx
    
    # If cross product is negative, our "left" is actually right - flip it
    if (cross < 0) {
      perp_dx <- -perp_dx
      perp_dy <- -perp_dy
    }
    
    # offset
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
  
  cat(" -- computing potential plot polygons ...\n")
  
  # Create perpendicular lines
  perpendicular_plots_lr <- purrr::map(road_vertices_zones_strict$geometry, ~ {
    create_perpendicular_plots(road_geom_highres$geometry[[1]]
                               , .x
                               , length = plot_length
                               , width = plot_width)
  })
  
  # Create separate sf objects for left and right
  perp_plots_left_sf <- sf::st_sf(
    id = road_vertices_zones_strict$id,
    elevation_m = road_vertices_zones_strict$elevation_m,
    plot_id = road_vertices_zones_strict$plot_id,
    side = "left",
    geometry = sf::st_sfc(purrr::map(perpendicular_plots_lr, "left"), crs = sf::st_crs(road_geom_highres))
  )
  
  perp_plots_right_sf <- sf::st_sf(
    id = road_vertices_zones_strict$id,
    elevation_m = road_vertices_zones_strict$elevation_m,
    plot_id = road_vertices_zones_strict$plot_id,
    side = "right",
    geometry = sf::st_sfc(purrr::map(perpendicular_plots_lr, "right"), crs = sf::st_crs(road_geom_highres))
  )
  
  # Or combine into one sf object with a side column
  perp_plots_bothsides_sf <- bind_rows(perp_plots_left_sf, perp_plots_right_sf)
  
  
  ## > Mask DEM (slope, road) ----
  
  cat(" -- masking steep slopes ...\n")
  
  # calculate steep slope mask
  slope_masking_thresh <- matrix(c(-Inf, slope_threshold_value, 1,
                                   slope_threshold_value, Inf, NA)
                                 , ncol = 3
                                 , byrow = T)
  
  # create mask
  slope_mask_agg_factor <- (slope_mask_res %/% dem_res) %>% unique()
  road_dem_slopemask <- road_dem %>% 
    terra::aggregate(fact = slope_mask_agg_factor, # aggregate to coarser resolution for slope calculation
                     fun = "mean") %>% 
    terra::terrain(v = "slope"
                   , unit = "degrees") %>% 
    terra::classify(slope_masking_thresh)
  
  # # mask DEM
  # road_dem_masked <- road_dem %>% 
  #   terra::mask(road_dem_slopemask)
  
  
  ## > Filter vertices ----
  
  filter_perpendicular_plots <- function(plots_sf,
                                         checkTerrain = TRUE,
                                         terrain_rast,
                                         conservative = FALSE,  # whether or not to consider adjacent cells, i.e. terra::extract(touches = TRUE)
                                         checkRoad = TRUE,
                                         road_geom,
                                         road_width,
                                         checkWater = TRUE,
                                         checkInfrastructure = TRUE
  ){
    
    if(checkTerrain){
      # print message
      cat(" -- > filtering based on terrain mask ...\n")
      
      check_NAs_polygon <- function(polygon,
                                    rast,
                                    touches = conservative,  # whether or not to consider adjacent cells, i.e. terra::extract(touches = TRUE) -- passed from parent
                                    summary = TRUE  # whether or not to return cell- (FALSE) or plot-level (TRUE) is.na()
      ){
        # make polygon a spatVector
        polygon <- terra::vect(polygon)
        
        # extract values
        NAs_in_polygon <- terra::extract(rast,
                                         polygon
                                         , touches = touches
                                         , ID = FALSE) %>% 
          dplyr::pull(1) %>% 
          is.na() %>% 
          {if(summary) any(.) else .}
        
        return(NAs_in_polygon)
      }
      
      # filter out plots with NAs
      plots_filtered_sf <- plots_sf %>% 
        dplyr::rowwise() %>% 
        dplyr::mutate(
          terrainNAs = check_NAs_polygon(
            polygon = geometry,
            rast = terrain_rast,
            touches = conservative,  # whether or not to consider adjacent cells, i.e. terra::extract(touches = TRUE) --
            summary = TRUE
          )
        ) %>% 
        dplyr::ungroup() %>% 
        tidylog::filter(!terrainNAs)
      
    } else {
      plots_filtered_sf <- plots_sf
    }
    
    if(checkRoad){
      # print message
      cat(" -- > filtering based on road intersections ...\n")
      
      check_intersect_polygon_line <- function(plot_polygon,
                                               road_polygon){

        # Convert to sf object if it's just geometry
        if (inherits(plot_polygon, "sfc")) {
          plot_polygon <- sf::st_sf(geometry = plot_polygon)
        }
        
        # # add small buffer around polygon to ensure road intersection
        # polygon <- polygon %>% sf::st_buffer(1)
        
        suppressWarnings({
          intersect <- sf::st_intersection(plot_polygon, road_polygon)
        })
        
        # Handle GEOMETRYCOLLECTION - extract only LINESTRING/MULTILINESTRING
        if (nrow(intersect) > 0) {
          geom_types <- sf::st_geometry_type(intersect)
          
          if (any(geom_types == "GEOMETRYCOLLECTION")) {
            # Extract linestring components from geometry collections
            intersect <- intersect %>%
              sf::st_collection_extract(type = "POLYGON")
          }
        }

        # cast to separate polygons
        n_intersect <- intersect %>%
          # sf::st_cast("MULTILINESTRING", warn = FALSE) %>% 
          sf::st_cast("POLYGON", warn = FALSE) %>%
          nrow()
        
        # Explicitly handle empty result
        if (length(nrow(intersect)) == 0) {
          return_value <- FALSE
        } else if (n_intersect > 1) {
          return_value <- TRUE
        } else if (n_intersect %in% c(0, 1)) {
          return_value <- FALSE
        }
        
        return(return_value)
      }
      
      # filter out plots with multiple intersections
      plots_filtered_sf <- plots_filtered_sf %>% 
        dplyr::rowwise() %>% 
        dplyr::mutate(roadIntersect = check_intersect_polygon_line(
          plot_polygon = geometry,
          road_polygon = sf::st_buffer(road_geom
                                       , dist = road_width / 2)
        )
      ) %>% 
        dplyr::ungroup() %>% 
        tidylog::filter(!roadIntersect)
      
    }
    
    if(checkWater){
      # print message
      cat(" -- > filtering based on water bodies ...\n")
      
      # get lakes & wetlands 
      roadside_lakes <- queryOSMwithRetry(bbox = sf::st_bbox(road_geom %>% 
                                                               sf::st_buffer(plot_length)) %>% 
                                            sf::st_transform("epsg:4326"),
                                          key = "natural", 
                                          value = "water",
                                          max_retries = 10) %>% 
        .$osm_polygons %>% 
        sf::st_as_sf()
      
      # get wetlands and add extra buffer
      roadside_wetlands <- queryOSMwithRetry(bbox = sf::st_bbox(road_geom %>% 
                                                                  sf::st_buffer(plot_length)) %>% 
                                               sf::st_transform("epsg:4326"),
                                             key = "natural", 
                                             value = "wetland",
                                             max_retries = 10) %>% 
        .$osm_polygons %>% 
        {if(!is.null(.)) 
          # add 10-m buffer
          sf::st_buffer(., dist = 10) %>% 
            sf::st_as_sf()
          else .}
      
      # get streams
      roadside_streams <- queryOSMwithRetry(bbox = sf::st_bbox(road_geom %>% 
                                                                 sf::st_buffer(plot_length)) %>% 
                                              sf::st_transform("epsg:4326"),
                                            key = "waterway", 
                                            value = "stream",
                                            max_retries = 10) %>% 
        .$osm_lines %>% 
        {if(!is.null(.)) 
          sf::st_buffer(., dist = 2)  %>% 
            sf::st_as_sf()
          else .}
      
      # combine features
      roadside_water <- dplyr::bind_rows(roadside_lakes,
                                         roadside_wetlands,
                                         roadside_streams) %>% 
        sf::st_transform(crs = "epsg:2056") %>% 
        sf::st_cast("MULTIPOLYGON")
      
      check_intersect_polygons <- function(plot_polygon,
                                           polygon2){
        intersects_yn <- sf::st_intersects(plot_polygon, 
                                           polygon2
                                           , sparse = F) %>% 
          any()
        return(intersects_yn)
      }
      # filter out plots with multiple intersections
      plots_filtered_sf <- plots_filtered_sf %>% 
        dplyr::rowwise() %>% 
        dplyr::mutate(waterIntersect = check_intersect_polygons(
          plot_polygon = geometry,
          polygon2 = roadside_water
        )
        ) %>% 
        dplyr::ungroup() %>% 
        tidylog::filter(!waterIntersect)
      
    }
    
    if(checkInfrastructure){
      # print message
      cat(" -- > filtering based on infrastructure ...\n")
      
      # get buildings 
      roadside_buildings <- queryOSMwithRetry(bbox = sf::st_bbox(road_geom %>% 
                                                                   sf::st_buffer(plot_length)) %>% 
                                                sf::st_transform("epsg:4326"),
                                              key = "building", 
                                              value = "yes",
                                              max_retries = 10) %>% 
        .$osm_polygons %>% 
        sf::st_as_sf()
      
      # load parkings from OSM
      roadside_parkings <- queryOSMwithRetry(bbox = sf::st_bbox(road_geom %>% 
                                                                  sf::st_buffer(plot_length)) %>% 
                                               sf::st_transform("epsg:4326"),
                                             key = "building", 
                                             value = "yes",
                                             max_retries = 10) %>% 
        .$osm_polygons %>% 
        {if(!is.null(.)) 
          dplyr::mutate(., area = sf::st_area(geometry)) %>% 
            # exclude small parkings < 250 m2
            dplyr::filter(as.numeric(area) > 250)
          else .}
      
      # load railways from OSM
      roadside_railways <- queryOSMwithRetry(bbox = sf::st_bbox(road_geom %>% 
                                                                  sf::st_buffer(plot_length)) %>% 
                                               sf::st_transform("epsg:4326"),
                                             key = "landuse", 
                                             value = "railway",
                                             max_retries = 10) %>% 
        .$osm_lines %>% 
        {if(!is.null(.)) 
          sf::st_buffer(., dist = road_width / 2) 
          else .}
      
      # combine features
      roadside_infrastructure <- dplyr::bind_rows(roadside_buildings,
                                                  roadside_parkings,
                                                  roadside_railways) %>% 
        sf::st_transform(crs = "epsg:2056") %>% 
        sf::st_cast("MULTIPOLYGON")
      
      check_intersect_polygons <- function(plot_polygon,
                                           polygon2){
        intersects_yn <- sf::st_intersects(plot_polygon, 
                                           polygon2
                                           , sparse = F) %>% 
          any()
        return(intersects_yn)
      }
      
      # filter out plots with multiple intersections
      plots_filtered_sf <- plots_filtered_sf %>% 
        dplyr::rowwise() %>% 
        dplyr::mutate(infraIntersect = check_intersect_polygons(
          plot_polygon = geometry,
          polygon2 = roadside_infrastructure
        )
        ) %>% 
        dplyr::ungroup() %>% 
        tidylog::filter(!infraIntersect)
      
    }
    
    return(plots_filtered_sf)
  }
  
  cat(" -- filtering potential plots ...\n")
  
  # exclude plot candidates overlapping with steep terrain, road, water, infrastructure etc
  if(any(c("terrain", "slope") %in% spatial_objects_filter)) check_steep_terrain <- TRUE
  if("road" %in% spatial_objects_filter) check_road_overlap <- TRUE
  if("water" %in% spatial_objects_filter) check_water_overlap <- TRUE
  if("infrastructure" %in% spatial_objects_filter) check_infrastructure_overlap <- TRUE

  perp_plots_bothsides_filtered <- perp_plots_bothsides_sf %>% 
  ### ~ no steep areas within perpendicular plot ----
    filter_perpendicular_plots(checkTerrain = check_steep_terrain,
                               terrain_rast = road_dem_slopemask,
                               conservative = FALSE,
  ### ~ perpendicular plot not overlapping with road line (in road turns) ----
                               checkRoad = check_road_overlap,
                               road_geom = road_geom,
                               road_width = 4,
  ### ~ plot not overlapping with larger water bodies ----
                               checkWater = check_water_overlap,
  ### ~ plot not overlapping with infrastructure ----
                               checkInfrastructure = check_infrastructure_overlap) %>% 
    dplyr::arrange(plot_id)
  
            #   
            #           # perp_plots_bothsides_filtered %>% 
            #           #   sf::st_drop_geometry() %>% 
            #           #   summarise(n_candidates = n()
            #           #             , .by = plot_id) %>% 
            #           #   arrange(plot_id)
            #           # #> Site 5 with no plots remaining!! in the middle of serpentines
            # perp_plots_bothsides_filtered %>%
            #   sf::st_write(file.path(miren_planning_dir, "zz_temp_flüela_perpendicular_plot_candidates_filtered.shp"))
            # # road_dem_slopemask %>% terra::writeRaster(file.path(miren_planning_dir, "zz_temp_flüela_slopemask.tif"))
    
  
  ## > Select final plots ----
  
  cat(" -- determining final set of plots ...\n")

  ### ~ remove disparate clusters ----
  perp_plots_bothsides_disparate <- perp_plots_bothsides_filtered %>% 
    dplyr::group_by(plot_id) %>% 
    dplyr::mutate(centroid = sf::st_centroid(geometry),
                  centroid_first = dplyr::first(centroid)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(dist_to_first = sf::st_distance(geometry, centroid_first
                                                  , by_element = TRUE),
                  dist_to_first = as.numeric(dist_to_first)) %>% 
    tidylog::filter(dist_to_first <= 1000) %>% 
    dplyr::select(-dplyr::starts_with("centroid"),
                  -dist_to_first)
  
  ### ~ pick plot closest to ideal elevation if several remain within a group ----
  perp_plots_bothsides_selection <- perp_plots_bothsides_disparate %>% 
    dplyr::left_join(site_elev %>% dplyr::select(plot_id,
                                                 elevation_ideal = elevation_m)
                     , by = "plot_id") %>% 
    dplyr::mutate(elevation_diff_abs = abs(elevation_m - elevation_ideal)) %>% 
    dplyr::slice_min(order_by = elevation_diff_abs,
                     by = c(plot_id, side),
                     n = 1,
                     with_ties = FALSE) %>% 
    dplyr::arrange(plot_id)
  
  ### ~ check if all groups have left and right available ----
  
  ## determine any sites missing entirely
  sites_missing <- setdiff(site_elev$plot_id, unique(perp_plots_bothsides_selection$plot_id))
  
  if(length(sites_missing) > 0){
    ## print warning
    warning(paste0(stringr::str_to_title(target_road), ": after applying spatial filters with current settings, no available plots remain at site(s) ", paste(sites_missing, collapse = ", "), "."))
    
    sites_missing <- tibble::tibble(plot_id = rep(sites_missing, each = 2),
                                    side = rep(c("left", "right"), times = length(sites_missing)),
                                    available = FALSE)
  }
  sites_lr_available <- perp_plots_bothsides_selection %>% 
    sf::st_drop_geometry() %>% 
    dplyr::mutate(available = TRUE) %>% 
    dplyr::select(plot_id, side, available) %>% 
    {if(length(sites_missing) > 0) dplyr::bind_rows(., sites_missing) else .} %>% 
    tidyr::pivot_wider(names_from = side,
                       values_from = available
                       , values_fill = FALSE) %>% 
    dplyr::arrange(plot_id)
  
  ## check left-side plots, print warning
  sites_wo_left <- sites_lr_available %>% dplyr::filter(!left) %>% dplyr::pull(plot_id)
  if(length(sites_wo_left) > 0) 
    warning(paste0("After applying spatial filters, no available LEFT-side plots remaining at site(s) ", paste(sort(sites_wo_left), collapse = ", "), "."))
  
  ## check right-side plots, print warning
  sites_wo_right <- sites_lr_available %>% dplyr::filter(!right) %>% dplyr::pull(plot_id)
  if(length(sites_wo_right) > 0) 
    warning(paste0("After applying spatial filters, no available RIGHT-side plots remaining at site(s) ", paste(sort(sites_wo_right), collapse = ", "), "."))
  
  ### ~ determine left-right sequence ----
  
  # set order of sides, select final plots
  order_select_sites <- function(start_side = start_side,
                                 all_sites_lr = sites_lr_available,
                                 candidate_plot_selection = perp_plots_bothsides_selection){
    # get alternating sequence
    sides_order <- rep(c(start_side,
                         setdiff(c("left", "right"), start_side)),
                       times = ceiling(n_sites/2))
    # combine plots with sequence
    target_sites_sides <- tibble::tibble(plot_id = all_sites_lr$plot_id,
                                         target_side = sides_order)
    # filter candidates selection for final sides to sample
    sites_plot_selection <- candidate_plot_selection %>%
      dplyr::select(plot_id, 
                    id, 
                    elevation_m, 
                    side,
                    geometry) %>% 
      dplyr::left_join(target_sites_sides
                       , by = "plot_id") %>% 
      dplyr::group_by(plot_id) %>% 
      dplyr::filter(if(n() > 1) side == target_side else TRUE) %>%  # only filter for ideal side if both sides available at a side
      dplyr::ungroup() %>% 
      dplyr::select(-target_side)
    return(sites_plot_selection)
  }
  
  ## if all left/right combinations available, pick random order
  all_available <- length(c(sites_wo_left, sites_wo_right)) == 0
  if(all_available){
    set.seed(35)
    start_side <- sample(c("left", "right"), 1)
    
    target_sites_selection <- order_select_sites(start_side = start_side,
                                                 all_sites_lr = sites_lr_available,
                                                 candidate_plot_selection = perp_plots_bothsides_selection)
    
  } else {
    
  ## if not all left-right combinations available, check which starting side returns the longer sequence
    
    sites_left_first <- sites_lr_available %>%
      tidyr::pivot_longer(cols = c(left, right), names_to = "side", values_to = "available") %>%
      dplyr::filter(available == TRUE) %>%
      dplyr::arrange(plot_id, side)
    sites_right_first <- sites_lr_available %>%
      tidyr::pivot_longer(cols = c(left, right), names_to = "side", values_to = "available") %>%
      dplyr::filter(available == TRUE) %>%
      dplyr::arrange(plot_id, desc(side))
    determine_alternating_sequence_length <- function(sites_df){
      sites_df %>%
        dplyr::mutate(
          alternates = side != dplyr::lag(side, default = ""),
          group = cumsum(!alternates)
        ) %>%
        dplyr::group_by(group) %>%
        dplyr::mutate(seq_length = n()) %>%
        dplyr::ungroup() %>%
        dplyr::filter(seq_length == max(seq_length)) %>%
        nrow()
    }
    if(determine_alternating_sequence_length(sites_left_first) > determine_alternating_sequence_length(sites_right_first)){  # if starting left yields longer alternating sequence
      start_side <- "left"
    } else if(determine_alternating_sequence_length(sites_left_first) < determine_alternating_sequence_length(sites_right_first)){  # if starting right yields longer alternating sequence
      start_side <- "right"
    } else if(determine_alternating_sequence_length(sites_left_first) == determine_alternating_sequence_length(sites_right_first)){  # if even
      start_side <- sample(c("left", "right"), 1)  # determine random starting side
    }
    
    target_sites_selection <- order_select_sites(start_side = start_side,
                                                 all_sites_lr = sites_lr_available,
                                                 candidate_plot_selection = perp_plots_bothsides_selection)
  }
  
  
  ## > Get points at roadside ----
  
  find_roadside_point <- function(plot_polygon,  # plot polygon perpendicular to road
                                  road_geom,  # road geometry
                                  distance = 2  # distance from road centre in m
  ) {
    
    # plot_polygon <- polygon_test_F %>% st_sf()
    # distance <- 2
    
    # 1. Get polygon vertices
    coords <- st_coordinates(st_cast(st_boundary(plot_polygon), "POINT", warn = FALSE))
    coords <- coords[1:(nrow(coords)-1), ]  # Remove duplicate last point
    
    # 2. Find the longest edge (orientation of rectangle)
    max_length <- 0
    best_edge <- NULL
    
    for (i in 1:nrow(coords)) {
      next_i <- ifelse(i == nrow(coords), 1, i + 1)
      
      edge_length <- sqrt((coords[next_i, "X"] - coords[i, "X"])^2 + 
                            (coords[next_i, "Y"] - coords[i, "Y"])^2)
      
      if (edge_length > max_length) {
        max_length <- edge_length
        best_edge <- c(i, next_i)
      }
    }
    
    # 3. Calculate direction vector from longest edge
    dx <- coords[best_edge[2], "X"] - coords[best_edge[1], "X"]
    dy <- coords[best_edge[2], "Y"] - coords[best_edge[1], "Y"]
    
    # Normalize direction vector
    length_edge <- sqrt(dx^2 + dy^2)
    dx_norm <- dx / length_edge
    dy_norm <- dy / length_edge
    
    # 4. Create centerline along this direction through polygon centroid
    suppressWarnings({
      centroid <- st_centroid(plot_polygon)
    })
    centroid_coords <- st_coordinates(centroid)
    
    # Extend centerline far in both directions
    extension <- max_length * 10
    
    start_point <- c(centroid_coords[1, "X"] - dx_norm * extension,
                     centroid_coords[1, "Y"] - dy_norm * extension)
    end_point <- c(centroid_coords[1, "X"] + dx_norm * extension,
                   centroid_coords[1, "Y"] + dy_norm * extension)
    
    centerline <- st_linestring(rbind(start_point, end_point))
    centerline <- st_sfc(centerline, crs = st_crs(plot_polygon))
    
    # 5. Find intersection with road
    int_point <- st_intersection(centerline, road_geom) %>% 
      sf::st_cast("POINT") %>% 
      sf::st_sf()
    
    if (nrow(int_point) == 0) {
      stop("Centerline does not intersect the reference line")  # throw error
    } else if (nrow(int_point) == 1){
      int_point_closest <- int_point  # define only intersection point as closest
    } else if (nrow(int_point) > 1) {
      int_point_closest <- int_point %>% 
        # calculate distance to centroid
        dplyr::mutate(dist_centroid = sf::st_distance(geometry, centroid),
                      dist_centroid = as.numeric(dist_centroid)) %>% 
        # filter for closest intersection
        dplyr::slice_min(dist_centroid, n = 1)
    }
    
    int_coords <- st_coordinates(int_point_closest)
    
    # 6. Determine correct direction (toward polygon centroid)
    to_centroid_x <- centroid_coords[1, "X"] - int_coords[1, "X"]
    to_centroid_y <- centroid_coords[1, "Y"] - int_coords[1, "Y"]
    
    # Check if direction vector points toward centroid
    dot_product <- dx_norm * to_centroid_x + dy_norm * to_centroid_y
    
    if (dot_product < 0) {
      # Reverse direction
      dx_norm <- -dx_norm
      dy_norm <- -dy_norm
    }
    
    # 7. Calculate target point at distance along centerline
    target_x <- int_coords[1, "X"] + dx_norm * distance
    target_y <- int_coords[1, "Y"] + dy_norm * distance
    
    target_point <- st_point(c(target_x, target_y))
    target_point <- st_sfc(target_point, crs = st_crs(plot_polygon))
    
    return(target_point)
  }
  
  target_plot_points <- target_sites_selection %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(geometry = find_roadside_point(geometry, 
                                                 road_geom = road_geom, 
                                                 distance = 3)) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(plot_id, 
                  id,
                  elevation_m, 
                  side, 
                  geometry)
  
  ## > Obtain convex hull around all candidate plots within site ----
  site_hulls_candidate_plots <- perp_plots_bothsides_disparate %>% 
    dplyr::group_by(plot_id) %>% 
    dplyr::summarise(geometry = sf::st_union(geometry),
                     n_plot_candidates = n()
                     , .groups = "drop") %>% 
    sf::st_convex_hull()
  
  
  ## > Write shapefiles ----
  if(!dir.exists(output_dir)) dir.create(output_dir)
  # polygons
  if(length(write_output) == 1 && write_output == TRUE | "plots" %in% write_output | "polygons" %in% write_output){
    sf::st_write(perp_plots_bothsides_filtered,
                 dsn = file.path(output_dir, paste0("GloNoMo_MIREN_target_plot_polygons_", target_road, ".shp")))
  }
  # points
  if(length(write_output) == 1 && write_output == TRUE | "points" %in% write_output){
    sf::st_write(target_plot_points,
                 dsn = file.path(output_dir, paste0("GloNoMo_MIREN_target_plot_points_", target_road, ".shp")))
  }
  # hulls
  if(length(write_output) == 1 && write_output == TRUE | "hulls" %in% write_output){
    sf::st_write(site_hulls_candidate_plots,
                 dsn = file.path(output_dir, paste0("GloNoMo_MIREN_potential_sampling_areas_", target_road, "_revised.shp")))
  }
  
  ## > Return list of selected sites and candidate plots hull ----
  return(list(priority_plots = target_plot_points,
              candidate_polygons = perp_plots_bothsides_filtered,
              site_hulls = site_hulls_candidate_plots))
}

# Run function for multiple roads/tracks ----
glonomo_sites_snpp <- purrr::map(c(
  # "Flüela",
  # "Ofenpass",
  "Umbrail_Stelvio"
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
