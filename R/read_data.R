#' Read in all snake data and spatial data
#'
#' @name read_data
#' @description A
#' @param species c("OPHA", "PYBI", "BUCA", "BUFA")
#' @param landscape c("binary", "continuous")
#' @return a
#'
#' @export
read_data <- function(species, hypothesis, landscape){
  # species <- "OPHA"
  # species <- "PYBI"
  # species <- "BUCA"
  # species <- "BUFA"
  # hypothesis <- "H1"
  # landscape <- "binary"
  print(species)
  
  movementData <- read_csv(here("data", paste0("movement", species, ".csv")),
                           locale = locale(tz = "Asia/Bangkok"))
  
  if(species == "BUFA"){
    # add an hour to the date only BUFA times, prevents issues with ctmm as.telemetry later
    movementData <- movementData %>% 
      mutate(datetime = datetime + 60*60) %>% 
      mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S",
                                   tz = "Asia/Bangkok"))
  }
  
  if(length(unique(movementData$UTMzone)) == 2){
    
    moveData_47 <- movementData %>% 
      filter(UTMzone == "47N")
    moveData_48 <- movementData %>% 
      filter(UTMzone == "48N")
    
    coord47 <- st_as_sf(moveData_47, coords = c("x", "y"), remove = FALSE,
                        crs = st_crs("EPSG:32647"))
    
    coord48 <- st_as_sf(moveData_48, coords = c("x", "y"), remove = FALSE,
                        crs = st_crs("EPSG:32648"))
    coord47Converted <- st_transform(coord48, crs = st_crs("EPSG:32647"))
    movementData_sf <- rbind(coord47, coord47Converted) %>% 
      arrange(datetime, id)
    movementData_sf$UTMzone <- "47N"
    
    movementData_sf$x <- sf::st_coordinates(movementData_sf)[,1]
    movementData_sf$y <- sf::st_coordinates(movementData_sf)[,2]
    
    # drop 029 becuase they have too few relocations to even make an MCP
    movementData_sf <- movementData_sf %>% 
      filter(!id == "BUCA029")
    
  } else {
    movementData_sf <- st_as_sf(movementData, coords = c("x", "y"), remove = FALSE,
                                crs = st_crs("EPSG:32647"))
  }
  
  binaryLandscape <- sf::read_sf(here("data", paste0("landuse", species, ".geoJSON")))
  
  extent_m <- ext(binaryLandscape)
  # will result in a grid that has a 1 m x 1 m res
  xRes <- abs(extent_m[1] - extent_m[2])
  yRes <- abs(extent_m[3] - extent_m[4])
  
  # ggplot() +
  #   geom_sf(data = binaryLandscape, aes(fill = landuse)) +
  #   geom_sf(data = movementData_sf)
  
  template <- rast(vect(binaryLandscape), nrows = yRes, ncols = xRes)
  
  binaryRaster <- rasterize(vect(binaryLandscape %>% 
                                   filter(habitat == paste0(hypothesis, "_Habitat"))),
                            template)
  
  print("Binary Raster")
  
  if(landscape == "binary"){
    binaryRaster[is.na(binaryRaster)] <- 0
    habitatRaster <- binaryRaster
  } else if(landscape == "continuous"){
    
    distanceRaster <- distance(binaryRaster)
    print("Distance Raster")
    distanceValues <- terra::values(distanceRaster)
    terra::values(distanceRaster) <- abs(distanceValues - max(distanceValues))
    print("Distance Inverted")
    # (x-min(x))/(max(x)-min(x))
    habitatRaster <- distanceRaster
  }
  
  writeRaster(habitatRaster, filename = here("data", paste0("raster", species,
                                                            "_", hypothesis, 
                                                            "_", landscape, ".tif")),
              overwrite = TRUE,  gdal = c("COMPRESS=LZW"))
  
  return(list("habitatRasterLocation" = here("data", paste0("raster", species,
                                                            "_", hypothesis, 
                                                            "_", landscape, ".tif")),
              "movementData_sf" = movementData_sf))
  
  # have to wrap terra rasters for them to save, must unwrap at the other end
  # DOESN'T SEEM TO WORK
  # return(list("habitatRaster" = wrap(habitatRaster),
  #             "movementData_sf" = movementData_sf))
  
}
