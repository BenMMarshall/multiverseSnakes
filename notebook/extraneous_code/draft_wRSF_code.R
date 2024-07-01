
targets::tar_load("movementData_BUFA_H1_binary")

movementData <- movementData_BUFA_H1_binary$movementData_sf
landscape <- rast(allIndividualData$habitatRasterLocation)
landscapeRaster <- raster::raster(allIndividualData$habitatRasterLocation)
land <- str_extract(allIndividualData$habitatRasterLocation, "binary|continuous")

movementDataLL <- st_transform(movementData, crs = "EPSG:4326")
movementDataLL <- movementDataLL %>% 
  rename("timestamp" = datetime)

movementDataLL$lon <- sf::st_coordinates(movementDataLL)[,1]
movementDataLL$lat <- sf::st_coordinates(movementDataLL)[,2]
movementDataLL <- st_drop_geometry(movementDataLL)
# spPoints <- sp::SpatialPoints(movementData[,c("x", "y")],
#                               sp::CRS(SRS_string = "EPSG:32601"))
# spLL <- sp::spTransform(spPoints, sp::CRS(SRS_string = "EPSG:4326"))
# movementData$lon <- spLL@coords[,1]
# movementData$lat <- spLL@coords[,2]

teleObj <- ctmm::as.telemetry(movementDataLL,
                              timeformat = "%Y-%m-%d %H:%M:%S",
                              timezone = "Asia/Bangkok",
                              projection = sp::CRS(SRS_string = "EPSG:32647"))

## cannot pass teleobj of multiple IDs to variogram function

varioDataVarList <- lapply(teleObj, function(x){
  varioDataVar <- ctmm::variogram(x, fast = TRUE)
  return(varioDataVar)
})
fitsList <- lapply(teleObj, function(x){
  guess <- ctmm::ctmm.guess(x, interactive = FALSE)
  fits <- try(
    ctmm::ctmm.select(x, guess, verbose = FALSE,
                      cores = 4, method = "pHREML")
  )
  return(fits)
})
print("fit")

akdeRes <- ctmm::akde(teleObj, fitsList,
           weights = TRUE)

# NEED TO FILL IN NAs in raster
landscapeRaster[is.na(landscapeRaster)] <- 0
landscapeRaster[] <- as.factor(landscapeRaster[])

wRSF <- ctmm:::rsf.fit(teleObj[[1]],
                       UD = akdeRes[[1]],
                       R = list(c = landscapeRaster),
                       # R = list(
                       #   c0 = r0,
                       #   c1 = r1,
                       #   c2 = r2),
                       error = 0.01,
                       reference = 0,
                       max.mem = "1 Gb")
# print("teleObj")
# # can do the slower one for the real deal
# # varioDataVar <- variogram(teleObj, fast = FALSE, CI = "Gauss")
# varioDataVar <- ctmm::variogram(teleObj[[1]], fast = TRUE)
# print("vario")
# guess <- ctmm::ctmm.guess(teleObj[[1]], interactive = FALSE)
# print("guess")
# # need to specify more cores???
# fits <- try(
#   ctmm::ctmm.select(teleObj[[1]], guess, verbose = FALSE,
#                     cores = 4, method = "pHREML")
# )
# print("fit")

if(class(fits) == "try-error"){
  area_OUT <- fits
} else {
  # akdeRes <- ctmm::akde(teleObj, fits[[1]],
  #                       weights = TRUE)
  # needed to catch weird instances with limited data
  akdeRes <- try(
    ctmm::akde(teleObj, fits,
               weights = TRUE)
  )
  print("area")
  area_OUT <- akdeRes
}

### POPULATION LEVEL ADKE ALSO AVAILABLE
# pkde(data,UD,kernel="individual",weights=FALSE,ref="Gaussian",...)

##################
for(lc in Method_lc){
  i <- i+1
  
  wRSF <- ctmm:::rsf.fit(teleObj,
                         UD = areaOUT,
                         R = list(c = landscape[[paste0(lc, "LatLon")]]),
                         # R = list(
                         #   c0 = r0,
                         #   c1 = r1,
                         #   c2 = r2),
                         error = 0.01,
                         reference = 1,
                         max.mem = "1 Gb")
  
  # summary(wRSF)
  wRSFOUT <- summary(wRSF)$CI[1,]
  rm(wRSF)
  
  listOUT[[i]] <- data.frame(
    Estimate = wRSFOUT["est"],
    Lower = wRSFOUT["low"],
    Upper = wRSFOUT["high"],
    analysis = "wRSF",
    classLandscape = lc,
    area = NA,
    contour = NA,
    availPointsPer = NA,
    samplingPattern = NA,
    weighting = NA
  )
  
} # lc
#################

# } # else bit to skip certain wrsf because of long run times

} # if error in akde area method
} # if wRSF
