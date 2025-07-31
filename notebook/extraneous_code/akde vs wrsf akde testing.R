
library(here)
library(dplyr)
library(amt)
library(ctmm)
library(sf)
library(raster)
library(stringr)

targets::tar_load("movementData_OPHA_H1_binary")

landscapeRaster <- raster(here("data", "rasterOPHA_H1_binary.tif"))
movementData <- movementData_OPHA_H1_binary$movementData_sf

land <- str_extract("rasterOPHA_H1_binary.tif", "binary|continuous")

movementDataLL <- st_transform(movementData, crs = "EPSG:4326")
movementDataLL <- movementDataLL %>% 
  rename("timestamp" = datetime)

movementDataLL$lon <- sf::st_coordinates(movementDataLL)[,1]
movementDataLL$lat <- sf::st_coordinates(movementDataLL)[,2]
movementDataLL <- st_drop_geometry(movementDataLL)

teleObj <- ctmm::as.telemetry(as.data.frame(movementDataLL),
                              timeformat = "%Y-%m-%d %H:%M:%S",
                              timezone = "Asia/Bangkok",
                              projection = sp::CRS(SRS_string = "EPSG:32647"))

# can skip if already saved -----------------------------------------------

## cannot pass teleobj of multiple IDs to variogram function
varioDataVarList <- lapply(teleObj, function(x){
  varioDataVar <- ctmm::variogram(x, fast = TRUE)
  return(varioDataVar)
})
print("--- variogram")
fitsList <- lapply(teleObj, function(x){
  guess <- ctmm::ctmm.guess(x, interactive = FALSE)
  fits <- try(
    ctmm::ctmm.select(x, guess, verbose = FALSE,
                      cores = 4, method = "pHREML")
  )
  return(fits)
})
print("--- ctmm.select")

akdeList <- vector("list", length = length(names(teleObj)))
names(akdeList) <- names(teleObj)
for(n in names(teleObj)){
  akdeList[[n]] <- ctmm::akde(teleObj[[n]], fitsList[[n]],
                              weights = TRUE)
}
print("--- akde")

if(land == "binary"){
  landscapeRaster[] <- as.factor(landscapeRaster[])
  ref <- 1
} else {
  ref <- "auto"
}

wrsfList <- vector("list", length = length(teleObj))
names(wrsfList) <- names(teleObj)
for(indi in 1:length(teleObj)){
  print(paste0("--- wrsf: ", names(wrsfList)[indi]))
  wrsfOUT <- try(
    rsf.fit(teleObj[[indi]],
            UD = akdeList[[indi]],
            R = list("H" = landscapeRaster),
            debias = TRUE)
  )
  
  if(class(wrsfOUT)[1] == "try-error"){
    wrsfOUT <- NULL
  }
  wrsfList[[names(teleObj[indi])]] <- wrsfOUT
}
# wrsfList
wrsfList <- wrsfList[!sapply(wrsfList, is.null)]

# save(teleObj, file = here("notebook", "teleObj.RData"))
# save(varioDataVarList, file = here("notebook", "varioDataVarList.RData"))
# save(fitsList, file = here("notebook", "fitsList.RData"))
# save(akdeList, file = here("notebook", "akdeList.RData"))
# save(wrsfList, file = here("notebook", "wrsfList.RData"))

# resume ------------------------------------------------------------------

load(here("notebook", "teleObj.RData"))
load(here("notebook", "varioDataVarList.RData"))
load(here("notebook", "fitsList.RData"))
load(here("notebook", "akdeList.RData"))
load(here("notebook", "wrsfList.RData"))

summary(wrsfList$OPHA010)

naiveAKDEpolyList <- vector("list", length = length(akdeList))
names(naiveAKDEpolyList) <- names(akdeList)
wrsfAKDEpolyList <- vector("list", length = length(akdeList))
names(wrsfAKDEpolyList) <- names(akdeList)
for(indi in names(naiveAKDEpolyList)){
  
  akdePoly <- ctmm::SpatialPolygonsDataFrame.UD(object = akdeList[[indi]], level.UD = 95/100)
  # just get the point estimate
  poly_OUT <- akdePoly[akdePoly$name ==
                         akdePoly$name[stringr::str_detect(akdePoly$name, "est")],]
  
  RAKDE <- akde(data = teleObj[[indi]], CTMM = akdeList[[indi]], RSF = wrsfList[[indi]], R = landscapeRaster, weights=TRUE)
  # plot(teleObj[[indi]], error=2, UD=RAKDE, col.grid=NA, main="iRSF-AKDE")
  wrsfPoly <- ctmm::SpatialPolygonsDataFrame.UD(object = RAKDE[[1]], UD = 0.95)
  # just get the point estimate
  polywrsf_OUT <- wrsfPoly[wrsfPoly$name ==
                         wrsfPoly$name[stringr::str_detect(wrsfPoly$name, "est")],]
  naiveAKDEpolyList[[indi]] <- poly_OUT
  wrsfAKDEpolyList[[indi]] <- polywrsf_OUT
}

naive95Poly <- do.call(rbind, naiveAKDEpolyList)
wrsf95Poly <- do.call(rbind, wrsfAKDEpolyList)

library(ggplot2)

ggplot() +
  geom_polygon(data = naive95Poly, aes(x = long, y = lat, group = group),
               alpha = 0.15, colour = "red", fill = NA) +
  geom_polygon(data = wrsf95Poly, aes(x = long, y = lat, group = group),
               alpha = 0.15, fill = "blue", colour = NA) +
  facet_wrap(facet = vars(id))
