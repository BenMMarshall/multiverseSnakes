#' Calculate the population results from wrsf
#'
#' @name method_wrsf
#' @description A
#' @param allIndividualData The full list of all simulated individuals, subsampled into regimes
#' @return Population estimates for using wrsf
#'
#' @export
method_wrsf <- function(allIndividualData){
  
  # targets::tar_load("movementData_OPHA_H1_binary")
  # targets::tar_load("movementData_BUCA_H1_continuous")
  # allIndividualData <- movementData_OPHA_H1_binary
  # allIndividualData <- movementData_BUCA_H1_continuous
  movementData <- allIndividualData$movementData_sf
  landscapeRaster <- raster::raster(allIndividualData$habitatRasterLocation)
  land <- str_extract(allIndividualData$habitatRasterLocation, "binary|continuous")
  
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
  
  # RSF.1 <- rsf.fit(teleObj$BUFA001, UD = akdeList$BUFA001, R = list("H1" = landscapeRaster),
  #                  debias = TRUE, 
  #                  reference = ref)
  # RSF.2 <- rsf.fit(teleObj$BUFA003, UD = akdeList$BUFA003, R = list("H1" = landscapeRaster),
  #                  debias = TRUE, 
  #                  reference = ref)
  # RSF.3 <- rsf.fit(teleObj$BUFA004, UD = akdeList$BUFA004, R = list("H1" = landscapeRaster),
  #                  debias = TRUE, 
  #                  reference = ref)
  
  wrsfList <- vector("list", length = length(teleObj))
  names(wrsfList) <- names(teleObj)
  for(indi in 1:length(teleObj)){
    print(paste0("--- wrsf: ", names(wrsfList)[indi]))
    wrsfOUT <- try(
      rsf.fit(teleObj[[indi]],
              UD = akdeList[[indi]],
              R = list("H" = landscapeRaster),
              debias = TRUE,
              # max.mem = "4 Gb",
              reference = ref)
    )
    
    if(class(wrsfOUT)[1] == "try-error"){
      wrsfOUT <- NULL
    }
    wrsfList[[names(teleObj[indi])]] <- wrsfOUT
  }
  # wrsfList
  wrsfList <- wrsfList[!sapply(wrsfList, is.null)]
  
  wrsfPopulation <- try(mean(wrsfList))
  
  # begin a series of for loops if the mean wrsf fails
  if(class(wrsfPopulation) == "try-error"){
    for(i in 1:(length(wrsfList)+1)){
      
      wrsfPopulation <- try(mean(wrsfList[-i]))
      
      if(!class(wrsfPopulation) == "try-error"){
        wrsfPopulation$exclusion <- 
          names(wrsfList)[i]
        
        print(wrsfPopulation$exclusion)
        {break}
      }
    }
  }
  
  if(class(wrsfPopulation) == "try-error"){
    twoIndiExclude <- expand.grid(1:length(wrsfList), 1:length(wrsfList))
    twoIndiExclude <- twoIndiExclude %>% 
      dplyr::filter(!Var1 == Var2)
    for(j in seq_along(nrow(twoIndiExclude))){
      # j <- 1
      tempList <- wrsfList[-twoIndiExclude[j,][[1]]]
      tempList <- tempList[-twoIndiExclude[j,][[2]]]
      wrsfPopulation <- try(mean(tempList))
      
      if(!class(wrsfPopulation) == "try-error"){
        wrsfPopulation$exclusion <- 
          names(wrsfList)[!names(wrsfList) %in% names(tempList)]
        print(wrsfPopulation$exclusion)
        {break}
      }
    }
  }
  
  if(class(wrsfPopulation) == "try-error"){
    threeIndiExclude <- expand.grid(1:length(wrsfList), 1:length(wrsfList), 1:length(wrsfList))
    threeIndiExclude <- threeIndiExclude %>% 
      dplyr::filter(!Var1 == Var2,
                    !Var2 == Var3,
                    !Var1 == Var3)
    for(j in 1:nrow(threeIndiExclude)){
      # j <- 1
      tempList1 <- wrsfList[-threeIndiExclude[j,][[1]]]
      names(tempList1)
      tempList2 <- tempList1[-threeIndiExclude[j,][[2]]]
      names(tempList2)
      tempList3 <- tempList2[-threeIndiExclude[j,][[3]]]
      names(tempList3)
      wrsfPopulation <- try(mean(tempList3))
      
      if(!class(wrsfPopulation) == "try-error"){
        wrsfPopulation$exclusion <- 
          names(wrsfList)[!names(wrsfList) %in% names(tempList)]
        print(wrsfPopulation$exclusion)
        {break}
      }
    }
  }
  
  return(wrsfPopulation)
}