#' Extract all required values for area methods
#'
#' @name area_based_extraction
#' @description A
#' @param allIndividualData The full list of all simulated individuals, subsampled into regimes
#' @param optionsList 
#' @return a
#'
#' @export
area_based_extraction <- function(allIndividualData, optionsList){
  # targets::tar_load("movementData_BUFA_H1_binary")
  # targets::tar_load("movementData_BUFA_H1_continuous")
  # targets::tar_load("movementData_BUCA_H1_binary")
  # targets::tar_load("movementData_OPHA_H1_binary")
  # targets::tar_source()
  # allIndividualData <- movementData_BUFA_H1_binary
  # allIndividualData <- movementData_BUFA_H1_continuous
  # allIndividualData <- movementData_BUCA_H1_binary
  # allIndividualData <- movementData_OPHA_H1_binary
  # optionsList <- optionsList_area
  areaMethod <- optionsList$areaMethod
  areaContour <- optionsList$areaContour
  Method_ap <- optionsList$Method_ap
  Method_sp <- optionsList$Method_sp
  
  maxAP <- max(Method_ap)
  
  landscape <- rast(allIndividualData$habitatRasterLocation)
  landscapeRaster <- raster::raster(allIndividualData$habitatRasterLocation)
  land <- str_extract(allIndividualData$habitatRasterLocation, "binary|continuous")
  hypo <- str_extract(allIndividualData$habitatRasterLocation, "H1|H2")
  
  # Loop to create individual polygons --------------------------------------
  
  i <- 0
  usedAvailList <- vector("list",
                          length = 
                            length(areaMethod) *
                            length(areaContour) *
                            length(Method_sp) *
                            length(Method_ap))
  rsfUsedAvailList <- vector("list",
                             length = 
                               length(areaMethod) *
                               length(areaContour) *
                               length(Method_sp) *
                               length(Method_ap))
  
  indiIDs <- unique(allIndividualData$movementData_sf$id)
  for(method in areaMethod){
    # method <- areaMethod[1]
    resourceList <- vector("list", length = length(indiIDs))
    names(resourceList) <- indiIDs
    for(indiID in indiIDs){
      # indiID <- "OPHA099"
      movementData <- allIndividualData$movementData_sf %>% 
        filter(id == indiID) %>% 
        st_drop_geometry()
      
      baa <- build_available_area(movementData = movementData,
                                  method = method,
                                  SRS_string = "EPSG:32647",
                                  dBBMMsettings = NULL)
      
      resourceList[[indiID]] <- baa
      
      print(paste0(indiID, " --- Area Resource --- ", method))
    }
    
    for(contour in areaContour){
      # contour <- areaContour[1]
      print(contour)
      
      polygonList <- vector("list", length = length(indiIDs))
      names(polygonList) <- indiIDs
      for(indiID in indiIDs){
        
        print(indiID)
        
        bap <- build_available_polygon(areaResource = resourceList[[indiID]],
                                       method = method,
                                       contour = contour,
                                       SRS_string = "EPSG:32647")
        
        
        polygonList[[indiID]] <- bap
        
        print(paste0(indiID, " --- Polygon created --- ", method, " - ", contour))
      } # end indiID polygon generation
      polygonList
      
      for(spSamp in Method_sp){
        # spSamp <- Method_sp[1]
        print(spSamp)
        for(aPoints in Method_ap){
          # aPoints <- Method_ap[1]
          print(aPoints)
          
          # Individual use / available ----------------------------------------------
          # usedAvailList <- vector("list", length = length(names(allIndividualData)[-1]))
          # names(usedAvailList) <- names(allIndividualData)[-1]
          for(indiID in indiIDs){
            # indiID <- "OPHA007"
            print(indiID)
            
            movementData <- allIndividualData$movementData_sf %>% 
              filter(id == indiID) %>% 
              st_drop_geometry()
            
            indiPolygon <- polygonList[[indiID]]
            
            # generate points based on the availableArea and the number of points
            suppressWarnings({
              availPoints <- sp::spsample(indiPolygon,
                                          n = nrow(movementData) * aPoints,
                                          type = ifelse(spSamp == "rd", "random", "stratified"))
            })
            crs(availPoints) <- sp::CRS(SRS_string = "EPSG:32647")
            # extract the habitat types each point is located within
            availValues <- raster::extract(landscapeRaster, availPoints)
            availValues[is.na(availValues)] <- 0
            
            suppressWarnings({
              usedValues <- raster::extract(landscapeRaster,
                                            sp::SpatialPoints(as.data.frame(movementData[,c("x", "y")]),
                                                              sp::CRS(SRS_string = "EPSG:32647")))
            })
            usedValues[is.na(usedValues)] <- 0
            
            if(land == "binary"){
              
              print("--- Available / Used start")
              availValues_DF <- data.frame(rbind(table(availValues)))
              names(availValues_DF) <- sub("X", "c", names(availValues_DF))
              
              usedValues_DF <- data.frame(rbind(table(usedValues)))
              names(usedValues_DF) <- sub("X", "c", names(usedValues_DF))
              
              print("-- Break 1")
              
              aClass <- names(availValues_DF)
              uClass <- names(usedValues_DF)
              
              if(length(aClass) > length(uClass)){
                toAdd <- as.data.frame(matrix(0, nrow = 1, ncol = length(aClass[!aClass %in% uClass])))
                names(toAdd) <- aClass[!aClass %in% uClass]
                usedValues_DF <- cbind(usedValues_DF, toAdd)
              } else if(length(uClass) > length(aClass)){
                toAdd <- as.data.frame(matrix(0, nrow = 1, ncol = length(uClass[!uClass %in% aClass])))
                names(toAdd) <- uClass[!uClass %in% aClass]
                availValues_DF <- cbind(availValues_DF, toAdd)
              }
              
              if(length(names(availValues_DF)) == 1){
                toAdd <- as.data.frame(matrix(0, nrow = 1, ncol = 1))
                names(toAdd) <- c("c0", "c1")[!c("c0", "c1") %in% names(availValues_DF)]
                availValues_DF <- cbind(availValues_DF, toAdd)
              }
              if(length(names(usedValues_DF)) == 1){
                toAdd <- as.data.frame(matrix(0, nrow = 1, ncol = 1))
                names(toAdd) <- c("c0", "c1")[!c("c0", "c1") %in% names(usedValues_DF)]
                usedValues_DF <- cbind(usedValues_DF, toAdd)
              }
              
              print("-- Break 2")
              
              usedValues_DF <- usedValues_DF[,sort(names(usedValues_DF))]
              availValues_DF <- availValues_DF[,sort(names(availValues_DF))]
              # availPopValues_DF <- availPopValues_DF[,sort(names(availPopValues_DF))]
              
              names(usedValues_DF) <- paste0("used_", names(usedValues_DF))
              names(availValues_DF) <- paste0("avail_", names(availValues_DF))
              # names(availPopValues_DF) <- paste0("avail_", names(availPopValues_DF))
              
              usedAvailable <- data.frame(usedValues_DF, availValues_DF)
              
            } else {
              
              usedAvailable <- data.frame(used_c0 = NA,
                                          used_c1 = NA,
                                          avail_c0 = NA,
                                          avail_c1 = NA)
              
              # usedAvailable <- list("Continous raster values - unable to compare",
              #                       "species" = allIndividualData$movementData_sf$species[1],
              #                       "hypothesis" = hypo)
            }
            
            usedAvailable$id <- movementData$id[1]
            usedAvailable$species <- movementData$species[1]
            usedAvailable$classLandscape <- land
            usedAvailable$hypothesis <- hypo
            usedAvailable$type <- "III"
            usedAvailable$analysis <- "compana"
            usedAvailable$method <- method
            usedAvailable$contour <- contour
            usedAvailable$availablePoints <- aPoints
            usedAvailable$samplingPattern <- spSamp
            
            print("-- Break 3")
            # ADD ANALYSIS TO BOTH SECTION compana vs rsf
            # COMBINE FULL DETAILS OF USED AND AVAIL HERE TO ONE SIDE, for RSF
            if(aPoints == maxAP){ 
              # the if statement will keep the largest avail points, need
              # subsampling in rsf section
              rsfUsedAvailable <- rbind(data.frame("case_" = TRUE,
                                                   "value" = usedValues),
                                        data.frame("case_" = FALSE,
                                                   "value" = availValues))
              rsfUsedAvailable$id <- movementData$id[1]
              rsfUsedAvailable$species <- movementData$species[1]
              rsfUsedAvailable$classLandscape <- land
              rsfUsedAvailable$hypothesis <- hypo
              rsfUsedAvailable$type <- "III"
              rsfUsedAvailable$analysis <- "rsf"
              rsfUsedAvailable$method <- method
              rsfUsedAvailable$contour <- contour
              rsfUsedAvailable$availablePoints <- aPoints
              rsfUsedAvailable$samplingPattern <- spSamp
              
              rsfUsedAvailList[[i]] <- rsfUsedAvailable
            }
            print("-- Break 4")
            
            print(usedAvailable)
            # print(usedAvailablePop)
            
            i <- i+1
            
            usedAvailList[[i]] <- usedAvailable
            
            
          } # indiID
        } # aPoints
      } # spSamp
    } # area contour
  } # area method
  # print(usedAvailList)
  print(("-- Break 5"))
  
  usedAvailFull <- do.call(bind_rows, usedAvailList)
  rsfUsedAvailFull <- do.call(bind_rows, rsfUsedAvailList)
  
  return(list("usedAvailFull" = usedAvailFull,
              "rsfUsedAvailFull" = rsfUsedAvailFull))
}
