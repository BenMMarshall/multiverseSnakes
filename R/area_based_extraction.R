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
  
  landscape <- rast(allIndividualData$habitatRasterLocation)
  landscapeRaster <- raster::raster(allIndividualData$habitatRasterLocation)
  land <- str_extract(allIndividualData$habitatRasterLocation, "binary|continuous")
  
  if(land == "continuous"){
    return(list("Continous raster values - unable to compare",
                "species" = allIndividualData$movementData_sf$species[1]))
  }
  
  # Loop to create individual polygons --------------------------------------
  
  i <- 0
  usedAvailList <- vector("list",
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
      # Loop to create TypeII polygon -------------------------------------------
      
      # popPolygon <- polygonList[[1]]
      # for(p in names(polygonList[-1])){
      #   popPolygon <- rgeos::gUnion(popPolygon, polygonList[[p]])
      # }
      # popPolygon
      # print("pop polygon complete")
      
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
            
            print("--- Available / Used start")
            availValues_DF <- data.frame(rbind(table(availValues)))
            names(availValues_DF) <- sub("X", "c", names(availValues_DF))
            
            suppressWarnings({
              usedValues <- raster::extract(landscapeRaster,
                                            sp::SpatialPoints(as.data.frame(movementData[,c("x", "y")]),
                                                              sp::CRS(SRS_string = "EPSG:32647")))
            })
            usedValues[is.na(usedValues)] <- 0
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
            ## TYPE II ##
            # suppressWarnings({
            #   availPopPoints <- sp::spsample(popPolygon,
            #                                  n = nrow(movementData) * aPoints,
            #                                  type = ifelse(spSamp == "rd", "random", "stratified"))
            # })
            # crs(availPopPoints) <- sp::CRS(SRS_string = "EPSG:32647")
            # 
            # availPopValues <- raster::extract(landscapeRaster, availPopPoints)
            # 
            # availPopValues[is.na(availPopValues)] <- 0
            # availPopValues_DF <- data.frame(rbind(table(availPopValues)))
            # names(availPopValues_DF) <- sub("X", "c", names(availPopValues_DF))
            # 
            # aPopClass <- names(availPopValues_DF)
            # if(length(aPopClass) > length(uClass)){
            #   toAdd <- as.data.frame(matrix(0, nrow = 1, ncol = length(aPopClass[!aPopClass %in% uClass])))
            #   names(toAdd) <- aPopClass[!aPopClass %in% uClass]
            #   usedValues_DF <- cbind(usedValues_DF, toAdd)
            # } else if(length(uClass) > length(aPopClass)){
            #   toAdd <- as.data.frame(matrix(0, nrow = 1, ncol = length(uClass[!uClass %in% aPopClass])))
            #   names(toAdd) <- uClass[!uClass %in% aPopClass]
            #   availPopValues_DF <- cbind(availPopValues_DF, toAdd)
            # }
            
            print("-- Break 3")
            
            usedValues_DF <- usedValues_DF[,sort(names(usedValues_DF))]
            availValues_DF <- availValues_DF[,sort(names(availValues_DF))]
            # availPopValues_DF <- availPopValues_DF[,sort(names(availPopValues_DF))]
            
            names(usedValues_DF) <- paste0("used_", names(usedValues_DF))
            names(availValues_DF) <- paste0("avail_", names(availValues_DF))
            # names(availPopValues_DF) <- paste0("avail_", names(availPopValues_DF))
            
            usedAvailable <- data.frame(usedValues_DF, availValues_DF)
            usedAvailable$id <- movementData$id[1]
            usedAvailable$species <- movementData$species[1]
            usedAvailable$classLandscape <- land
            usedAvailable$type <- "III"
            usedAvailable$method <- method
            usedAvailable$contour <- contour
            usedAvailable$availablePoints <- aPoints
            usedAvailable$samplingPattern <- spSamp
            
            usedAvailable
            
            # usedAvailablePop <- cbind(usedValues_DF, availPopValues_DF)
            # usedAvailablePop$id <- movementData$id[1]
            # usedAvailablePop$species <- movementData$species[1]
            # usedAvailablePop$classLandscape <- land
            # usedAvailablePop$type <- "II"
            # usedAvailablePop$method <- method
            # usedAvailablePop$contour <- contour
            # usedAvailablePop$availablePoints <- aPoints
            # usedAvailablePop$samplingPattern <- spSamp
            
            print("-- Break 4")
            
            print(usedAvailable)
            # print(usedAvailablePop)
            
            # usedAvailableAll <- rbind(usedAvailable, usedAvailablePop)
            usedAvailableAll <- usedAvailable
            
            i <- i+1
            
            usedAvailList[[i]] <- usedAvailableAll
            
          } # indiID
        } # aPoints
      } # spSamp
    } # area contour
  } # area method
  # print(usedAvailList)
  print(("-- Break 5"))
  
  usedAvailFull <- do.call(bind_rows, usedAvailList)
  
  return(usedAvailFull)
  }
