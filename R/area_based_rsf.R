#' Calculate the population results for the area based methods
#'
#' @name area_based_rsf
#' @description A
#' @param availUseData comes from area_based_extraction
#' @param sampleGroups list if IDs for the sample groups
#' @param optionsList options list that will be used to loop through options
#' @return Population estimates for area based methods.
#'
#' @export
area_based_rsf <- function(availUseData, optionsList, optionsListArea){
  
  # targets::tar_load("areaBasedAvailUse_BUFA_H1_binary")
  # availUseData <- areaBasedAvailUse_BUFA_H1_binary
  # targets::tar_load("areaBasedAvailUse_OPHA_H1_continuous")
  # availUseData <- areaBasedAvailUse_OPHA_H1_continuous
  availUseData <- availUseData$rsfUsedAvailFull
  
  # optionsList <- optionsList_area
  # optionsListArea <- optionsList_areaMethods
  
  optionsRandom <- optionsListArea$areaBasedRsfRandom
  
  optionsMethod <- unique(availUseData$method)
  optionsContour <- unique(availUseData$contour)
  optionsAPoints <- optionsList$Method_ap
  optionsSPSamp <- unique(availUseData$samplingPattern)
  optionsLand <- unique(availUseData$classLandscape)
  
  listLength <- 
    length(optionsMethod)*
    length(optionsContour)*
    length(optionsAPoints)*
    length(optionsSPSamp) *
    length(optionsRandom)
  
  rsfResultsList <- vector("list", length = listLength)
  i <- 0
  for(met in optionsMethod){
    # met <- optionsMethod[1]
    for(con in optionsContour){
      # con <- optionsContour[1]
      for(aPo in optionsAPoints){
        # aPo <- optionsAPoints[8]
        for(spS in optionsSPSamp){
          # spS <- optionsSPSamp[1]
          
          modelData <- availUseData %>% 
            dplyr::filter(method == met,
                          contour == con,
                          samplingPattern == spS) %>% 
            mutate(id = as.factor(id))
          
          usedData <- modelData %>% 
            filter(case_ == TRUE) 
          
          nDataPoint <- usedData %>% 
            group_by(id) %>% 
            summarise(n = n())
          
          availDataList <- vector("list", length = length(nDataPoint$id))
          names(availDataList) <- nDataPoint$id
          for(id in nDataPoint$id){
            availDataList[[id]] <- modelData %>% 
              filter(case_ == FALSE) %>% 
              slice_sample(n = nDataPoint$n[nDataPoint$id == id] * aPo)
          }
          availData <- do.call(rbind, availDataList)
          
          modelDataFiltered <- rbind(
            usedData, availData
          )
          
          if(availUseData$classLandscape[1] == "binary"){
            modelDataFiltered$value <- as.factor(modelDataFiltered$value)
          } else {
            modelDataFiltered$value <- as.numeric(scale(modelDataFiltered$value))
          }
          
          print("data prepared")
          
          for(ran in optionsRandom){
            
            if(ran == "slopes"){
              form <- case_ ~ value + (id|id)
            } else if(ran == "interOnly"){
              form <- case_ ~ value + (1|id)
            }
            
            # fit the model using base R glm()
            rsfOUT <- glmer(form,
                            family = binomial(),
                            data = modelDataFiltered,
                            nAGQ = 0) # nAGQ set to zero to speed up process
            print("model ran")
            
            if(class(rsfOUT)[1] == "try-error"){
              rsfDF <- data.frame(
                "Estimate" = NA,
                "SE" = NA,
                "zValue" = NA,
                "PrZ" = NA
              )
            } else {
              rsfDF <- as.data.frame(summary(rsfOUT)$coef)
              rsfDF <- rsfDF[2,]
              names(rsfDF) <- c("Estimate", "SE", "zValue", "PrZ")
            }
            rsfDF$species <- modelData$species[1]
            rsfDF$classLandscape <- modelData$classLandscape[1]
            rsfDF$hypothesis <- modelData$hypothesis[1]
            rsfDF$type <- modelData$type[1]
            rsfDF$analysis <- modelData$analysis[1]
            rsfDF$method <- met
            rsfDF$contour <- con
            rsfDF$availablePoints <- aPo
            rsfDF$samplingPattern <- spS
            
            i <- i+1
            rsfResultsList[[i]] <- rsfDF
            
          }
        }
      }
    }
  }
  return(do.call(rbind, rsfResultsList))
  
}