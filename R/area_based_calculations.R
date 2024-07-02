#' Calculate the population results for the area based methods
#'
#' @name area_based_calculations
#' @description A
#' @param availUseData comes from area_based_extraction
#' @param sampleGroups list if IDs for the sample groups
#' @param optionsList options list that will be used to loop through options
#' @return Population estimates for area based methods.
#'
#' @export
area_based_calculations <- function(availUseData, optionsList, optionsListArea){
  
  # targets::tar_load("areaBasedAvailUse_OPHA_H1_continuous")
  # availUseData <- areaBasedAvailUse_OPHA_H1_continuous
  # optionsList <- optionsList_area
  # optionsListArea <- optionsList_areaMethods
  if(class(availUseData) == "list"){
    companaResultsDF <- data.frame(
      species = availUseData$species[1],
      classLandscape = "continous",
      hypothesis = availUseData$hypothesis[1],
      analysis = "compana",
      type = NA,
      areaMethod = NA,
      contour = NA,
      availablePoints = NA,
      samplingPattern = NA,
      test = NA,
      companaHabDiff = NA,
      companaLambda = NA,
      companaP = NA
    )
    return(companaResultsDF)
  }
  
  # availUseData <- areaBasedAvailUse_BUFA_H1_binary
  optionsType <- unique(availUseData$type)
  optionsMethod <- unique(availUseData$method)
  optionsContour <- unique(availUseData$contour)
  optionsAPoints <- unique(availUseData$availablePoints)
  optionsSPSamp <- unique(availUseData$samplingPattern)
  optionsLand <- unique(availUseData$classLandscape)
  
  # optionsList$areaBasedMethod
  # optionsListArea <- optionsList_areaMethods
  optionsTest <- optionsListArea$areaBasedTest
  
  listLength <- length(optionsType)*
    length(optionsMethod)*
    length(optionsContour)*
    length(optionsAPoints)*
    length(optionsSPSamp)*
    length(optionsTest)
  
  companaResultsList <- vector("list", length = listLength)
  i <- 0
  for(typ in optionsType){
    # typ <- optionsType[1]
    for(met in optionsMethod){
      # met <- optionsMethod[1]
      for(con in optionsContour){
        # con <- optionsContour[1]
        for(aPo in optionsAPoints){
          # aPo <- optionsAPoints[1]
          for(spS in optionsSPSamp){
            # spS <- optionsSPSamp[1]
                
                use <- availUseData %>% 
                  dplyr::filter(type == typ,
                                method == met,
                                contour == con,
                                availablePoints == aPo,
                                samplingPattern == spS) %>% 
                  dplyr::select(used_c0, used_c1)
                
                avail <- availUseData %>% 
                  dplyr::filter(type == typ,
                                method == met,
                                contour == con,
                                availablePoints == aPo,
                                samplingPattern == spS) %>% 
                  dplyr::select(avail_c0, avail_c1)
                
                names(use) <- c("c0", "c1")
                names(avail) <- c("c0", "c1")
                
                for(tes in optionsTest){
                  # tes <- optionsTest[1]
                  companaOUT <- compana(used = use, avail = avail,
                                        test = tes)
                  
                  # names(companaOUT$rank[1])
                  # companaOUT$rm
                  # companaOUT$rm
                  # companaOUT$rmv
                  
                  companaResultsDF <- data.frame(
                    species = availUseData$species[1],
                    classLandscape = availUseData$classLandscape[1],
                    hypothesis = availUseData$hypothesis[1],
                    analysis = "compana",
                    type = typ,
                    areaMethod = met,
                    contour = con,
                    availablePoints = aPo,
                    samplingPattern = spS,
                    test = tes,
                    companaHabDiff = companaOUT$rmv[2,1],
                    companaLambda = companaOUT$test["Lambda"],
                    companaP = companaOUT$test["P"]
                  )
                  
                  i <- i+1
                  
                  companaResultsList[[i]] <- companaResultsDF
                  
                } # tes
                
          }
        }
      }
    }
  }
  
  return(do.call(rbind, companaResultsList))
  
}