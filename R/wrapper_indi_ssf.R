#' Run all ssf options
#'
#' @name wrapper_indi_ssf
#' @description A
#' @param movementData must have a x and y column for locations, and a datetime
#'   column for timestamps ("%Y-%m-%d %H:%M:%S")
#' @param landscape
#' @param optionsList Must have the following items: Method_method,
#'   MethodSSF_mf, MethodSSF_sd, MethodSSF_td, MethodSSF_as
#' @return a
#'
#' @export
wrapper_indi_ssf <- function(
    allIndividualData,
    # landscape,
    optionsList
){
  # targets::tar_load("movementData_BUFA_H1_continuous")
  # targets::tar_load("movementData_OPHA_H1_binary")
  # allIndividualData <- movementData_BUFA_H1_continuous
  # allIndividualData <- movementData_OPHA_H1_binary
  # optionsList <- optionsList_sff
  Method_method <- optionsList$Method_method
  MethodSSF_land <- optionsList$Method_land
  MethodSSF_mf <- optionsList$MethodSSF_mf
  MethodSSF_sd <- optionsList$MethodSSF_sd
  MethodSSF_td <- optionsList$MethodSSF_td
  MethodSSF_as <- optionsList$MethodSSF_as
  
  landscape <- rast(allIndividualData$habitatRasterLocation)
  land <- str_extract(allIndividualData$habitatRasterLocation, "binary|continuous")
  
  allIndividualData <- split(allIndividualData$movementData_sf, allIndividualData$movementData_sf$id)
  indiSSFResults <- vector("list", length = length(names(allIndividualData)))
  names(indiSSFResults) <- names(allIndividualData)
  for(indiID in names(allIndividualData)){
    
    # indiID <- names(allIndividualData)[2]
    print(indiID)
    movementData <- allIndividualData[[indiID]]
    
    # ssf places
    listSize <- length(MethodSSF_mf) *
      length(MethodSSF_land) *
      length(MethodSSF_sd) *
      length(MethodSSF_td) *
      length(MethodSSF_as)
    
    listOUT <- vector("list",
                      length = listSize)
    i <- 0
    for(mf in MethodSSF_mf){
      # mf <- MethodSSF_mf[1]
      for(stepD in MethodSSF_sd){
        # stepD <- MethodSSF_sd[1]
        for(turnD in MethodSSF_td){
          # turnD <- MethodSSF_td[1]
          for(as in MethodSSF_as){
            # as <- MethodSSF_as[1]
            # for(land in MethodSSF_land){
            
            ssfOUT <- method_indi_ssf(
              movementData = movementData,
              landscape = landscape,
              methodForm = mf,
              stepDist = stepD,
              turnDist = turnD,
              availableSteps = as
            )
            
            if(ssfOUT$model[1] == "Only one habitat used"){
              optionsData <- data.frame(
                id = movementData$id[1],
                Estimate = "SingleHabitat",
                Lower = NA,
                Upper = NA,
                analysis = "ssf",
                classLandscape = land,
                modelFormula = mf,
                stepDist = stepD,
                turnDist = turnD,
                availablePerStep = as
              )
            } else {
              
              
              ssfDF <- as.data.frame(summary(ssfOUT)$coef)
              method <- rep("ssf", nrow(ssfDF))
              ssfDF <- cbind(ssfDF, method)
              if(any(rownames(ssfDF) %in% "valuesc1")){
                ssfEst <- data.frame(ssfDF[rownames(ssfDF) == "valuesc1",][1],
                                     ssfDF[rownames(ssfDF) == "valuesc1",][3],
                                     "ssf")
                names(ssfEst) <- c("Estimate", "SE", "Method")
              } else if(any(rownames(ssfDF) %in% "values")){
                ssfEst <- data.frame(ssfDF[rownames(ssfDF) == "values",][1],
                                     ssfDF[rownames(ssfDF) == "values",][3],
                                     "ssf")
                names(ssfEst) <- c("Estimate", "SE", "Method")
              } else {
                ssfEst <- data.frame(NA,
                                     NA,
                                     "ssf")
                names(ssfEst) <- c("Estimate", "SE", "Method")
              }
              
              optionsData <- data.frame(
                id = movementData$id[1],
                Estimate = ssfEst$Estimate,
                Lower = ssfEst$Estimate - ssfEst$SE,
                Upper = ssfEst$Estimate + ssfEst$SE,
                analysis = "ssf",
                classLandscape = land,
                modelFormula = mf,
                stepDist = stepD,
                turnDist = turnD,
                availablePerStep = as
              )
            }
            
            ssfOUT$options <- optionsData
            
            i <- i+1
            listOUT[[i]] <- ssfOUT
            # print(i)
            
            # } #landscape
          } # as
        } # stepD
      } # turnD
    } # mf
    indiSSFResults[[indiID]] <- do.call(list, listOUT)
  }
  
  # allIndiSSFResults <- do.call(list, indiSSFResults)
  
  # return(allIndiSSFResults)
  return(indiSSFResults)
}
