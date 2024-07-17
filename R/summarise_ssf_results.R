#' Summarise individual SSF models
#'
#' @name summarise_ssf_results
#' @description create a population estimate
#' @param ssfResults all sff models and corresponding options data.frame
#' @param sampleGroups list if IDs for the sample groups
#' @return Population estimates for SSF based methods.
#'
#' @export
summarise_ssf_results <- function(allIndividualData, sampleGroups, optionsList){
  # targets::tar_load("movementData_OPHA_H1_binary")
  # targets::tar_load("movementData_BUCA_H1_continuous")
  # allIndividualData <- movementData_OPHA_H1_binary
  # allIndividualData <- movementData_BUCA_H1_continuous
  # optionsList <- optionsList_sff
  
  landscape <- rast(allIndividualData$habitatRasterLocation)
  land <- str_extract(allIndividualData$habitatRasterLocation, "binary|continuous")
  hypo <- str_extract(allIndividualData$habitatRasterLocation, "H1|H2")
  
  optionsForm <- optionsList$MethodSSF_mf
  optionsASteps <- optionsList$MethodSSF_as
  optionsStepD <- optionsList$MethodSSF_sd
  optionsTurnD <- optionsList$MethodSSF_td
  
  listLength <- length(optionsForm) *
    length(optionsASteps) *
    length(optionsStepD) *
    length(optionsTurnD)
  
  ssfSummarisedList <- vector("list", length = listLength)
  i <- 0
  for(mf in optionsForm){
    # mf <- optionsForm[1]
    for(step in optionsStepD){
      # step <- optionsStepD[1]
      for(turn in optionsTurnD){
        # turn <- optionsTurnD[1]
        for(as in optionsASteps){
          # as <- optionsASteps[1]
          
          optionsList_sffTemp <- list(
            Method_method = c("ssf"),
            Method_land = land,
            MethodSSF_as = as,
            MethodSSF_mf = mf,
            MethodSSF_sd = step,
            MethodSSF_td = turn
          )
          
          # we need to pass the landscape data into the function with the sampledIndidata
          ssfResultsIndividuals <- wrapper_indi_ssf(allIndividualData = allIndividualData,
                                                    optionsList = optionsList_sffTemp)
          
          sampleModelList <- lapply(ssfResultsIndividuals, function(x){
            x[[1]]$model
          })
          sampleEstList_sample <- lapply(ssfResultsIndividuals, function(x){
            x[[1]]$options
          })
          sampleEst_sample <- do.call(rbind, sampleEstList_sample)
          
          print(all(sampleEst_sample$modelFormula == mf,
                    sampleEst_sample$classLandscape == land,
                    sampleEst_sample$stepDist == step,
                    sampleEst_sample$turnDist == turn,
                    sampleEst_sample$availablePerStep == as))
          print(paste(land,
                      mf,
                      step,
                      turn,
                      as, sep = " - "))
          
          if(any(sampleEst_sample$Estimate[!is.na(sampleEst_sample$Estimate)] == "SingleHabitat")){
            singleHabIssues <- paste0(sampleEst_sample$id[!is.na(sampleEst_sample$Estimate) &
                                                            sampleEst_sample$Estimate == "SingleHabitat"],
                                      collapse = ";")
          } else {
            singleHabIssues <- NA
          }
          # calculate CI surrounding the naive mean
          sampleEst_sample$Estimate <- as.numeric(sampleEst_sample$Estimate)
          
          nEst <- length(sampleEst_sample$Estimate)
          meanEst <- mean(sampleEst_sample$Estimate, na.rm = TRUE)
          sdEst <- sd(sampleEst_sample$Estimate, na.rm = TRUE)
          marginEst <- qt(0.975, df = nEst - 1) * sdEst/sqrt(nEst)
          lowEst <- meanEst - marginEst
          highEst <- meanEst + marginEst
          
          # modelAvgOUT <- MuMIn::model.avg(sampleModelList)
          # # need an try here as confint can fail, in those cases return NA
          # modelAvgOUTCI <- try(
          #   confint(modelAvgOUT)
          # )
          # if(class(modelAvgOUTCI)[1] == "try-error"){
          #   modelAvgOUTCI <- matrix(NA, 2, 2)
          # }
          # 
          # modelAvgData <- data.frame(
          #   sampleID = sampID,
          #   species = speciesCodeLetter,
          #   sampleSize = length(IDs),
          #   trackFreq = sampleEst_sample$trackFreq[1],
          #   trackDura = sampleEst_sample$trackDura[1],
          #   analysis = sampleEst_sample$analysis[1],
          #   classLandscape = land,
          #   modelFormula = sampleEst_sample$modelFormula[1],
          #   stepDist = sampleEst_sample$stepDist[1],
          #   turnDist = sampleEst_sample$turnDist[1],
          #   availablePerStep = sampleEst_sample$availablePerStep[1],
          #   averagingMethod = "Model average",
          #   modelAvg = modelAvgOUT$coefficients[1,1],
          #   modelAvgSE = summary(modelAvgOUT)$coefmat.full[1,2],
          #   modelAvgLower = modelAvgOUTCI[1,1],
          #   modelAvgUpper = modelAvgOUTCI[1,2]
          # )
          
          naiveAvgData <- data.frame(
            species = allIndividualData$movementData_sf$species[1],
            analysis = sampleEst_sample$analysis[1],
            classLandscape = land,
            hypothesis = hypo,
            modelFormula = sampleEst_sample$modelFormula[1],
            stepDist = sampleEst_sample$stepDist[1],
            turnDist = sampleEst_sample$turnDist[1],
            availablePerStep = sampleEst_sample$availablePerStep[1],
            averagingMethod = "Naive average",
            modelAvg = meanEst,
            modelAvgSE = sdEst/sqrt(length(sampleEst_sample$Estimate)),
            modelAvgLower = lowEst,
            modelAvgUpper = highEst,
            singleHabIssues
          )
          
          # ssfSampleOUT <- rbind(modelAvgData, naiveAvgData)
          ssfOUT <- naiveAvgData
          
          i <- i+1
          # ssfSampledList[[paste0(regime, "_", sampID)]] <- ssfSampleOUT
          ssfSummarisedList[[i]] <- ssfOUT
          
        }
      }
    }
    
  }
  
  return(do.call(rbind, ssfSummarisedList))
  
}