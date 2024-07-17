#' Run all brm models
#'
#' @name run_brms
#' @description A
#' @param resultsData output tar_combine resulting in ssfResults or areaResults
#' @return a
#'
#' @export
run_brms <- function(resultsData,
                     iter = 4000,
                     warmup = 750,
                     thin = 4){
  
  # targets::tar_load("ssfResults")
  # targets::tar_load("poisResults")
  # targets::tar_load("areaBasedResults")
  # targets::tar_load("twoStepResults")
  # targets::tar_load("rsfResults")
  # resultsData <- ssfResults
  # resultsData <- poisResults
  # resultsData <- areaBasedResults
  # resultsData <- twoStepResults
  # resultsData <- rsfResults
  # resultsData <- wrsfResults
  
  if(resultsData$analysis[1] == "ssf"){
    
    modelData <- resultsData %>% 
      dplyr::group_by(species, hypothesis, classLandscape) %>% 
      dplyr::mutate(medEst = median(modelAvg, na.rm = TRUE),
                    absDeltaEst = abs(modelAvg - medEst)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        availablePerStepScaled  = (availablePerStep - mean(availablePerStep))/sd(availablePerStep)
      )
    
    formAbsDelta <- brms::bf(
      absDeltaEst ~ 1 + 
        modelFormula + stepDist + turnDist + availablePerStepScaled + 
        (hypothesis|hypothesis) + (classLandscape|classLandscape) + (species|species)
    )
    
    brmpriors <- c(
      # data quantity decreases deviation from median effect
      brms::set_prior("cauchy(-0.1, 3)", coef = "availablePerStepScaled"),
      # other kept as weak positive priors
      brms::set_prior("cauchy(0.1, 3)", coef = "modelFormulamf.ss"),
      brms::set_prior("cauchy(0.1, 3)", coef = "stepDistgamma"),
      brms::set_prior("cauchy(0.1, 3)", coef = "turnDistvonmises")
    )
    
    modelSave <- here::here("modelOutput",
                            "absDelta_ssf.txt")
    modelFile <- here::here("modelOutput",
                            "absDelta_ssf")
    
  } else if(resultsData$analysis[1] == "Poisson"){
    
    modelData <- resultsData %>% 
      dplyr::group_by(species, hypothesis, classLandscape) %>% 
      dplyr::mutate(medEst = median(estimateDiff, na.rm = TRUE),
                    absDeltaEst = abs(estimateDiff - medEst)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        availablePerStepScaled  = (availablePerStep - mean(availablePerStep))/sd(availablePerStep)
      )
    
    formAbsDelta <- brms::bf(
      absDeltaEst ~ 1 + 
        modelFormula + stepDist + turnDist + availablePerStepScaled +
        (hypothesis|hypothesis) + (classLandscape|classLandscape) + (species|species)
    )
    
    brmpriors <- c(
      # data quantity decreases deviation from median effect
      brms::set_prior("cauchy(-0.1, 3)", coef = "availablePerStepScaled"),
      # other kept as weak positive priors
      brms::set_prior("cauchy(0.1, 3)", coef = "modelFormulamf.ss"),
      brms::set_prior("cauchy(0.1, 3)", coef = "stepDistgamma"),
      brms::set_prior("cauchy(0.1, 3)", coef = "turnDistvonmises")
    )
    
    modelSave <- here::here("modelOutput",
                            "absDelta_pois.txt")
    modelFile <- here::here("modelOutput",
                            "absDelta_pois")
    
  } else if("companaHabDiff" %in% names(resultsData)){
    
    modelData <- resultsData %>% 
      dplyr::filter(classLandscape == "binary") %>% 
      dplyr::group_by(species, hypothesis, classLandscape) %>% 
      dplyr::mutate(medEst = median(companaHabDiff, na.rm = TRUE),
                    absDeltaEst = abs(companaHabDiff - medEst)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        contourScaled = (contour - mean(contour))/sd(contour),
        availablePointsScaled  = (availablePoints - mean(availablePoints))/sd(availablePoints)
      )
    
    formAbsDelta <- brms::bf(
      absDeltaEst ~ 1 + 
        areaMethod + contourScaled +
        availablePointsScaled + samplingPattern + test +
        (hypothesis|hypothesis) + (species|species)
    )
    
    brmpriors <- c(
      # data quantity decreases deviation from median effect
      brms::set_prior("cauchy(-0.1, 3)", coef = "availablePointsScaled"),
      # other kept as weak positive priors
      brms::set_prior("cauchy(0.1, 3)", coef = "contourScaled"),
      brms::set_prior("cauchy(0.1, 3)", coef = "samplingPatternst"),
      brms::set_prior("cauchy(0.1, 3)", coef = "testrandomisation"),
      brms::set_prior("cauchy(0.1, 3)", coef = "areaMethodMCP")
    )
    
    modelSave <- here::here("modelOutput",
                            "absDelta_areaBased.txt")
    modelFile <- here::here("modelOutput",
                            "absDelta_areaBased")
    
  } else if(resultsData$analysis[1] == "TwoStep"){
    
    modelData <- resultsData %>% 
      dplyr::group_by(species, hypothesis, classLandscape) %>% 
      dplyr::mutate(medEst = median(twoStepBeta, na.rm = TRUE),
                    absDeltaEst = abs(twoStepBeta - medEst)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        availablePerStepScaled  = (availablePerStep - mean(availablePerStep))/sd(availablePerStep)
      )
    
    formAbsDelta <- brms::bf(
      absDeltaEst ~ 1 + 
        modelFormula + stepDist + turnDist + availablePerStepScaled +
        (hypothesis|hypothesis) + (classLandscape|classLandscape) + (species|species)
    )
    
    brmpriors <- c(
      # data quantity decreases deviation from median effect
      brms::set_prior("cauchy(-0.1, 3)", coef = "availablePerStepScaled"),
      # other kept as weak positive priors
      brms::set_prior("cauchy(0.1, 3)", coef = "modelFormulamf.ss"),
      brms::set_prior("cauchy(0.1, 3)", coef = "stepDistgamma"),
      brms::set_prior("cauchy(0.1, 3)", coef = "turnDistvonmises")
    )
    
    modelSave <- here::here("modelOutput",
                            "absDelta_twoStep.txt")
    modelFile <- here::here("modelOutput",
                            "absDelta_twoStep")
    
  } else if(resultsData$analysis[1] == "rsf"){
    
    modelData <- resultsData %>% 
      dplyr::group_by(species, hypothesis, classLandscape) %>% 
      dplyr::mutate(medEst = median(Estimate, na.rm = TRUE),
                    absDeltaEst = abs(Estimate - medEst)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        contourScaled = (contour - mean(contour))/sd(contour),
        availablePointsScaled  = (availablePoints - mean(availablePoints))/sd(availablePoints)
      )
    
    formAbsDelta <- brms::bf(
      absDeltaEst ~ 1 + 
        method + contourScaled +
        availablePointsScaled + samplingPattern + 
        (hypothesis|hypothesis) + (classLandscape|classLandscape) + (species|species)
    )
    
    brmpriors <- c(
      # data quantity decreases deviation from median effect
      brms::set_prior("cauchy(-0.1, 3)", coef = "availablePointsScaled"),
      # other kept as weak positive priors
      brms::set_prior("cauchy(0.1, 3)", coef = "contourScaled"),
      brms::set_prior("cauchy(0.1, 3)", coef = "samplingPatternst"),
      brms::set_prior("cauchy(0.1, 3)", coef = "methodMCP")
    )
    
    modelSave <- here::here("modelOutput",
                            "absDelta_rsf.txt")
    modelFile <- here::here("modelOutput",
                            "absDelta_rsf")
    
  }
  
  modOUT_absDelta <- brms::brm(formula = formAbsDelta,
                               data = modelData,
                               family = gaussian,
                               prior = brmpriors,
                               warmup = warmup, iter = iter, chains = 4,
                               cores = 4,
                               thin = thin,
                               # control = list(adapt_delta = 0.90,
                               #                max_treedepth = 15),
                               seed = 1,
                               save_pars = brms::save_pars(all = TRUE),
                               save_model = modelSave,
                               file = modelFile
  )
  
  return(modOUT_absDelta)
  
}