#' Run all brm models
#'
#' @name run_brms
#' @description A
#' @param resultsData output tar_combine resulting in ssfResults or areaResults
#' @return a
#'
#' @export
run_brms <- function(resultsData,
                     modelName,
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
  
  # modelName <- "OPHA_H2_binary"
  
  modelOptions <- str_split(modelName, "_")[[1]]
  names(modelOptions) <- c("species", "hypo", "landscape")
  
  resultsData <- resultsData %>% 
    filter(species == case_when(
      modelOptions["species"] == "OPHA" ~ "Ophiophagus hannah",
      modelOptions["species"] == "PYBI" ~ "Python bivittatus",
      modelOptions["species"] == "BUCA" ~ "Bungarus candidus",
      modelOptions["species"] == "BUFA" ~ "Bungarus fasciatus"
    ),
    hypothesis == modelOptions["hypo"],
    classLandscape == modelOptions["landscape"])
  
  if(resultsData$analysis[1] == "ssf"){
    
    modelData <- resultsData %>% 
      mutate("estimate" = modelAvg,
             "se" = modelAvgSE) %>% 
      dplyr::mutate(
        availablePerStepScaled  = (availablePerStep - mean(availablePerStep))/sd(availablePerStep)
      )
    
    formMeta <- brms::bf(
      estimate|se(se) ~ 1 + 
        modelFormula + stepDist + turnDist + availablePerStepScaled
    )
    
    brmpriors <- c(
      brms::set_prior("normal(0,2)", class = "Intercept"),
      # data quantity decreases deviation from median effect
      brms::set_prior("cauchy(-0.1, 3)", coef = "availablePerStepScaled"),
      # other kept as weak positive priors
      brms::set_prior("cauchy(0.1, 3)", coef = "modelFormulamf.ss"),
      brms::set_prior("cauchy(0.1, 3)", coef = "stepDistgamma"),
      brms::set_prior("cauchy(0.1, 3)", coef = "turnDistvonmises")
    )
    
    modelSave <- here::here("modelOutput",
                            paste0("meta_", modelName, "_ssf.txt"))
    modelFile <- here::here("modelOutput",
                            paste0("meta_", modelName, "_ssf"))
    
  } else if(resultsData$analysis[1] == "Poisson"){
    
    modelData <- resultsData %>% 
      mutate("estimate" = estimateDiff,
             "se" = sd) %>% 
      dplyr::mutate(
        availablePerStepScaled  = (availablePerStep - mean(availablePerStep))/sd(availablePerStep)
      )
    
    formMeta <- brms::bf(
      estimate|se(se) ~ 1 + 
        modelFormula + stepDist + turnDist + availablePerStepScaled
    )
    
    brmpriors <- c(
      brms::set_prior("normal(0,2)", class = "Intercept"),
      # data quantity decreases deviation from median effect
      brms::set_prior("cauchy(-0.1, 3)", coef = "availablePerStepScaled"),
      # other kept as weak positive priors
      brms::set_prior("cauchy(0.1, 3)", coef = "modelFormulamf.ss"),
      brms::set_prior("cauchy(0.1, 3)", coef = "stepDistgamma"),
      brms::set_prior("cauchy(0.1, 3)", coef = "turnDistvonmises")
    )
    
    modelSave <- here::here("modelOutput",
                            paste0("meta_", modelName, "_pois.txt"))
    modelFile <- here::here("modelOutput",
                            paste0("meta_", modelName, "_pois"))
    
  } else if("companaHabDiff" %in% names(resultsData)){
    
    modelData <- resultsData %>% 
      dplyr::filter(classLandscape == "binary") %>% 
      mutate("estimate" = companaHabDiff) %>% 
      dplyr::mutate(
        contourScaled = (contour - mean(contour))/sd(contour),
        availablePointsScaled  = (availablePoints - mean(availablePoints))/sd(availablePoints)
      )
    
    formMeta <- brms::bf(
      estimate ~ 1 + 
        areaMethod + contourScaled +
        availablePointsScaled + samplingPattern + test
    )
    
    brmpriors <- c(
      brms::set_prior("normal(0,2)", class = "Intercept"),
      # data quantity decreases deviation from median effect
      brms::set_prior("cauchy(-0.1, 3)", coef = "availablePointsScaled"),
      # other kept as weak positive priors
      brms::set_prior("cauchy(0.1, 3)", coef = "contourScaled"),
      brms::set_prior("cauchy(0.1, 3)", coef = "samplingPatternst"),
      brms::set_prior("cauchy(0.1, 3)", coef = "testrandomisation"),
      brms::set_prior("cauchy(0.1, 3)", coef = "areaMethodMCP")
    )
    
    modelSave <- here::here("modelOutput",
                            paste0("meta_", modelName, "_areaBased.txt"))
    modelFile <- here::here("modelOutput",
                            paste0("meta_", modelName, "_areaBased"))
    
  } else if(resultsData$analysis[1] == "TwoStep"){
    
    modelData <- resultsData %>% 
      dplyr::mutate("estimate" = twoStepBeta,
                    "se" = twoStepSE) %>% 
      dplyr::mutate(
        availablePerStepScaled  = (availablePerStep - mean(availablePerStep))/sd(availablePerStep)
      )
    
    if(all(is.na(modelData$estimate[modelData$modelFormula == "mf.is"]))){
      formMeta <- brms::bf(
        estimate|se(se) ~ 1 + 
          stepDist + turnDist + availablePerStepScaled
      )
      
      brmpriors <- c(
        brms::set_prior("normal(0,2)", class = "Intercept"),
        # data quantity decreases deviation from median effect
        brms::set_prior("cauchy(-0.1, 3)", coef = "availablePerStepScaled"),
        # other kept as weak positive priors
        brms::set_prior("cauchy(0.1, 3)", coef = "stepDistgamma"),
        brms::set_prior("cauchy(0.1, 3)", coef = "turnDistvonmises")
      )
    } else {
      formMeta <- brms::bf(
        estimate|se(se) ~ 1 + 
          modelFormula + stepDist + turnDist + availablePerStepScaled
      )
      
      brmpriors <- c(
        brms::set_prior("normal(0,2)", class = "Intercept"),
        # data quantity decreases deviation from median effect
        brms::set_prior("cauchy(-0.1, 3)", coef = "availablePerStepScaled"),
        # other kept as weak positive priors
        brms::set_prior("cauchy(0.1, 3)", coef = "modelFormulamf.ss"),
        brms::set_prior("cauchy(0.1, 3)", coef = "stepDistgamma"),
        brms::set_prior("cauchy(0.1, 3)", coef = "turnDistvonmises")
      )
    }
    
    
    modelSave <- here::here("modelOutput",
                            paste0("meta_", modelName, "_twostep.txt"))
    modelFile <- here::here("modelOutput",
                            paste0("meta_", modelName, "_twostep"))
    
  } else if(resultsData$analysis[1] == "rsf"){
    
    modelData <- resultsData %>% 
      dplyr::mutate("estimate" = Estimate,
                    "se" = SE) %>% 
      dplyr::mutate(
        contourScaled = (contour - mean(contour))/sd(contour),
        availablePointsScaled  = (availablePoints - mean(availablePoints))/sd(availablePoints)
      )
    
    formMeta <- brms::bf(
      estimate|se(se) ~ 1 + 
        method + contourScaled +
        availablePointsScaled + samplingPattern
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
                            paste0("meta_", modelName, "_rsf.txt"))
    modelFile <- here::here("modelOutput",
                            paste0("meta_", modelName, "_rsf"))
    
  }
  
  modOUT_meta <- brms::brm(formula = formMeta,
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
  
  modOUT_meta$modelName <- modelName
  
  return(modOUT_meta)
  
}