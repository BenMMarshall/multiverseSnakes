#' Generate summary tables and plots
#'
#' @name compiled_overall_summary
#' @description A
#' @param modelExtracts target of same name
#' @param ssfResults target of same name
#' @param poisResults target of same name
#' @param areaBasedResults target of same name
#' @param twoStepResults target of same name
#' @param rsfResults target of same name
#' @return a
#'
#' @export
compiled_overall_summary <- function(modelExtracts,
                                     ssfResults,
                                     poisResults,
                                     areaBasedResults,
                                     twoStepResults,
                                     rsfResults){
  # library(dplyr)
  # library(stringr)
  
  # targets::tar_load("modelExtracts")
  # targets::tar_load("ssfResults")
  # targets::tar_load("poisResults")
  # targets::tar_load("areaBasedResults")
  # targets::tar_load("twoStepResults")
  # targets::tar_load("rsfResults")
  
  metaBetas <- modelExtracts$betasOutputs
  metaR2 <- modelExtracts$r2Outputs
  
  # ssf ---------------------------------------------------------------------
  
  ssfResults <- ssfResults %>% 
    mutate("estimate" = modelAvg,
           "se" = modelAvgSE) %>% 
    mutate(hypoSupportSig = case_when(
      modelAvgLower > 0 & modelAvgUpper > 0 & species == "Ophiophagus hannah" ~ "Significant Support OPHA",
      modelAvgLower > 0 & modelAvgUpper > 0 & species == "Python bivittatus" ~ "Significant Support PYBI",
      modelAvgLower > 0 & modelAvgUpper > 0 & species == "Bungarus candidus" ~ "Significant Support BUCA",
      modelAvgLower > 0 & modelAvgUpper > 0 & species == "Bungarus fasciatus" ~ "Significant Support BUFA",
      TRUE ~ "No Support"
    ),
    lower = modelAvgLower,
    upper = modelAvgUpper) %>%  
    dplyr::select(-modelAvg, -modelAvgSE, -modelAvgLower, -modelAvgUpper,
                  -averagingMethod, -singleHabIssues)
  
  ssfMetaBetas <- metaBetas %>% 
    mutate(method = str_extract(model, "ssf|rsf|pois|twoStep|areaBased"),
           species = str_extract(model, "OPHA|PYBI|BUCA|BUFA"),
           hypothesis = str_extract(model, "H1|H2"),
           classLandscape = str_extract(model, "binary|continuous")) %>% 
    mutate(species = case_when(
      species == "OPHA" ~ "Ophiophagus hannah",
      species == "PYBI" ~ "Python bivittatus",
      species == "BUCA" ~ "Bungarus candidus",
      species == "BUFA" ~ "Bungarus fasciatus"
    )) %>% 
    filter(method == "ssf") %>% 
    filter(.variable == "b_Intercept") %>% 
    select(-.width, -.variable)
  
  ssf_summary <- ssfResults %>% 
    group_by(species, hypothesis, classLandscape) %>% 
    summarise(
      hypoDirection = sum(estimate > 0, na.rm = TRUE),
      hypoSupportSigCount = sum(!hypoSupportSig == "No Support"),
      totalEstimates = n()
    ) %>% 
    left_join(ssfMetaBetas)
  
  
  # areaBased ---------------------------------------------------------------
  
  areaBasedResults <- areaBasedResults %>% 
    mutate("estimate" = companaHabDiff,
           "se" = companaP) %>% 
    mutate(hypoSupportSig = case_when(
      se < 0.05 & species == "Ophiophagus hannah" ~ "Significant Support OPHA",
      se < 0.05 & species == "Python bivittatus" ~ "Significant Support PYBI",
      se < 0.05 & species == "Bungarus candidus" ~ "Significant Support BUCA",
      se < 0.05 & species == "Bungarus fasciatus" ~ "Significant Support BUFA",
      TRUE ~ "No Support"
    ), lower = estimate - 2*se,
    upper = estimate + 2*se) %>%  
    dplyr::select(-companaHabDiff, -companaLambda, -companaP, -type)
  
  areaBasedMetaBetas <- metaBetas %>% 
    mutate(method = str_extract(model, "ssf|rsf|pois|twoStep|areaBased"),
           species = str_extract(model, "OPHA|PYBI|BUCA|BUFA"),
           hypothesis = str_extract(model, "H1|H2"),
           classLandscape = str_extract(model, "binary|continuous")) %>% 
    mutate(species = case_when(
      species == "OPHA" ~ "Ophiophagus hannah",
      species == "PYBI" ~ "Python bivittatus",
      species == "BUCA" ~ "Bungarus candidus",
      species == "BUFA" ~ "Bungarus fasciatus"
    )) %>% 
    filter(method == "areaBased") %>% 
    filter(.variable == "b_Intercept") %>% 
    select(-.width, -.variable)
  
  areaBased_summary <- areaBasedResults %>% 
    filter(classLandscape == "binary") %>% 
    group_by(species, hypothesis, classLandscape) %>% 
    summarise(
      hypoDirection = sum(estimate > 0, na.rm = TRUE),
      hypoSupportSigCount = sum(!hypoSupportSig == "No Support"),
      totalEstimates = n()
    ) %>% 
    left_join(areaBasedMetaBetas)
  
  # pois --------------------------------------------------------------------
  
  poisResults <- poisResults %>% 
    mutate("estimate" = estimateDiff,
           "se" = sd) %>% 
    mutate(hypoSupportSig = case_when(
      (estimate - 2*se) > 0 & species == "Ophiophagus hannah" ~ "Significant Support OPHA",
      (estimate - 2*se) > 0 & species == "Python bivittatus" ~ "Significant Support PYBI",
      (estimate - 2*se) > 0 & species == "Bungarus candidus" ~ "Significant Support BUCA",
      (estimate - 2*se) > 0 & species == "Bungarus fasciatus" ~ "Significant Support BUFA",
      TRUE ~ "No Support"
    ), lower = estimate - 2*se,
    upper = estimate + 2*se) %>%  
    dplyr::select(-mean, -sd, -q025, -q50, -q975, -mode, -kld, -estimateDiff,
                  -mmarginal, -emarginal, -term)
  
  poisMetaBetas <- metaBetas %>% 
    mutate(method = str_extract(model, "ssf|rsf|pois|twoStep|areaBased"),
           species = str_extract(model, "OPHA|PYBI|BUCA|BUFA"),
           hypothesis = str_extract(model, "H1|H2"),
           classLandscape = str_extract(model, "binary|continuous")) %>% 
    mutate(species = case_when(
      species == "OPHA" ~ "Ophiophagus hannah",
      species == "PYBI" ~ "Python bivittatus",
      species == "BUCA" ~ "Bungarus candidus",
      species == "BUFA" ~ "Bungarus fasciatus"
    )) %>% 
    filter(method == "pois") %>% 
    filter(.variable == "b_Intercept") %>% 
    select(-.width, -.variable)
  
  pois_summary <- poisResults %>% 
    group_by(species, hypothesis, classLandscape) %>% 
    summarise(
      hypoDirection = sum(estimate > 0, na.rm = TRUE),
      hypoSupportSigCount = sum(!hypoSupportSig == "No Support"),
      totalEstimates = n()
    ) %>% 
    left_join(poisMetaBetas)
  
  # twostep -----------------------------------------------------------------
  
  twoStepResults <- twoStepResults %>% 
    dplyr::mutate("estimate" = twoStepBeta,
                  "se" = twoStepSE) %>% 
    mutate(hypoSupportSig = case_when(
      (estimate - 1.96*se) > 0 & species == "Ophiophagus hannah" ~ "Significant Support OPHA",
      (estimate - 1.96*se) > 0 & species == "Python bivittatus" ~ "Significant Support PYBI",
      (estimate - 1.96*se) > 0 & species == "Bungarus candidus" ~ "Significant Support BUCA",
      (estimate - 1.96*se) > 0 & species == "Bungarus fasciatus" ~ "Significant Support BUFA",
      TRUE ~ "No Support"
    ), lower = estimate - 1.96*se,
    upper = estimate + 1.96*se) %>%   
    dplyr::select(-twoStepBeta, -twoStepSE)
  
  twoStepMetaBetas <- metaBetas %>% 
    mutate(method = str_extract(model, "ssf|rsf|pois|twoStep|areaBased"),
           species = str_extract(model, "OPHA|PYBI|BUCA|BUFA"),
           hypothesis = str_extract(model, "H1|H2"),
           classLandscape = str_extract(model, "binary|continuous")) %>% 
    mutate(species = case_when(
      species == "OPHA" ~ "Ophiophagus hannah",
      species == "PYBI" ~ "Python bivittatus",
      species == "BUCA" ~ "Bungarus candidus",
      species == "BUFA" ~ "Bungarus fasciatus"
    )) %>% 
    filter(method == "twoStep") %>% 
    filter(.variable == "b_Intercept") %>% 
    select(-.width, -.variable)
  
  twoStep_summary <- twoStepResults %>% 
    group_by(species, hypothesis, classLandscape) %>% 
    summarise(
      hypoDirection = sum(estimate > 0, na.rm = TRUE),
      hypoSupportSigCount = sum(!hypoSupportSig == "No Support"),
      totalEstimates = n()
    ) %>% 
    left_join(twoStepMetaBetas)
  
  # rsf ---------------------------------------------------------------------
  
  rsfResults <- rsfResults %>% 
    dplyr::mutate("estimate" = Estimate,
                  "se" = SE) %>% 
    mutate(hypoSupportSig = case_when(
      (estimate - 1.96*se) > 0 & species == "Ophiophagus hannah" ~ "Significant Support OPHA",
      (estimate - 1.96*se) > 0 & species == "Python bivittatus" ~ "Significant Support PYBI",
      (estimate - 1.96*se) > 0 & species == "Bungarus candidus" ~ "Significant Support BUCA",
      (estimate - 1.96*se) > 0 & species == "Bungarus fasciatus" ~ "Significant Support BUFA",
      TRUE ~ "No Support"
    ), lower = estimate - 1.96*se,
    upper = estimate + 1.96*se) %>%   
    dplyr::select(-SE, -zValue, -PrZ, -type)
  
  rsfMetaBetas <- metaBetas %>% 
    mutate(method = str_extract(model, "ssf|rsf|pois|twoStep|areaBased"),
           species = str_extract(model, "OPHA|PYBI|BUCA|BUFA"),
           hypothesis = str_extract(model, "H1|H2"),
           classLandscape = str_extract(model, "binary|continuous")) %>% 
    mutate(species = case_when(
      species == "OPHA" ~ "Ophiophagus hannah",
      species == "PYBI" ~ "Python bivittatus",
      species == "BUCA" ~ "Bungarus candidus",
      species == "BUFA" ~ "Bungarus fasciatus"
    )) %>% 
    filter(method == "rsf") %>% 
    filter(.variable == "b_Intercept") %>% 
    select(-.width, -.variable)
  
  rsf_summary <- rsfResults %>% 
    group_by(species, hypothesis, classLandscape) %>% 
    summarise(
      hypoDirection = sum(estimate > 0, na.rm = TRUE),
      hypoSupportSigCount = sum(!hypoSupportSig == "No Support"),
      totalEstimates = n()
    ) %>% 
    left_join(rsfMetaBetas)
  
  # combine -----------------------------------------------------------------

  fullSummaryTable <- do.call(rbind, mget(ls(pattern = "_summary$")))
  
  fullSummaryTable <- fullSummaryTable %>% 
    dplyr::select(-model, -.point, -.interval) %>% 
    mutate(species = factor(species, levels = c(
      "Ophiophagus hannah",
      "Python bivittatus",
      "Bungarus candidus",
      "Bungarus fasciatus"
    )),
    hypothesis = factor(hypothesis, levels = c(
      "H1",
      "H2"
    )),
    method = factor(method, levels = c(
      "areaBased",
      "rsf",
      "twoStep",
      "ssf",
      "pois"
    )),
    classLandscape = factor(classLandscape, levels = c(
      "binary",
      "continuous"
    ))
    ) %>% 
    arrange(method, species, hypothesis, classLandscape)
  
  write.csv(fullSummaryTable, file = here::here("modelOutput", "metaResults.csv"),
            row.names = FALSE)
  
  return(fullSummaryTable)
  
}