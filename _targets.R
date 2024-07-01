# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed. # nolint
library(dplyr)

# Set target options:
tar_option_set(
  packages = c("qs",
               "here",
               "readr",
               "dplyr",
               "stringr",
               "sf",
               "terra",
               "tidyterra",
               "amt",
               "move",
               "ggplot2",
               "ggtext",
               "ggridges",
               "adehabitatHS",
               "adehabitatHR",
               "sp",
               "raster",
               "INLA",
               "TwoStepCLogit",
               "patchwork",
               "inborutils"), # zenodo download
  garbage_collection = TRUE,
  format = "qs", # storage format
  storage = "worker",
  retrieval = "worker",
  memory = "transient"
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

options(clustermq.scheduler = "multiprocess")

dir.create(here::here("figures"), showWarnings = FALSE)
dir.create(here::here("tables"), showWarnings = FALSE)

# Data locations ----------------------------------------------------------

speciesLocations <- data.frame(speciesCode = c("OPHA", "PYBI", "BUCA", "BUFA"),
                               doi = c("10.5281/zenodo.3666028",
                                       "10.5281/zenodo.4026927",
                                       "10.5281/zenodo.5495839",
                                       NA),
                               movebankID = c("1649411628;556564170;1093796277",
                                              NA,
                                              NA,
                                              "1204811528")
)

# Options for analysis variations -----------------------------------------

set.seed(2024)

optionsList_data <- list(
  species = c("OPHA", "PYBI", "BUCA", "BUFA")
)

optionsData <- tidyr::expand_grid(
  species = optionsList_data$species,
  landscape = c("binary", "continuous")
)

optionsData <- optionsData %>% 
  left_join(tidyr::expand_grid(
    species = c("OPHA"),
    landscape = c("binary", "continuous"),
    hypothesis = c("H1", "H2")
  )) %>% 
  mutate(hypothesis = ifelse(is.na(hypothesis), "H1", hypothesis)) %>% 
  select(species, hypothesis, landscape)

optionsList_area <- list(
  Method_method = c("areaBased"),
  # areaMethod = c("MCP", "AKDE"),
  areaMethod = c("MCP"),
  areaContour = c(95, 99),
  Method_ap = as.integer(round(exp(seq(log(1), log(10), length.out = 2)), digits = 1)),
  Method_sp = c("rd", "st")
)

optionsList_areaMethods <- list(
  Method_method = c("areaBased"),
  areaBasedMethod = c("Compana"),
  areaBasedTest = c("randomisation", "parametric")
)

optionsList_sff <- list(
  Method_method = c("ssf"),
  MethodSSF_as = c(2, 10),
  MethodSSF_mf = c("mf.is", "mf.ss"),
  MethodSSF_sd = c("gamma", "exp"),
  MethodSSF_td = c("vonmises", "unif")
)

optionsList_pois <- list(
  Method_method = c("pois"),
  MethodPois_as = c(2, 10),
  MethodPois_mf = c("mf.is", "mf.ss"),
  MethodPois_sd = c("gamma", "exp"),
  MethodPois_td = c("vonmises", "unif")
)

optionsCompleteList <- list(
  "species" = optionsList_data,
  "pois" = optionsList_pois,
  "ssf" = optionsList_sff,
  "area" = optionsList_area,
  "areaMethods" = optionsList_areaMethods
)

saveRDS(optionsCompleteList, file = here::here("data", "optionsCompleteList.rds"))

# Target pipeline ---------------------------------------------------------

prereg <- list(
  tar_map(
    values = optionsList_data,
    tar_target(
      rawData,
      download_datasets(speciesLocations, species),
      format = "file"
    )
  ),
  tar_target(
    rmdRenderPrereg,
    render_rmd(fileIN = here::here("notebook", "prereg",
                                   "multiverseSnakesPrereg.rmd"),
               fileOUT = here::here("notebook", "prereg",
                                    "multiverseSnakesPrereg.pdf")),
    format = "file",
    priority = 0.95
  )
)

coreMultiverse <- list(
  tar_map(
    values = optionsData,
    tar_target(
      movementData, # should be list where first object is the landscape - classified
      read_data(species, hypothesis, landscape) # need to read in landscape per species
    ),
    tar_target(areaBasedAvailUse,
               area_based_extraction(
                 allIndividualData = movementData,
                 optionsList = optionsList_area
               ),
               priority = 0.9),
    tar_target(areaBasedOUT,
               area_based_calculations(
                 availUseData = areaBasedAvailUse,
                 optionsList = optionsList_area,
                 optionsListArea = optionsList_areaMethods
               ),
               priority = 0.9),
    tar_target(ssfOUT,
               summarise_ssf_results(
                 movementData,
                 optionsList = optionsList_sff
               ),
               priority = 0.9),
    tar_target(poisOUT,
               method_pois_inla(
                 allIndividualData = movementData,
                 optionsList = optionsList_pois),
               priority = 0.9),
    tar_target(twoStepOUT,
               method_twoStep(
                 allIndividualData = movementData,
                 optionsList = optionsList_pois),
               priority = 0.9)
  )
)


# Poisson model combined --------------------------------------------------

poisCompiled <- list(
  tar_combine(
    poisResults,
    coreMultiverse[[1]][grep("poisOUT", names(coreMultiverse[[1]]))],
    command = rbind(!!!.x),
    priority = 0.8
  ),
  tar_target(
    poisEstimateOutputs,
    write.csv(poisResults,
              here::here("data", "poisEstimateOutputs_uncom.csv"), row.names = FALSE),
    format = "file"
  ),
  tar_target(
    poisSpecCurve,
    generate_spec_curves(
      outputResults = poisResults,
      method = "pois"
    )
  ),
  tar_target(
    poisBrms,
    run_brms(
      resultsData = poisResults,
      iter = 20000,
      warmup = 8000,
      thin = 20
    )
  )
)

# TwoStep Model Combined --------------------------------------------------

twoStepCompiled <- list(
  tar_combine(
    twoStepResults,
    coreMultiverse[[1]][grep("twoStepOUT", names(coreMultiverse[[1]]))],
    command = rbind(!!!.x),
    priority = 0.8
  ),
  tar_target(
    twoStepEstimateOutputs,
    write.csv(twoStepResults,
              here::here("data", "twoStepEstimateOutputs_uncom.csv"), row.names = FALSE),
    format = "file"
  ),
  tar_target(
    twoStepSpecCurve,
    generate_spec_curves(
      outputResults = twoStepResults,
      method = "twoStep"
    )
  ),
  tar_target(
    twoStepBrms,
    run_brms(
      resultsData = twoStepResults,
      iter = 20000,
      warmup = 8000,
      thin = 20
    )
  )
)

# Area Based Model Combined -----------------------------------------------

areaBasedCompiled <- list(
  tar_combine(
    areaBasedResults,
    coreMultiverse[[1]][grep("areaBasedOUT", names(coreMultiverse[[1]]))],
    command = rbind(!!!.x),
    priority = 0.8
  ),
  tar_target(
    areaBasedEstimateOutputs,
    write.csv(areaBasedResults,
              here::here("data", "areaBasedEstimateOutputs_uncom.csv"), row.names = FALSE),
    format = "file"
  ),
  tar_target(
    areaBasedSpecCurve,
    generate_spec_curves(
      outputResults = areaBasedResults,
      method = "areaBased"
    )
  ),
  tar_target(
    areaBasedBrms,
    run_brms(
      resultsData = areaBasedResults,
      iter = 20000,
      warmup = 8000,
      thin = 20
    )
  )
)

# SSF Models Combined -----------------------------------------------------

ssfCompiled <- list(
  tar_combine(
    ssfResults,
    coreMultiverse[[1]][grep("ssfOUT", names(coreMultiverse[[1]]))],
    command = rbind(!!!.x),
    priority = 0.8
  ),
  tar_target(
    ssfEstimateOutputs,
    write.csv(ssfResults,
              here::here("data", "ssfEstimateOutputs_uncom.csv"), row.names = FALSE),
    format = "file"
  ),
  tar_target(
    ssfSpecCurve,
    generate_spec_curves(
      outputResults = ssfResults,
      method = "ssf"
    )
  ),
  tar_target(
    ssfBrms,
    run_brms(
      resultsData = ssfResults,
      iter = 20000,
      warmup = 8000,
      thin = 20
    )
  )
)

# BRM Model Outputs -------------------------------------------------------

brmModelOutputs <- list(
  tar_combine(
    modelsBrms,
    # manually pull out the brms model outputs
    list(
      ssfCompiled[[4]],
      areaBasedCompiled[[4]],
      poisCompiled[[4]],
      twoStepCompiled[[4]]),
    command = list(!!!.x),
    priority = 0.5
  ),
  tar_target(
    diagnosticPlots,
    diagnostics_brms(modelsList = modelsBrms),
    priority = 0.4
  ),
  tar_target(
    modelExtracts,
    extract_model_values(modelsList = modelsBrms),
    priority = 0.4
  ),
  tar_target(
    effectPlots,
    generate_effect_plots(modelsList = modelsBrms),
    priority = 0.4
  ),
  tar_target(
    allEffectPlots,
    generate_allEffect_plots(modelExtracts = modelExtracts),
    priority = 0.4
  )
)

# Manuscript Prep ---------------------------------------------------------

extraDetails <- list(
  tar_combine(
    movementDataAll,
    coreMultiverse[[1]][grep("movementData", names(coreMultiverse[[1]]))],
    command = list(!!!.x),
    priority = 0.3
  ),
  tar_target(
    landscapePlots,
    plot_analysis_landscapes(movementDataAll) # need to read in landscape per species
  ),
  tar_target(
    trackingPlotsAndTables,
    summarise_tracking_data(movementDataAll) # need to read in landscape per species
  )
)

manuscriptRendering <- list(
  tar_target(
    rmdRenderManuscript,
    render_rmd(fileIN = here::here("notebook", "manuscript",
                                   "multiverseSnakesManuscript.rmd"),
               fileOUT = here::here("notebook", "manuscript",
                                    "multiverseSnakesManuscript.pdf"),
               allEffectPlots,
               poisSpecCurve,
               areaBasedSpecCurve,
               twoStepSpecCurve,
               ssfSpecCurve,
               trackingPlotsAndTables,
               landscapePlots
               ),
    cue = tar_cue(mode = "always"),
    priority = 0.1
  )
)

# All targets lists -------------------------------------------------------

list(
  prereg,
  coreMultiverse,
  poisCompiled,
  twoStepCompiled,
  areaBasedCompiled,
  ssfCompiled,
  brmModelOutputs,
  extraDetails,
  manuscriptRendering
)