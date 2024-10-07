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
               "tidyr",
               "stringr",
               "sf",
               "terra",
               "tidyterra",
               "amt",
               "move",
               "ggplot2",
               "ggtext",
               "ggnewscale",
               "ggridges",
               "adehabitatHS",
               "adehabitatHR",
               "sp",
               "raster",
               "INLA",
               "TwoStepCLogit",
               "patchwork",
               "brms",
               "tidybayes",
               "lme4",
               "ctmm",
               "performance",
               "bayesplot",
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
dir.create(here::here("modelOutput"), showWarnings = FALSE)

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
  dplyr::select(species, hypothesis, landscape)

optionsDataBrms <- optionsData %>% 
  rowwise() %>% 
  mutate(fullOption = paste0(species, "_", hypothesis, "_", landscape))

optionsList_area <- list(
  Method_method = c("areaBased"),
  areaMethod = c("MCP", "AKDE", "KDEhref"),
  # areaMethod = c("MCP", "KDEhref"),
  areaContour = c(90, 95, 99),
  Method_ap = as.integer(round(exp(seq(log(1), log(50), length.out = 8)), digits = 0)),
  Method_sp = c("rd", "st")
)

optionsList_areaMethods <- list(
  Method_method = c("areaBased"),
  areaBasedMethod = c("Compana", "rsf"),
  areaBasedTest = c("randomisation", "parametric"),
  # areaBasedRsfRandom = c("slopes", "interOnly")
  areaBasedRsfRandom = c("interOnly")
)

optionsList_sff <- list(
  Method_method = c("ssf"),
  # MethodSSF_as = c(2, 10),
  MethodSSF_as = as.integer(round(exp(seq(log(1), log(50), length.out = 8)), digits = 0)),
  MethodSSF_mf = c("mf.is", "mf.ss"),
  MethodSSF_sd = c("gamma", "exp"),
  MethodSSF_td = c("vonmises", "unif")
)

optionsList_pois <- list(
  Method_method = c("pois"),
  # MethodPois_as = c(2, 10),
  MethodPois_as = as.integer(round(exp(seq(log(1), log(50), length.out = 8)), digits = 0)),
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

# prereg <- list(
#   tar_map(
#     values = optionsList_data,
#     tar_target(
#       rawData,
#       download_datasets(speciesLocations, species),
#       format = "file"
#     )
#   ),
#   tar_target(
#     rmdRenderPrereg,
#     rmarkdown::render(input = here::here("notebook", "prereg",
#                                           "multiverseSnakesPrereg.Rmd"),
#                       output_file = here::here("notebook", "prereg",
#                                            "multiverseSnakesPrereg.pdf")),
#     format = "file",
#     priority = 0.95
#   )
# )

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
    tar_target(rsfOUT,
               area_based_rsf(
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
               priority = 0.9),
    tar_target(wrsfOUT,
               method_wrsf(
                 allIndividualData = movementData),
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
  tar_map(
    values = optionsDataBrms,
    tar_target(
      poisBrms,
      run_brms(
        resultsData = poisResults,
        modelName = fullOption,
        iter = 1200,
        warmup = 400,
        thin = 2
      )
    )
  ),
  tar_target(
    poisSpecCurve,
    generate_spec_curves(
      outputResults = poisResults,
      method = "pois"
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
  tar_map(
    values = optionsDataBrms,
    tar_target(
      twoStepBrms,
      run_brms(
        resultsData = twoStepResults,
        modelName = fullOption,
        iter = 1200,
        warmup = 400,
        thin = 2
      )
    )
  ),
  tar_target(
    twoStepSpecCurve,
    generate_spec_curves(
      outputResults = twoStepResults,
      method = "twoStep"
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
  tar_map(
    values = optionsDataBrms %>% 
      filter(landscape == "binary"),
    tar_target(
      areaBasedBrms,
      run_brms(
        resultsData = areaBasedResults,
        modelName = fullOption,
        iter = 1200,
        warmup = 400,
        thin = 2
      )
    )
  ),
  tar_target(
    areaBasedSpecCurve,
    generate_spec_curves(
      outputResults = areaBasedResults,
      method = "area"
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
  tar_map(
    values = optionsDataBrms,
    tar_target(
      ssfBrms,
      run_brms(
        resultsData = ssfResults,
        modelName = fullOption,
        iter = 1200,
        warmup = 400,
        thin = 2
      )
    )
  ),
  tar_target(
    ssfSpecCurve,
    generate_spec_curves(
      outputResults = ssfResults,
      method = "ssf"
    )
  )
)

# RSF Models Combined -----------------------------------------------------

rsfCompiled <- list(
  tar_combine(
    rsfResults,
    coreMultiverse[[1]][grep("^rsfOUT", names(coreMultiverse[[1]]))],
    command = rbind(!!!.x),
    priority = 0.8
  ),
  tar_target(
    rsfEstimateOutputs,
    write.csv(rsfResults,
              here::here("data", "rsfEstimateOutputs_uncom.csv"), row.names = FALSE),
    format = "file"
  ),
  tar_map(
    values = optionsDataBrms,
    tar_target(
      rsfBrms,
      run_brms(
        resultsData = rsfResults,
        modelName = fullOption,
        iter = 1200,
        warmup = 400,
        thin = 2
      )
    )
  ),
  tar_target(
    rsfSpecCurve,
    generate_spec_curves(
      outputResults = rsfResults,
      method = "rsf"
    )
  )
)

# wRSF Models Combined -----------------------------------------------------

wrsfCompiled <- list(
  tar_combine(
    wrsfResults,
    coreMultiverse[[1]][grep("^wrsfOUT", names(coreMultiverse[[1]]))],
    command = list(!!!.x),
    priority = 0.8
  ),
  tar_target(
    wrsfEstimateOutputs,
    compile_wrsf_summaries(wrsfResults)
  ),
  tar_target(
    wrsfSpecCurve,
    generate_spec_curves(
      outputResults = wrsfResults,
      method = "wrsf"
    )
  )
)

# BRM Model Outputs -------------------------------------------------------

brmModelOutputs <- list(
  tar_combine(
    modelsBrms,
    # manually pull out the brms model outputs
    list(
      ssfCompiled[[3]],
      areaBasedCompiled[[3]],
      poisCompiled[[3]],
      rsfCompiled[[3]],
      twoStepCompiled[[3]]),
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
    metaBetaPlot,
    plot_meta_betas(modelExtracts = modelExtracts),
    priority = 0.4
  ),
  tar_target(
    metaR2Plot,
    plot_meta_r2(modelExtracts = modelExtracts),
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
                                   "multiverseSnakesManuscript.Rmd"),
               fileOUT = here::here("notebook", "manuscript",
                                    "multiverseSnakesManuscript.pdf"),
               allEffectPlots,
               poisSpecCurve,
               areaBasedSpecCurve,
               twoStepSpecCurve,
               ssfSpecCurve,
               rsfSpecCurve,
               wrsfSpecCurve,
               trackingPlotsAndTables,
               landscapePlots,
               diagnosticPlots,
               modelExtracts,
               metaBetaPlot,
               metaR2Plot
    ),
    cue = tar_cue(mode = "always"),
    priority = 0.1
  )
)

# All targets lists -------------------------------------------------------

list(
  # prereg,
  coreMultiverse,
  poisCompiled,
  twoStepCompiled,
  areaBasedCompiled,
  rsfCompiled,
  wrsfCompiled,
  ssfCompiled,
  brmModelOutputs,
  extraDetails,
  manuscriptRendering
)
