# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("qs",
               "here",
               "amt",
               "move",
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

optionsList_data <- list(
  species = c("OPHA", "BUCA", "BUFA", "PYBI")
)

optionsList_area <- list(
  Method_method = c("areaBased"),
  areaMethod = c("MCP", "AKDE"),
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
      rawMovementData,
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
    values = optionsList_data,
    tar_target(
      movementData, # should be list where first object is the landscape - classified
      read_data(species) # need to read in landscape per species
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
                 sampleGroups = optionsList_samples,
                 optionsList = optionsList_areaMethods
               ),
               priority = 0.9),
    tar_target(ssfModels,
               wrapper_indi_ssf(
                 allIndividualData = movementData,
                 optionsList = optionsList_sff
               ),
               priority = 0.9),
    tar_target(ssfOUT,
               sample_ssf_results(
                 ssfModels,
                 optionsList = optionsList_sff
               ),
               priority = 0.9),
    tar_target(poisOUT,
               method_pois_inla(
                 allIndividualData = movementData,
                 sampleGroups = optionsList_samples,
                 optionsList = optionsList_pois),
               priority = 0.9),
    tar_target(twoStepOUT,
               method_twoStep(
                 allIndividualData = movementData,
                 sampleGroups = optionsList_samples,
                 optionsList = optionsList_pois),
               priority = 0.9)
  )
)

resultsCompiled <- list(
  tar_combine(
    allResults,
    coreMultiverse[[1]][grep("OUT", names(coreMultiverse[[1]]))],
    command = rbind(!!!.x),
    priority = 0.8
  ),
  tar_target(
    allSpecCurve,
    generate_spec_curves(
      outputResults = allResults,
      method = "pois"
    )
  ),
  tar_target(
    allBrms,
    run_brms(
      resultsData = allResults
    )
  ),
  tar_target(
    diagnosticPlots,
    diagnostics_brms(modelsList = allBrms)
  ),
  tar_target(
    modelExtracts,
    extract_model_values(modelsList = allBrms)
  ),
  tar_target(
    effectPlots,
    generate_effect_plots(modelsList = allBrms)
  ),
  tar_target(
    rmdRenderManuscript,
    render_rmd(fileIN = here::here("notebook", "manuscript",
                                   "multiverseSnakesManuscript.rmd"),
               fileOUT = here::here("notebook", "manuscript",
                                    "multiverseSnakesManuscript.pdf"),
               modelExtracts, effectPlots, allSpecCurve, optionsCompleteList),
    cue = tar_cue(mode = "always"),
    priority = 0.1
  )
)

# All targets lists -------------------------------------------------------

list(
  prereg,
  coreMultiverse,
  resultsCompiled
)