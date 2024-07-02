#' Calculate population selection using a Two step clogit
#'
#' @name method_twoStep
#' @description A
#' @param allIndividualData All simulated movement data
#' @param optionsList options list that will be used to loop through options
#' @return Population estimates using a Poisson model.
#'
#' @export
method_twoStep <- function(allIndividualData, optionsList){
  # targets::tar_load("movementData_BUFA_H1_binary")
  # targets::tar_load("movementData_OPHA_H1_binary")
  # targets::tar_source()
  # allIndividualData <- movementData_BUFA_H1_binary
  # allIndividualData <- movementData_OPHA_H1_binary
  # optionsList <- optionsList_pois
  landscape <- rast(allIndividualData$habitatRasterLocation)
  land <- str_extract(allIndividualData$habitatRasterLocation, "binary|continuous")
  hypo <- str_extract(allIndividualData$habitatRasterLocation, "H1|H2")
  
  optionsForm <- optionsList$MethodPois_mf
  optionsLand <- optionsList$MethodPois_land
  optionsASteps <- optionsList$MethodPois_as
  optionsStepD <- optionsList$MethodPois_sd
  optionsTurnD <- optionsList$MethodPois_td
  
  listLength <- length(optionsForm) *
    length(optionsASteps) *
    length(optionsStepD) *
    length(optionsTurnD)
  
  twoStepOUTList <- vector("list", length = listLength)
  i <- 0
  # for(sampID in names(sampleGroups)){
  
  # print(sampID)
  
  # sampID <- "samp4"
  # IDs <- optionsList_samples[[sampID]]
  # speciesCodeLetter <- stringr::str_extract(names(allIndividualData)[1], ".$")
  # IDs <- paste0("simData_",
  #               speciesCodeLetter, # add in species code letter
  #               "_i", sprintf("%03d", IDs))
  # 
  # sampledIndividualData <- allIndividualData[names(allIndividualData) %in% IDs]
  # 
  # movementData <- do.call(rbind, lapply(sampledIndividualData, function(x){
  #   x$locations
  # }))
  # 
  # print("--- movementData subsampled")
  
  # select only the information we need
  movementData <- allIndividualData$movementData_sf %>% 
    dplyr::mutate(datetime = as.POSIXct(datetime, format = "%y-%m-%d %H:%M:%S",
                                        tz = "UTC")) %>% 
    dplyr::select("x" = x, "y" = y,
                  "t" = datetime, "id" = id) %>% 
    dplyr::group_by(id) %>% 
    dplyr::arrange(t)
  
  # separate the individuals out into a list-column of dataframes, each item an animal
  movementDataNest <- sf::st_drop_geometry(movementData) %>% 
    tidyr::nest(moveData = -id)
  
  # map operates a lot like an apply or loop. It repeats the function to each item
  # in the list. In this case we make all the individual dataframes into track
  # objects. Check dat_all to see the list of dataframes now has a second
  # dataframe/tibble for the track.
  movementDataNest <- movementDataNest %>% 
    mutate(trk = map(moveData, function(d){
      make_track(d, .x = x, .y = y, .t = t, crs = 32647)
    }))
  
  # Here the summarize_sampling_rate is repeated on each track object to give you
  # an individual level summary.
  # movementData %>%
  #   mutate(sr = lapply(trk, summarize_sampling_rate)) %>%
  #   dplyr::select(id, sr) %>%
  #   unnest(cols = c(sr))
  
  for(as in optionsASteps){
    # as <- 2
    for(sd in optionsStepD){
      # sd <- "gamma"
      for(td in optionsTurnD){
        # td <- "vonmises"
        
        # had to modify the code and avoid map to make sure the distributions are
        # based on a single individual, issues with the sl_ and ta_ being passed to
        # the fit_distr functions inside map
        allTracksList <- vector("list", length = length(movementDataNest$id))
        names(allTracksList) <- movementDataNest$id
        for(indiID in movementDataNest$id){
          # indiID <- "BADGER_i005"
          # which(movementDataNest$id == indiID)
          
          # print(indiID)
          
          indiTrack <- movementDataNest$trk[[which(movementDataNest$id == indiID)]] %>% 
            steps() %>% 
            filter(sl_>0)
          
          indiTrackCov <- try(
            indiTrack %>% # removing the non-moves, or under GPS error
              random_steps(
                n_control = as,
                sl_distr = amt::fit_distr(x = indiTrack$sl_, dist_name = sd),
                ta_distr = amt::fit_distr(x = indiTrack$ta_, dist_name = td)
              ) %>% 
              extract_covariates(landscape) %>% 
              mutate(id = indiID)
          )
          
          if(class(indiTrackCov)[1] == "try-error"){
            print("step length fitting error")
            stepToFit <- indiTrack$sl_[indiTrack$sl_ < quantile(indiTrack$sl_, 0.75)]
            indiTrackCov <- indiTrack %>% # removing the non-moves, or under GPS error
              random_steps(
                n_control = as,
                sl_distr = amt::fit_distr(x = stepToFit, dist_name = sd),
                ta_distr = amt::fit_distr(x = indiTrack$ta_, dist_name = td)
              ) %>% 
              extract_covariates(landscape) %>% 
              mutate(id = indiID)
          }
          
          allTracksList[[indiID]] <- indiTrackCov
        }
        popModelData <- do.call(rbind, allTracksList)
        
        popModelData$layer[is.na(popModelData$layer)] <- 0
        
        # for some binrary have to remove IDs --- the model cannot be fitted because
        # the value of variable 'layerc1' remains constant within all strata of
        # at least one cluster
        if(land == "binary"){
          landscapeLayers <- popModelData %>% 
            as.data.frame() %>% 
            dplyr::group_by(id) %>% 
            dplyr::summarise(mixed = n_distinct(layer)) %>% 
            filter(mixed == 1)
          
          popModelData <- popModelData %>% 
            filter(!id %in% landscapeLayers$id)
        }
        # unique(popModelData$id)
        print("--- popModelData generated")
        
        popModelData <- popModelData %>% 
          mutate(
            y = as.numeric(case_),
            id = as.numeric(factor(id)), 
            step_id = paste(id, step_id_, sep = "-"),
            cos_ta = cos(ta_), 
            log_sl = log(sl_))
        
        if(land == "binary"){
          popModelData <- popModelData %>% 
            mutate(layer = factor(paste0("c", layer),
                                  levels = c("c0", "c1")))
        } else {
          # scaling appears to be required for INLA to run
          popModelData$layer <- scale(popModelData$layer)[,1]
        }
        
        print(paste(as, sd, td, land))
        # print(unique(popModelData$layer))
        
        for(form in optionsForm){
          if(form == "mf.is"){
            
            twoStepOUT <- try(
              Ts.estim(formula = y ~ 
                         layer + 
                         layer:log_sl + # covar iteractions
                         layer:cos_ta +
                         strata(step_id) +
                         cluster(id),
                       data = popModelData,
                       random = ~ layer,
                       all.m.1 = TRUE,
                       D = "UN(1)")
            )
            
          } else if(form == "mf.ss"){
            
            twoStepOUT <- try(
              Ts.estim(formula = y ~ 
                         layer + 
                         strata(step_id) +
                         cluster(id),
                       data = popModelData,
                       random = ~ layer,
                       all.m.1 = TRUE,
                       D = "UN(1)")
            )
            
          } # if else end
          
          if(class(twoStepOUT)[1] == "try-error"){
            
            optionsInfo <-
              data.frame(
                species = allIndividualData$movementData_sf$species[1],
                analysis = "TwoStep",
                classLandscape = land,
                hypothesis = hypo,
                modelFormula = form,
                availablePerStep = as,
                stepDist = sd,
                turnDist = td,
                twoStepBeta = NA,
                twoStepSE = NA
              )
            
          } else {
            
            if(land == "binary"){
              tsBeta = twoStepOUT$beta["layerc1"]
              tsSE = twoStepOUT$se["layerc1"]
            } else {
              tsBeta = twoStepOUT$beta["layer"]
              tsSE = twoStepOUT$se["layer"]
            }
            
            optionsInfo <-
              data.frame(
                species = allIndividualData$movementData_sf$species[1],
                analysis = "TwoStep",
                classLandscape = land,
                hypothesis = hypo,
                modelFormula = form,
                availablePerStep = as,
                stepDist = sd,
                turnDist = td,
                twoStepBeta = tsBeta,
                twoStepSE = tsSE
              )
          }
          
          i <- i + 1
          twoStepOUTList[[i]] <- optionsInfo
          
          rm(twoStepOUT)
          
        }
      }
    }
  }
  return(do.call(rbind, twoStepOUTList))
}
