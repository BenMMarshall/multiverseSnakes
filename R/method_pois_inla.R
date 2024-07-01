#' Calculate population selection using a Poisson model
#'
#' @name method_pois_inla
#' @description A
#' @param allIndividualData All simulated movement data
#' @param optionsList options list that will be used to loop through options
#' @return Population estimates using a Poisson model.
#'
#' @export
method_pois_inla <- function(allIndividualData, optionsList){
  
  # targets::tar_load("movementData_BUFA_H1_binary")
  # targets::tar_load("movementData_OPHA_H1_continuous")
  # targets::tar_source()
  # allIndividualData <- movementData_BUFA_H1_binary
  # allIndividualData <- movementData_OPHA_H1_continuous
  # optionsList <- optionsList_pois
  landscape <- rast(allIndividualData$habitatRasterLocation)
  land <- str_extract(allIndividualData$habitatRasterLocation, "binary|continuous")
  
  optionsForm <- optionsList$MethodPois_mf
  optionsLand <- optionsList$MethodPois_land
  optionsASteps <- optionsList$MethodPois_as
  optionsStepD <- optionsList$MethodPois_sd
  optionsTurnD <- optionsList$MethodPois_td
  
  listLength <- length(optionsForm) *
    length(optionsASteps) *
    length(optionsStepD) *
    length(optionsTurnD)
  
  inlaOUTList <- vector("list", length = listLength)
  i <- 0
  # for(sampID in names(sampleGroups)){
  #   print(sampID)
  # sampID <- "samp1"
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
  # print("--- movementData subsampled")
  
  # select only the information we need
  movementData <- allIndividualData$movementData_sf %>% 
    dplyr::mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S",
                                        tz = "UTC")) %>% 
    dplyr::select("x" = x, "y" = y,
                  "t" = datetime, "id" = id) %>% 
    dplyr::group_by(id) %>% 
    dplyr::arrange(t)
  
  # separate the individuals out into a list-column of dataframes, each item an animal
  movementDataNest <- sf::st_drop_geometry(movementData) %>% 
    tidyr::nest(moveData = -id)
  
  print(sum(is.na(movementData$t)))
  
  # map operates a lot like an apply or loop. It repeats the function to each item
  # in the list. In this case we make all the individual dataframes into track
  # objects. Check dat_all to see the list of dataframes now has a second
  # dataframe/tibble for the track.
  movementDataNest <- movementDataNest %>% 
    mutate(trk = purrr::map(.x = moveData, .f = function(d){
      make_track(d, .x = x, .y = y, .t = t, crs = 32647)
    }))
  
  # Here the summarize_sampling_rate is repeated on each track object to give you
  # an individual level summary.
  # movementData %>%
  #   mutate(sr = lapply(trk, summarize_sampling_rate)) %>%
  #   dplyr::select(id, sr) %>%
  #   unnest(cols = c(sr))
  
  for(as in optionsASteps){
    # as <- optionsASteps[2]
    for(sd in optionsStepD){
      # sd <- optionsStepD[1]
      for(td in optionsTurnD){
        # td <- optionsTurnD[1]
        # had to modify the code and avoid map to make sure the distributions are
        # based on a single individual, issues with the sl_ and ta_ being passed to
        # the fit_distr functions inside map
        allTracksList <- vector("list", length = length(movementDataNest$id))
        names(allTracksList) <- movementDataNest$id
        for(indiID in movementDataNest$id){
          print(indiID)
          # indiID <- "OPHA002"
          # which(movementDataNest$id == indiID)
          
          indiTrack <- movementDataNest$trk[[which(movementDataNest$id == indiID)]] %>% 
            steps() %>% 
            filter(sl_>0)
          
          # hist(indiTrack$sl_)
          # hist(indiTrack$sl_[indiTrack$sl_ < quantile(indiTrack$sl_, 0.75)])
          
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
        poisModelData <- do.call(rbind, allTracksList)
        
        print("--- poisModelData generated")
        
        poisModelData <- poisModelData %>% 
          mutate(
            y = as.numeric(case_),
            id = as.numeric(factor(id)), 
            step_id = paste0(id, step_id_, sep = "-"),
            cos_ta = cos(ta_), 
            log_sl = log(sl_))
        
        if(land == "binary"){
          poisModelData <- poisModelData %>% 
            mutate(layer = factor(paste0("c", layer),
                                  levels = c("c0", "c1")))
        } else {
          # scaling appears to be required for INLA to run
          poisModelData$layer <- scale(poisModelData$layer)[,1]
        }
        
        # We can run the INLA model using the priors and set-up from Muff et al.
        # Precision for the priors of slope coefficients
        prec.beta.trls <- 1e-4
        
        for(form in optionsForm){
          # form <- optionsForm[1]
          if(form == "mf.is"){
            
            inlaFormula <- y ~ -1 + 
              layer + # fixed covariate effect
              layer:log_sl + # covar interactions
              layer:cos_ta + 
              f(step_id, model="iid", hyper = list(theta = list(initial = log(1e-6), fixed = TRUE))) +
              f(id, layer, values = 1:length(unique(poisModelData$id)), model="iid",
                hyper = list(theta = list(initial = log(1), fixed = FALSE, 
                                          prior = "pc.prec", param = c(1, 0.05)))) 
            
          } else if(form == "mf.ss"){
            
            inlaFormula <- y ~ -1 + 
              layer + # fixed covariate effect
              f(step_id, model="iid", hyper = list(theta = list(initial = log(1e-6), fixed = TRUE))) +
              f(id, layer, values = 1:length(unique(poisModelData$id)), model="iid",
                hyper = list(theta = list(initial = log(1), fixed = FALSE, 
                                          prior = "pc.prec", param = c(1, 0.05)))) 
          } # if else end
          
          # print(sum(poisModelData$y[poisModelData$layer == "c0"]))
          # print(sum(poisModelData$y[poisModelData$layer == "c2"]))
          # print(sum(is.na(poisModelData$y)))
          # print(sum(is.na(poisModelData$log_sl)))
          # print(sum(is.na(poisModelData$cos_ta)))
          # print(sum(is.na(poisModelData$id)))
          
          # natural model
          inlaOUT <- try(
            inla(inlaFormula,
                 family = "Poisson",
                 data = poisModelData,
                 # verbose = TRUE,
                 control.fixed = list(
                   mean = 0,
                   prec = list(default = prec.beta.trls)),
                 safe = TRUE,
                 control.inla = list(control.vb = list(emergency = 30)))
          )
          
          if(class(inlaOUT)[1] == "try-error"){
            
            if(land == "binary"){
              inlaResults <- as.data.frame(matrix(NA, 2, 7))
              inlaResults$term <- c("layerc1", "layerc0")
            } else {
              inlaResults <- as.data.frame(matrix(NA, 1, 7))
              inlaResults$term <- c("layercontinuous")
            }
            
            names(inlaResults) <- c("mean", "sd", "q025", "q50", "q975",
                                    "mode", "kld", "term")
            inlaResults$mmarginal <- NA
            inlaResults$emarginal <- NA
            
          } else {
            
            if(land == "binary"){
              inlaResults <- inlaOUT$summary.fixed[1:2,]
              inlaResults$term <- row.names(inlaResults)
            } else {
              inlaResults <- inlaOUT$summary.fixed[1,]
              inlaResults$term <- paste0(row.names(inlaResults), "continuous")
            }
            
            names(inlaResults) <- c("mean", "sd", "q025", "q50", "q975",
                                    "mode", "kld", "term")
            inlaResults$mmarginal <- inla_mmarginal(inlaOUT)
            inlaResults$emarginal <- inla_emarginal(inlaOUT)
            
          }
          
          print(inlaResults$mean)
          
          optionsInfo <-
            data.frame(
              species = allIndividualData$movementData_sf$species[1],
              analysis = "Poisson",
              classLandscape = land,
              modelFormula = form,
              availablePerStep = as,
              stepDist = sd,
              turnDist = td
            )
          
          print(optionsInfo)
          print("INLA Results")
          
          combinedResults <- cbind(optionsInfo,
                                   inlaResults)
          i <- i + 1
          inlaOUTList[[i]] <- combinedResults
          
        } #form
      }
    }
  }
  # }
  return(do.call(rbind, inlaOUTList))
}
