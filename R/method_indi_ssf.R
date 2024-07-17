#' Run SSD based analyses
#'
#' @name method_indi_ssf
#' @description A
#' @param movementData must have a x and y column for locations, and a datetime column for timestamps ("%Y-%m-%d %H:%M:%S")
#' @param landscape
#' @param methodForm "mf.is", "mf.ss"
#' @param stepDist "gamma", "exp"
#' @param turnDist "vonmises", "unif"
#' @param availableSteps
#' @return a
#'
#' @export
method_indi_ssf <- function(
    # first two can be for individuals, as they will be provided by previous nodes
  movementData,
  landscape,
  # below can all be programmed as single values as the
  # targets workflow will be used to feed multiple values
  # in
  methodForm,
  stepDist,
  turnDist,
  availableSteps,
  seed){
  
  if(!require(amt)){
    stop("amt not installed")
  }
  
  if(class(movementData$datetime)[1] == "POSIXct"){
    movementData$t <- movementData$datetime
  } else {
    movementData$t <- as.POSIXct(movementData$datetime)
  }
  movementTrack <- amt::make_track(tbl = sf::st_drop_geometry(movementData),
                                   .x = x, .y = y, .t = t, crs = 32647)
  movementSteps <- amt::steps(movementTrack) %>% 
    filter(sl_>0)
  
  set.seed(seed)
  
  # methodForm <- "mf.is"
  # stepDist <- "gamma"
  # turnDist <- "vonmises"
  # availableSteps <- 2
  
  modelData <- try(
    amt::random_steps(movementSteps,
                      n_control = availableSteps,
                      sl_distr = amt::fit_distr(movementSteps$sl_, stepDist),
                      ta_distr = amt::fit_distr(movementSteps$ta_, turnDist))
  )
  
  if(class(modelData)[1] == "try-error"){
    print("step length fitting error")
    stepToFit <- movementSteps$sl_[movementSteps$sl_ < quantile(movementSteps$sl_, 0.75)]
    modelData <- amt::random_steps(movementSteps,
                                   n_control = availableSteps,
                                   sl_distr = amt::fit_distr(stepToFit, stepDist),
                                   ta_distr = amt::fit_distr(movementSteps$ta_, turnDist))
  }
  
  modelData <- amt::extract_covariates(modelData,
                                       landscape,
                                       where = "end")
  
  modelData$layer <- if_else(is.na(modelData$layer), 0, modelData$layer)
  if(all(modelData$layer %in% c(0, 1))){
    modelData$values <- paste0("c", modelData$layer)
    modelData$values <- factor(modelData$values)
  } else {
    modelData$values <- scale(modelData$layer)[,1]
  }
  
  if(length(unique(modelData$layer)) == 1){
    ssfOUT <- list(model = "Only one habitat used")
    return(ssfOUT)
  }
  
  if(methodForm == "mf.is"){
    mFormFull <- case_ ~
      values +
      sl_ + log(sl_) + cos(ta_) +
      strata(step_id_)
    
  } else if(methodForm == "mf.ss"){
    mFormFull <- case_ ~
      values +
      strata(step_id_)
    
  }
  
  ssfOUT <- amt::fit_issf(data = modelData,
                          formula = mFormFull,
                          model = TRUE)
  
  # ssfDF <- as.data.frame(summary(ssfOUT)$coef)
  # method <- rep("ssf", nrow(ssfDF))
  # ssfDF <- cbind(ssfDF, method)
  # return(multiverseHabitat::extract_estimate(ssfDF))
  return(ssfOUT)
}
