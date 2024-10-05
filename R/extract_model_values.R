#' Generate files needed for RMD manuscript
#'
#' @name extract_model_values
#' @importFrom magrittr %>%
#' @description Extract the required values to report in the RMD file.
#' @return Will save a series of csv files containing the raw estimates and
#'   model betas that are needed to knit the manuscript.
#'
#' @export
extract_model_values <- function(modelsList){
  
  # Get all r2 --------------------------------------------------------------
  
  r2Outputs <- lapply(names(modelsList), function(x){
    # x <- names(modelsList)[1]
    model <- modelsList[[x]]
    
    tau2 <- ggdist::median_hdci(tidybayes::gather_draws(model,
                                                        `sd_index__Intercept`, regex = TRUE),
                                .width = c(0.95)) %>% 
      dplyr::select(.variable, .value, .lower, .upper, .width)
    names(tau2) <- c("variable", "value", "lower", "upper", "ci")
    tau2$variable <- "tau2_sd_index_intercept"
    tau2$se <- NA
    
    r2OUT <- performance::r2_bayes(model)
    
    r2df_1 <- data.frame(
      value = c(unlist(
        r2OUT[1]),
        unlist(r2OUT[2])
      ),
      SE = c(attr(r2OUT,"SE")$R2_Bayes,
             attr(r2OUT,"SE")$R2_Bayes_marginal),
      rbind(attr(r2OUT,"CI")$R2_Bayes,
            attr(r2OUT,"CI")$R2_Bayes_marginal),
      variable = c("r2 conditional", "r2 marginal")
    )
    names(r2df_1) <- c("value", "se", "ci", "lower", "upper", "variable")
    modelExtras <- rbind(r2df_1, tau2)
    modelExtras$model <- x
    
    return(modelExtras)
    
    print(paste0(x, " - r2 Extracted"))
  })
  r2Outputs <- do.call(rbind, r2Outputs)
  
  write.csv(r2Outputs, file = here::here("modelOutput", "brmsR2Results.csv"),
            row.names = FALSE)
  
  # get all betas -----------------------------------------------------------
  
  betasOutputs <- lapply(names(modelsList), function(x){
    model <- modelsList[[x]]
    if(class(model) == "brmsfit"){
      out <- ggdist::median_hdci(tidybayes::gather_draws(model,
                                                         `b_.*`, regex = TRUE),
                                 .width = c(0.95))
      out$model <- x
      return(out)
    } else (
      return(NULL)
    )
    
    print(paste0(x, " - betas Extracted"))
  })
  betasOutputs <- do.call(rbind, betasOutputs)
  
  write.csv(betasOutputs, file = here::here("modelOutput", "brmsEstResults.csv"),
            row.names = FALSE)
  
  extractedList <- list(
    "r2Outputs" = r2Outputs,
    "betasOutputs" = betasOutputs)
  
  return(extractedList)
}