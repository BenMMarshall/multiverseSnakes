#' Generate plots to assess brms
#'
#' @name diagnostics_brms
#' @description A
#' @param modelsList tar_targets resulting in Brms model
#' @return a
#'
#' @export
diagnostics_brms <- function(modelsList){
  
  for(n in 1:length(modelsList)){
    
    name <- names(modelsList)[n]
    currMod <- modelsList[[n]]
    
    if(class(currMod) == "brmsfit"){
      
      vars <- get_variables(currMod)
      varsToPlot <- vars[stringr::str_detect(vars, "b_")]
      
      traceplot <- mcmc_trace(currMod, pars = varsToPlot)
      ggsave(traceplot,
             filename = here("modelOutput", paste0(name, "_traceplot.png")),
             dpi = 300, width = 210, height = 140,
             units = "mm")
      
      acfplot <- mcmc_acf(currMod, pars = varsToPlot)
      ggsave(acfplot,
             filename = here("modelOutput", paste0(name, "_acfplot.png")),
             dpi = 300, width = 210, height = 140,
             units = "mm")
    }
  }
  
  return(list(traceplot, acfplot))
}