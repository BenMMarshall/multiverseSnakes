#' Render the manuscript rmd
#'
#' @name render_rmd
#' @description A
#' @param modelExtractsTarget table of R2 and effects for quick reference in the rmd
#' @param effectPlotsTarget ggplot objects for plots showing the effects for quick reference in the rmd
#' @param allSpecCurve ggplot objects for plots showing the spec curves
#' @return Nothing, PDF (or output) will be saved to a folder.
#'
#' @export
render_rmd <- function(fileIN, fileOUT,
                       modelExtracts = NA,
                       effectPlots = NA,
                       allSpecCurve = NA,
                       optionsCompleteList = NA){
  
  rmarkdown::render(input = fileIN,
                    output_file = fileOUT)
  
}
