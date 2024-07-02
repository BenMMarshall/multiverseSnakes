#' Function that returns the palette of colours
#'
#' @name get_palette
#' @description A
#' @return a
#'
#' @export
get_palette <- function(){
  palette <- c("#AD6DED", "#7D26D4", "#4F0E99", "#E87D13", "#965A1D", "#302010", "#403F41")
  names(palette) <- c("KINGCOBRA", "VULTURE", "BADGER", "2", "1", "0", "coreGrey")
  
  speciesPalette <-   c("#bba07e",
                        "#6c2b05",
                        "#322b21",
                        "#b28904")
  names(speciesPalette)<- c("OPHA", "PYBI", "BUCA", "BUFA") 
  
  speciesFullPalette <- speciesPalette
  names(speciesFullPalette) <- c("Ophiophagus hannah",
                                 "Python bivittatus",
                                 "Bungarus candidus",
                                 "Bungarus fasciatus")
  
  speciesColour <- data.frame(
    species = c("Ophiophagus hannah",
                "Python bivittatus",
                "Bungarus candidus",
                "Bungarus fasciatus"),
    colour = c("#bba07e", "#6c2b05", "#322b21", "#b28904")
  )
  
  # speciesHypoSupport <- c("#bba07e", "#865e2d", "#6c2b05", "#322b21", "#b28904", "#4D4D4D")
  # names(speciesHypoSupport) <- c("Support OPHA H1", "Support OPHA H2",
  #                                "Support PYBI H1", "Support BUCA H1",
  #                                "Support BUFA H1", "No support")
  
  return(list("corePalette" = palette,
              "speciesPalette" = speciesPalette,
              "speciesFullPalette" = speciesFullPalette,
              "speciesColourDF" = speciesColour)
  )
}
