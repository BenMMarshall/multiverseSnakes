#' Get all available data from online sources
#'
#' @name download_datasets
#' @description A
#' @param speciesLocations a
#' @param species a
#' @return a
#'
#' @export
download_datasets <- function(speciesLocations, species){
  
  if(!is.na(speciesLocations[speciesLocations$speciesCode == species,]$doi)){
    
    download_zenodo(doi = speciesLocations[speciesLocations$speciesCode == species,]$doi,
                    path = here("data", species),
                    parallel = FALSE, quiet = FALSE)
  }
  
  if(!is.na(speciesLocations[speciesLocations$speciesCode == species,]$movebankID)){
    
    for(movebankID in unlist(strsplit(speciesLocations[speciesLocations$speciesCode == species,]$movebankID, ";"))){
      
      login <- movebank_login_abstraction()
      movementData <- move::getMovebank("event", login, study_id = movebankID, attributes="all")
      
      write.csv(movementData, file = here("data", species, paste0(species, "_movebankdata_",
                                                                  movebankID, ".csv")),
                row.names = FALSE)
    }
  }
  
}
