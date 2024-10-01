#' Save all the summaries from the mean of wrsf
#'
#' @name compile_wrsf_summaries
#' @description A
#' @return a
#'
#' @export
compile_wrsf_summaries <- function(wrsfResults){
  
  # targets::tar_load("wrsfResults")
  
  wrsfSummariesList <- lapply(names(wrsfResults), function(x){
    summaryList <- summary(wrsfResults[[x]])
    habOUT <- as.data.frame(summaryList$CI)
    habOUT$analysis <- "wrsf"
    habOUT$species <- str_split(x, "_")[[1]][2]
    habOUT$hypothesis <- str_split(x, "_")[[1]][3]
    habOUT$classLandscape <- str_split(x, "_")[[1]][4]
    return(habOUT)
  })
  wrsfSummaries <- do.call(rbind, wrsfSummariesList) 
  wrsfSummaries$variable <- rownames(wrsfSummaries)
  
  write.csv(wrsfSummaries,
            here::here("data", "wrsfEstimateOutputs_uncom.csv"), row.names = TRUE)
  
  return(wrsfSummaries)
  
}