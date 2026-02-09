shardDataByYear <- function(preparedData, analysisYears, outputDir, reRunDataset) {
  
  shardDir <- checkPath(file.path(outputDir, "yearShards"), create = TRUE)
  
  datasetYears <- list()
  
  for (yr in analysisYears) {
    path <- file.path(shardDir, paste0("year_", yr, ".csv"))
    datasetYears[[paste0("year_", as.character(yr))]] <- path
    
    if (!file.exists(path) || reRunDataset) {
      message("Creating shard for year: ", yr)
      # Slice the year and save as CSV to break all memory pointers
      fwrite(preparedData[year == yr], path)
    }
  }
  return(datasetYears)
}