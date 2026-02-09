theExperiment <- function(preparedData,
                           batchSize, epoch, learningRate,
                           outputDir, experimentPlan,
                          reRunModels, reRunDataset,
                          featurePriority){
  
  cors <- min(50, parallel::detectCores() - 2)
  
  if (Sys.getenv("RSTUDIO") != 1) {
    print(paste0("Running outside of RStudio, using future multicore with ", 
                 cors,
                 " workers..."))
    plan("multicore", workers = cors)
  }

  mapPath <- file.path(outputDir, "masterIdMap.csv")
  if (any(!"idIndex" %in% names(preparedData),
          !file.exists(mapPath))) {
    message("Creating Master ID Map...")
    preparedData[, idIndex := as.numeric(as.factor(id))]
    # Save the map for auditing
    fwrite(unique(preparedData[, .(id, idIndex)]), 
           mapPath)
  }
  
  # This returns a list of file paths for each year
  datasetYears <- shardDataByYear(
    preparedData = preparedData,
    analysisYears = unique(preparedData$year),
    outputDir = outputDir,
    reRunDataset = reRunDataset
  )
  
  # This is what needs to be in a future_lappy
  t1 <- Sys.time()
  fittedModels <- rbindlist(lapply( #future_lapply <~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ AFTER TEST CHANGE!!!!!
    1:6, #1:NROW(experimentPlan), <~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ AFTER TEST CHANGE!!!!!
    function(index) {
      modelNaming <- paste0(experimentPlan[index, groupId], "_", 
                            experimentPlan[index, typeValidation])
      neededYears <- unique(c(experimentPlan[index, trainStartYear]:experimentPlan[index, trainEndYear], 
                              experimentPlan[index, valStartYear]:experimentPlan[index, valEndYear], 
                              experimentPlan[index, testStartYear]:experimentPlan[index, testEndYear]))
      weightsPath <- file.path(outputDir, paste0(modelNaming,"_BW.pt"))
      modelPath <- file.path(outputDir, paste0(modelNaming,"_Mod.pt"))
      if (any(reRunModels,
              !file.exists(weightsPath),
              !file.exists(modelPath))){
        if (reRunModels){
          message("reRunModels is set to TRUE. Rerunning models...")
        } else {
          message(paste0(modelNaming, " doesn't exist. Fitting..."))
        }
        trainingExperimentNN(subsettedData = datasetYears, # Path to all years
                             neededYears = neededYears,
                             experimentPlanRow = experimentPlan[index,],
                             modelNaming = modelNaming,
                             batchSize = batchSize,
                             epoch = epoch,
                             featurePriority = featurePriority,
                             learningRate = learningRate,
                             weightsPath = weightsPath,
                             modelPath = modelPath)
      } else {
        message(paste0("reRunModels is set to FALSE and model ", modelNaming," exists. Returning paths..."))
      }
      return(data.table(modelName = modelNaming,
                        modelPath = modelPath,
                        modelWeightPath = weightsPath))

    }), use.names = TRUE)
  plan("sequential")
  t2 <- Sys.time()
  print(t2 - t1)
  
}
