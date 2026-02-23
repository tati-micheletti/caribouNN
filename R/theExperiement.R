theExperiment <- function(preparedData,
                          batchSize, 
                          epoch, 
                          learningRate,
                          outputDir, 
                          experimentPlan,
                          reRunModels, 
                          reRunDataset,
                          featurePriority,
                          modComplex,
                          maxClu,
                          useFuture){
  
  cors <- min(maxClu, parallel::detectCores() - 2)
  
  if (all(Sys.getenv("RSTUDIO") != 1,
          useFuture)) {
    print(paste0("Running outside of RStudio, using future multicore with ", 
                 cors,
                 " workers..."))
    plan("multicore", workers = cors)
  }

  outputDir <- Require::normPath(outputDir)
  mapPath <- file.path(outputDir, "masterIdMap.csv")
  
  if (any(!"idIndex" %in% names(preparedData),
          !file.exists(mapPath))) {
    message("Creating Master ID Map...")
    preparedData[, idIndex := as.numeric(as.factor(id))]
    # Save the map for auditing
    fwrite(unique(preparedData[, .(id, idIndex)]), 
           mapPath)
  }
  
  # Here we check if we need to reduce the preparedData to 
  # only the complexities we would like to run
  if (modComplex != "all"){
    print(paste0("Running models with ", modComplex," covariates"))
    experimentPlanOK <- copy(experimentPlan[numberOfCovariates == modComplex,])
  } else {
    experimentPlanOK <- copy(experimentPlan)
  }
  
  # This returns a list of file paths for each year
  datasetYears <- shardDataByYear(
    preparedData = preparedData,
    analysisYears = unique(preparedData$year),
    outputDir = outputDir,
    reRunDataset = reRunDataset
  )
  
  message("Setting threads for torch...")
  torch::torch_set_num_threads(num_threads = 1)
  torch::torch_set_num_interop_threads(num_threads = 1)

  t1 <- Sys.time()
  fittedModels <- rbindlist(future_lapply( #
    1:NROW(experimentPlanOK),
    function(index) {
      modelNaming <- paste0(experimentPlanOK[index, groupId], "_", 
                            experimentPlanOK[index, typeValidation])
      neededYears <- unique(c(experimentPlanOK[index, trainStartYear]:experimentPlanOK[index, trainEndYear], 
                              experimentPlanOK[index, valStartYear]:experimentPlanOK[index, valEndYear], 
                              experimentPlanOK[index, testStartYear]:experimentPlanOK[index, testEndYear]))
      weightsPath <- file.path(outputDir, paste0(modelNaming,"_BW.pt"))
      modelPath <- file.path(outputDir, paste0(modelNaming,"_Mod.pt"))
      availableKeys <- intersect(paste0("year_", as.character(neededYears)), names(datasetYears))
      if(length(availableKeys) == 0) {
        return(data.table(modelName = modelNaming, testLossMean = NA))
      }
      mds <- trainingExperimentNN(subsettedData = datasetYears, # Path to all years
                             neededYears = neededYears,
                             experimentPlanRow = experimentPlanOK[index,],
                             modelNaming = modelNaming,
                             batchSize = batchSize,
                             epoch = epoch,
                             featurePriority = featurePriority,
                             learningRate = learningRate,
                             weightsPath = weightsPath,
                             modelPath = modelPath,
                             outputDir = outputDir,
                             reRunExperiment = reRunModels)
      gc()
      return(mds)
    }, future.seed = TRUE), #}), 
    use.names = TRUE)
  plan("sequential")
  t2 <- Sys.time()
  print(t2 - t1)
  return(fittedModels)
}
