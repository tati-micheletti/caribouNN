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
                          useFuture,
                          useGPU = FALSE){
browser()
device <- if (isTRUE(useGPU) && torch::cuda_is_available()) "cuda" else "cpu"
message("Using device: ", device)
  cors <- min(maxClu, parallel::detectCores() - 2)
  
  if (all(Sys.getenv("RSTUDIO") != 1,
          useFuture,
        device == "cpu")) {
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
if (device == "cuda") {
gpu_threads <- max(1, parallel::detectCores() - 4)
  torch::torch_set_num_threads(num_threads = gpu_threads)
} else {
  torch::torch_set_num_threads(num_threads = 1)
}
  t1 <- Sys.time()
  applyFn <- if (device == "cuda") lapply else future_lapply
  extraArgs <- if (device == "cuda") list() else list(future.seed = TRUE)
  fittedModels <- rbindlist(
  do.call(applyFn, c(
    list(
      X = 1:NROW(experimentPlanOK),
      FUN = function(index) {
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
                             reRunExperiment = reRunModels,
                             device = device)
      gc()
      return(mds)
}),
    extraArgs
  )),
  use.names = TRUE)
  plan("sequential")
  t2 <- Sys.time()
  print(t2 - t1)
  return(fittedModels)
}
