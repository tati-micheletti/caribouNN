createDatasets <- function(experimentPlan, 
                           preparedDataFinal, 
                           useFuture,
                           whichPerc, 
                           outDir){
  uniqueGroupIDs <- unique(experimentPlan$groupId)
  total <- length(uniqueGroupIDs)
  if (all(useFuture,
          Sys.getenv("RSTUDIO") != 1)){
    nCores <- min(50, parallel::detectCores() - 2)
    plan("multicore", workers = nCores)
    experimentDatasets <- future_lapply(seq_along(uniqueGroupIDs), function(i){
      
      ID <- uniqueGroupIDs[i]
      dtName <- file.path(outDir, paste0("dt_", ID,".csv"))
      grp <- experimentPlan[groupId == ID,]
      
      if (any(length(unique(grp$trainStartYear)) != 1,
              length(unique(grp$testEndYear)) != 1))
        stop(paste0("Check generateExperimentPlan. All starting",
                    "training and end testing years across all triplets",
                    "need to be the same.", " Year mismatch in groupId: ", ID))
      dt <- preparedDataFinal[ year >= unique(grp$trainStartYear) & year <= unique(grp$testEndYear), ]
      ### Messaging
      messagePerc(i = i, 
                  total = total, whichPerc = whichPerc)
      fwrite(dt, dtName)
      return(dt)
    })
    plan("sequential")
  } else {
    plan("sequential")
    experimentDatasets <- lapply(seq_along(uniqueGroupIDs), function(i){
      ID <- uniqueGroupIDs[i]
      grp <- experimentPlan[groupId == ID,]
      if (any(length(unique(grp$trainStartYear)) != 1,
              length(unique(grp$testEndYear)) != 1))
        stop(paste0("Check generateExperimentPlan. All starting",
                    "training and end testing years across all triplets",
                    "need to be the same.", " Year mismatch in groupId: ", ID))
      dt <- preparedDataFinal[ year >= unique(grp$trainStartYear) & year <= unique(grp$testEndYear), ]
      messagePerc(i = i, total = total, whichPerc = whichPerc)
      return(dt)
    })
  }
  names(experimentDatasets) <- uniqueGroupIDs
  return(experimentDatasets)
} 