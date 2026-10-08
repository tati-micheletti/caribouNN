#' Fit and evaluate ONE model of the experiment.
#'
#' Differences to the original implementation (all deliberate, see NEWS.md):
#'  * train/val/test strata come from a disjoint, verified manifest (buildGroupSplits)
#'  * explicit training loop (no luz): validation loss drives the LR scheduler and checkpoint
#'  * best-epoch weights are saved, reloaded into a fresh network and re-scored (error if different)
#'  * animals unseen in training get the mean trained embedding (plain-R computation, tested)
#'  * per-stratum losses (validation and test) are saved with ids, burst, year, seen flag
#'  * every seed, the RNG kind and the software versions are written next to the model
#'  * NO silent fallbacks: any failure raises an error (the caller collects and reports them)
#'
#' @param store output of buildStrataStore()
#' @param manifest data.table(row, set) for this model's regime (rows index store$index)
#' @param planRow one row of the experiment plan
#' @param features covariates for this complexity level (ordered by importance)
trainingExperimentNN <- function(store, manifest, planRow, features, batchSize, learningRate,
                                 epochs, outputDir, reRun = FALSE, device = "cpu",
                                 zClip = 10, earlyStopPatience = Inf, verbose = TRUE) {
  modelName <- planRow$modelName
  finalPath <- file.path(outputDir, paste0(modelName, "_finalDT.csv"))
  if (!reRun && file.exists(finalPath)) {
    message(modelName, ": results exist and reRun = FALSE; loading.")
    return(data.table::fread(finalPath))
  }
  t0 <- Sys.time()
  idx <- store$index
  verifyModelManifest(idx, manifest, planRow$typeValidation, planRow$windowStart,
                      planRow$historyEnd, planRow$testYear,
                      c(nTrain = planRow$nTrainS, nVal = planRow$nValS, nTest = planRow$nTestS))

  trRows <- manifest$row[manifest$set == "train"]
  vaRows <- manifest$row[manifest$set == "val"]
  teRows <- manifest$row[manifest$set == "test"]
  tr <- storeSlice(store, trRows, features, device)
  va <- storeSlice(store, vaRows, features, device)
  te <- storeSlice(store, teRows, features, device)

  # Standardise with TRAINING statistics only
  stats <- scaleStats(tr$x, features)
  tr$x <- applyScale(tr$x, stats, zClip); va$x <- applyScale(va$x, stats, zClip); te$x <- applyScale(te$x, stats, zClip)
  saveRDS(stats, file.path(outputDir, paste0(modelName, "_scl.rds")))

  seed <- planRow$modelSeed
  fit <- fitStratumNet(tr$x, tr$id, va$x, va$id, nAnimals = store$nAnimals, lr = learningRate,
                       epochs = epochs, batchSize = batchSize, seed = seed, device = device,
                       earlyStopPatience = earlyStopPatience, verbose = verbose)
  weightsPath <- file.path(outputDir, paste0(modelName, "_BW.pt"))
  checkWeightsRoundTrip(fit$bestState, weightsPath, nIn = length(features), nAnimals = store$nAnimals,
                        xVal = va$x, idVal = va$id, trainIdsSeen = fit$trainIdsSeen,
                        expectedValLoss = fit$bestValLoss, device = device)

  # Final scoring at the best epoch
  sc <- function(set, d, rows) {
    r <- scoreStrata(fit$net, d$x, d$id, fit$trainIdsSeen, store$nAnimals)
    ix <- idx[match(rows, idx$row)]
    list(table = data.table::data.table(
      modelName = modelName, splitId = planRow$splitId, typeValidation = planRow$typeValidation,
      numberOfCovariates = planRow$numberOfCovariates, set = set, row = rows,
      indiv_step_id = ix$indiv_step_id, id = ix$id, burstKey = ix$burstKey, year = ix$year,
      loss = r$loss, pTrue = r$pTrue, rankTrue = r$rankTrue, correct = r$correct, seenAnimal = r$seen),
      scores = r$scores)
  }
  teR <- sc("test", te, teRows)
  vaR <- sc("val", va, vaRows)
  trLoss <- mean(scoreStrata(fit$net, tr$x, tr$id, fit$trainIdsSeen, store$nAnimals)$loss)
  perStratum <- rbind(teR$table, vaR$table)
  saveRDS(perStratum, file.path(outputDir, paste0(modelName, "_perStratum.rds")))
  saveRDS(as.matrix(teR$scores), file.path(outputDir, paste0(modelName, "_testScores.rds")))
  data.table::fwrite(fit$history, file.path(outputDir, paste0(modelName, "_history.csv")))

  tt <- teR$table
  prov <- list(
    modelName = modelName, splitId = planRow$splitId, splitSeed = planRow$splitSeed, modelSeed = seed,
    torchSeedUse = "torch_manual_seed(modelSeed): weight init + batch order; split sampling is separate (splitSeed)",
    RNGkind = paste(RNGkind(), collapse = "/"), R = R.version.string,
    torch = as.character(utils::packageVersion("torch")), device = device,
    host = Sys.info()[["nodename"]], slurmJob = Sys.getenv("SLURM_JOB_ID", NA_character_),
    slurmTask = Sys.getenv("SLURM_ARRAY_TASK_ID", NA_character_), features = features,
    hyper = list(batchSize = batchSize, learningRate = learningRate, epochs = epochs, zClip = zClip,
                 earlyStopPatience = earlyStopPatience, nAnimals = store$nAnimals),
    time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  saveRDS(prov, file.path(outputDir, paste0(modelName, "_provenance.rds")))

  nAn <- function(rows) data.table::uniqueN(idx$id[match(rows, idx$row)])
  nBu <- function(rows) data.table::uniqueN(idx$burstKey[match(rows, idx$row)])
  finalDT <- data.table::data.table(
    groupId = planRow$groupId, splitId = planRow$splitId, arm = planRow$arm,
    typeValidation = planRow$typeValidation, modelName = modelName,
    numberOfCovariates = planRow$numberOfCovariates, replicate = planRow$replicate,
    windowStart = planRow$windowStart, historyEnd = planRow$historyEnd, testYear = planRow$testYear,
    horizon = planRow$horizon, trainStartYear = planRow$trainStartYear, trainEndYear = planRow$trainEndYear,
    testStartYear = planRow$testStartYear,
    nTrain = length(trRows), nVal = length(vaRows), nTest = length(teRows),
    nAnimalsTrain = nAn(trRows), nBurstsTrain = nBu(trRows), nAnimalsTest = nAn(teRows),
    splitSeed = planRow$splitSeed, modelSeed = seed,
    trainLossBest = trLoss, valLossBest = fit$bestValLoss, bestEpoch = fit$bestEpoch,
    epochsRun = nrow(fit$history),
    testLossMean = mean(tt$loss), testLossSD = stats::sd(tt$loss), testLossSE = stats::sd(tt$loss) / sqrt(nrow(tt)),
    totalSamples = nrow(tt), correctPreds = sum(tt$correct), testAccuracy = mean(tt$correct),
    seenShare = mean(tt$seenAnimal),
    testLossSeen = if (any(tt$seenAnimal)) mean(tt$loss[tt$seenAnimal]) else NA_real_,
    testLossUnseen = if (any(!tt$seenAnimal)) mean(tt$loss[!tt$seenAnimal]) else NA_real_,
    secondsTotal = as.numeric(difftime(Sys.time(), t0, units = "secs")),
    modelWeight = weightsPath, perStratumPath = file.path(outputDir, paste0(modelName, "_perStratum.rds")))
  data.table::fwrite(finalDT, finalPath)
  rm(tr, va, te, fit); gc(verbose = FALSE)
  finalDT
}
