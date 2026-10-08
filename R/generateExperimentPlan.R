#' @title Experimental design for the validation-regime experiment
#'
#' @description
#' One SPLIT = (window start s, history end e, test year T > e, arm). Each split is fitted under
#' three regimes (FutureUnseen = PreVal, FutureTainted = status quo, Internal = random CV) and
#' four complexity levels. All regimes and complexity levels of a split are built from the SAME
#' strata (see buildGroupSplits): the shared test set is identical, sizes are identical, and the
#' FutureTainted data are exactly the FutureUnseen data re-allocated at random.
#'
#' @param strataIdx output of buildStrataIndex()
#' @param startYear first analysis year (2013: 2012 and earlier are not used)
#' @param endYear last year with data
#' @param numberOfCovariatesList complexity ladder (Inf = all covariates)
#' @param nReplicates independent network initialisations per cell (same splits)
#' @param testFraction share of the test year's strata used as the shared test set (temporal arm)
#' @param spatialTestYears,spatialHorizons which splits also get a spatial arm (blocks held out in all regimes)
#' @param blockKm,bufferKm,nFolds spatial block design
#' @param matchAnimals if TRUE (default), Internal draws only from animals present in the FutureUnseen pool
#'   (all regimes then use the same animals; same strata counts; bursts are reported, not matched)
#' @param registryPath optional seed registry csv
#' @return list(plan, splitSummary, checks, bundles) ; bundles are keyed by splitId
generateExperimentPlan <- function(strataIdx,
                                   startYear = 2013L,
                                   endYear = 2022L,
                                   numberOfCovariatesList = c(2, 5, 10, Inf),
                                   nReplicates = 1L,
                                   testFraction = 0.5,
                                   spatialTestYears = c(2018L, 2020L, 2022L),
                                   spatialHorizons = 1L,
                                   blockKm = 100, bufferKm = 10, nFolds = 4L,
                                   matchAnimals = TRUE,
                                   minTrain = 500L, minVal = 200L, minTest = 200L,
                                   registryPath = NULL) {
  stopifnot(requireNamespace("data.table", quietly = TRUE))
  yearsAvail <- sort(unique(strataIdx$year))
  allYears <- startYear:endYear
  spatialSeed <- stringSeed("spatialBlocks")
  spatial <- if (all(c("x1_", "y1_") %in% names(strataIdx)) && length(spatialTestYears))
    assignSpatialBlocks(strataIdx, blockKm = blockKm, bufferKm = bufferKm, nFolds = nFolds, seed = spatialSeed)
  else NULL
  registerSeeds(registryPath, "caribouNN::design", "spatialBlocks", spatialSeed, "random assignment of spatial blocks to folds")

  # window [s, e] has >= 2 years (train + validation for FutureUnseen); test year > e
  splitGrid <- data.table::rbindlist(lapply(allYears, function(s) {
    if (s + 1L > endYear - 1L) return(NULL)
    data.table::rbindlist(lapply((s + 1L):(endYear - 1L), function(e)
      data.table::data.table(s = s, e = e, testYear = (e + 1L):endYear)))
  }))
  splitGrid[, arm := "temporal"]
  if (!is.null(spatial)) {
    sp <- splitGrid[testYear %in% spatialTestYears & (testYear - e) %in% spatialHorizons][, arm := "spatial"]
    splitGrid <- rbind(splitGrid, sp)
  }
  splitGrid[, splitId := sprintf("s%d_e%d_T%d_%s", s, e, testYear, arm)]
  splitGrid[, splitSeed := vapply(splitId, stringSeed, integer(1))]
  if (anyDuplicated(splitGrid$splitSeed)) stop("Hash collision between split seeds; change the id scheme.")

  bundles <- list(); checks <- list(); summaries <- list(); skipped <- list()
  for (i in seq_len(nrow(splitGrid))) {
    g <- splitGrid[i]
    sp <- if (g$arm == "spatial") list(spatial = spatial, foldId = 1L + (g$splitSeed %% nFolds)) else NULL
    b <- buildGroupSplits(strataIdx, s = g$s, e = g$e, testYear = g$testYear, seed = g$splitSeed,
                          testFraction = testFraction, minTrain = minTrain, minVal = minVal,
                          minTest = minTest, spatial = sp, matchAnimals = matchAnimals)
    if (!isTRUE(b$ok)) { skipped[[length(skipped) + 1L]] <- data.table::data.table(splitId = g$splitId, reason = b$reason); next }
    b$matchAnimals <- matchAnimals
    checks[[length(checks) + 1L]] <- verifySplits(strataIdx, b, g$s, g$e, g$testYear, g$splitId, spatial = sp)
    summaries[[length(summaries) + 1L]] <- data.table::rbindlist(lapply(names(b$splits), function(rg)
      summariseSplit(strataIdx, b$splits[[rg]], g$splitId, rg)))
    b$spatialInfo <- sp
    bundles[[g$splitId]] <- b
    registerSeeds(registryPath, "caribouNN::splits", g$splitId, g$splitSeed,
                  "split seed (+1 test sample, +2 Tainted allocation, +3 Internal draw)")
  }
  keep <- splitGrid[splitId %in% names(bundles)]
  if (length(skipped)) message(sprintf("Dropped %d splits (too little data):\n%s", length(skipped),
                                       paste(utils::capture.output(print(data.table::rbindlist(skipped))), collapse = "\n")))

  regimes <- c("FutureUnseen", "FutureTainted", "Internal")
  rows <- data.table::rbindlist(lapply(seq_len(nrow(keep)), function(i) {
    g <- keep[i]; b <- bundles[[g$splitId]]
    data.table::rbindlist(lapply(regimes, function(rg) {
      data.table::data.table(
        splitId = g$splitId, arm = g$arm, typeValidation = rg,
        windowStart = g$s, historyEnd = g$e, testYear = g$testYear, horizon = g$testYear - g$e,
        trainStartYear = g$s,
        trainEndYear = switch(rg, FutureUnseen = g$e - 1L, FutureTainted = g$e, Internal = g$testYear),
        valStartYear = switch(rg, FutureUnseen = g$e, g$s),
        valEndYear = switch(rg, FutureUnseen = g$e, FutureTainted = g$e, Internal = g$testYear),
        testStartYear = g$testYear, testEndYear = g$testYear,
        nTrainS = b$sizes[["nTrain"]], nValS = b$sizes[["nVal"]], nTestS = b$sizes[["nTest"]],
        splitSeed = g$splitSeed, matchAnimals = matchAnimals)
    }))
  }))
  plan <- data.table::rbindlist(lapply(numberOfCovariatesList, function(k) {
    data.table::rbindlist(lapply(seq_len(nReplicates), function(r) {
      x <- data.table::copy(rows); x[, `:=`(numberOfCovariates = k, replicate = r)]; x
    }))
  }))
  plan[, groupId := paste0("Grp_", numberOfCovariates, "_", windowStart, "_", historyEnd, "_", testYear,
                           ifelse(arm == "spatial", "_sp", ""), ifelse(replicate > 1, paste0("_r", replicate), ""))]
  plan[, modelName := paste0(groupId, "_", typeValidation)]
  plan[, modelSeed := mapply(function(sid, k, r) stringSeed(paste(sid, "cov", k, "rep", r)), splitId, numberOfCovariates, replicate)]
  if (anyDuplicated(unique(plan[, .(splitId, numberOfCovariates, replicate, modelSeed)])$modelSeed))
    stop("Hash collision between model seeds; change the id scheme.")
  if (anyDuplicated(plan$modelName)) stop("Duplicated model names in the plan.")
  plan[, lowDataFlag := nTrainS < 2000L]
  u <- unique(plan[, .(splitId, numberOfCovariates, replicate, modelSeed)])
  registerSeeds(registryPath, "caribouNN::models", paste(u$splitId, "cov", u$numberOfCovariates, "rep", u$replicate),
                u$modelSeed, "torch seed: weight initialisation and batch order (shared by the three regimes of a cell)")

  cat(sprintf("Plan: %d models = %d splits x 3 regimes x %d complexity levels x %d replicate(s).\n",
              nrow(plan), nrow(keep), length(numberOfCovariatesList), nReplicates))
  list(plan = plan[],
       splitSummary = data.table::rbindlist(summaries),
       checks = data.table::rbindlist(checks),
       skipped = if (length(skipped)) data.table::rbindlist(skipped) else NULL,
       bundles = bundles)
}
