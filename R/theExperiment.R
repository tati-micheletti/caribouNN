#' Run (a slice of) the experiment.
#'
#' Parallelism is by SLURM array / independent R processes, not by in-process forking:
#' `runSlice = c(taskId, nTasks)` assigns models round-robin (largest first) so every task gets
#' a similar load. Splits are read from the manifests written once by the design step; nothing
#' is re-sampled here.
#'
#' @return data.table of per-model results (also written per model); errors are collected, written
#'   to `<outputDir>/errors/` and, if `stopOnError`, raised after all other models have run.
theExperiment <- function(strataStore, plan, manifestDir, featurePriority, batchSize, epoch,
                          learningRate, outputDir, reRunModels = FALSE, modComplex = "all",
                          runSlice = NULL, useGPU = FALSE, torchThreads = 1L, zClip = 10,
                          earlyStopPatience = Inf, stopOnError = TRUE, registryPath = NULL,
                          modulePaths = NULL, extendFrom = NA, onlyMissing = FALSE, featureSets = NULL) {
  device <- if (isTRUE(useGPU) && torch::cuda_is_available()) "cuda" else "cpu"
  message("Using device: ", device)
  torch::torch_set_num_threads(as.integer(torchThreads))
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(outputDir, "errors"), showWarnings = FALSE)

  work <- data.table::copy(plan)
  if (modComplex != "all") work <- work[numberOfCovariates == as.numeric(modComplex)]
  data.table::setorder(work, -nTrainS, modelName)
  # Mop-up mode: keep only models WITHOUT a result, then slice them, so a few leftovers are spread over many small tasks
  if (isTRUE(onlyMissing)) {
    work <- work[!file.exists(file.path(outputDir, paste0(modelName, "_finalDT.csv")))]
    message(sprintf("onlyMissing: %d models have no result yet.", nrow(work)))
  }
  if (!is.null(runSlice)) {
    stopifnot(length(runSlice) == 2, runSlice[1] >= 1, runSlice[1] <= runSlice[2])
    work <- work[(seq_len(.N) - 1L) %% runSlice[2] == (runSlice[1] - 1L)]
  }
  message(sprintf("This process runs %d of %d models.", nrow(work), nrow(plan)))

  writeRunProvenance(outputDir, runSlice, list(batchSize = batchSize, epoch = epoch,
                                               learningRate = learningRate, zClip = zClip,
                                               modComplex = modComplex, device = device),
                     modulePaths = modulePaths)
  manifests <- new.env()
  getManifest <- function(splitId, regime) {
    key <- paste(splitId, regime)
    if (is.null(manifests[[key]])) manifests[[key]] <- readManifest(manifestDir, splitId, regime)
    manifests[[key]]
  }

  results <- list(); errs <- list()
  for (i in seq_len(nrow(work))) {
    pr <- work[i]
    nPick <- if (is.infinite(pr$numberOfCovariates)) nrow(featurePriority) else pr$numberOfCovariates
    if (!is.null(featureSets) && "featureSet" %in% names(pr) && !is.na(pr$featureSet)) {   # feature-set arm: own covariate order
      fl <- featureSets[set == pr$featureSet][order(position)]$Feature
      feats <- fl[seq_len(min(nPick, length(fl)))]
    } else {
      feats <- featurePriority$Feature[seq_len(min(nPick, nrow(featurePriority)))]
    }
    message(sprintf("[%d/%d] %s", i, nrow(work), pr$modelName))
    res <- tryCatch(
      trainingExperimentNN(store = strataStore, manifest = getManifest(pr$splitId, pr$typeValidation),
                           planRow = pr, features = feats, batchSize = batchSize,
                           learningRate = learningRate, epochs = epoch, outputDir = outputDir,
                           reRun = reRunModels, device = device, zClip = zClip,
                           earlyStopPatience = earlyStopPatience, extendFrom = extendFrom),
      error = function(e) e)
    if (inherits(res, "error")) {
      msg <- conditionMessage(res)
      writeLines(c(pr$modelName, msg), file.path(outputDir, "errors", paste0(pr$modelName, ".txt")))
      errs[[length(errs) + 1L]] <- data.table::data.table(modelName = pr$modelName, error = msg)
      message("!!! FAILED: ", pr$modelName, ": ", msg)
    } else {
      results[[length(results) + 1L]] <- res
    }
    gc(verbose = FALSE)
  }
  out <- data.table::rbindlist(results, fill = TRUE)
  if (length(errs)) {
    errTable <- data.table::rbindlist(errs)
    data.table::fwrite(errTable, file.path(outputDir, "errors",
                       sprintf("_errors_task%s.csv", if (is.null(runSlice)) "all" else runSlice[1])))
    if (stopOnError) stop(sprintf("%d model(s) failed (see %s):\n%s", nrow(errTable),
                                  file.path(outputDir, "errors"),
                                  paste(utils::capture.output(print(errTable)), collapse = "\n")))
  }
  out
}

#' Write manifests (row, indiv_step_id, set) once; refuse to overwrite a different existing one.
writeManifests <- function(bundles, strataIdx, manifestDir) {
  dir.create(manifestDir, recursive = TRUE, showWarnings = FALSE)
  for (sid in names(bundles)) for (rg in names(bundles[[sid]]$splits)) {
    m <- data.table::copy(bundles[[sid]]$splits[[rg]])
    m[, indiv_step_id := strataIdx$indiv_step_id[match(row, strataIdx$row)]]
    p <- file.path(manifestDir, paste0(sid, "__", rg, ".rds"))
    if (file.exists(p)) {
      old <- readRDS(p)
      if (!isTRUE(all.equal(old[order(set, row)], m[order(set, row)], check.attributes = FALSE)))
        stop("Existing manifest differs from the newly built one (code, data or seed changed): ", p)
    } else saveRDS(m, p)
  }
  invisible(manifestDir)
}

readManifest <- function(manifestDir, splitId, regime) {
  p <- file.path(manifestDir, paste0(splitId, "__", regime, ".rds"))
  if (!file.exists(p)) stop("Manifest not found: ", p)
  readRDS(p)
}

#' Re-verify ALL manifests on disk against the strata index (post-hoc audit of what was run).
auditSplitManifests <- function(strataIdx, plan, manifestDir, spatial = NULL) {
  out <- list()
  for (sid in unique(plan$splitId)) {
    p <- plan[splitId == sid][1]
    sp <- if (p$arm == "spatial") list(spatial = spatial, foldId = 1L + (p$splitSeed %% spatial$nFolds)) else NULL
    splits <- stats::setNames(lapply(c("FutureUnseen", "FutureTainted", "Internal"),
                                     function(rg) readManifest(manifestDir, sid, rg)[, .(row, set)]),
                              c("FutureUnseen", "FutureTainted", "Internal"))
    bundle <- list(ok = TRUE, splits = splits,
                   matchAnimals = "matchAnimals" %in% names(p) && isTRUE(as.logical(p$matchAnimals)),
                   spatialExcluded = if (is.null(sp)) NULL else
                     which(spatialMasksXY(strataIdx, sp$spatial, sp$foldId)$excluded))
    out[[sid]] <- verifySplits(strataIdx, bundle, p$windowStart, p$historyEnd, p$testYear, sid, spatial = sp)
  }
  data.table::rbindlist(out)
}

#' Run-level provenance: software, host, git commits of the modules, parameters.
writeRunProvenance <- function(outputDir, runSlice, params, modulePaths = NULL) {
  git <- function(path) tryCatch(trimws(suppressWarnings(system2("git", c("-C", shQuote(path), "rev-parse", "HEAD"),
                                                stdout = TRUE, stderr = FALSE))), error = function(e) NA_character_)
  info <- list(time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), host = Sys.info()[["nodename"]],
               R = R.version.string, torch = as.character(utils::packageVersion("torch")),
               RNGkindDefault = paste(RNGkind(), collapse = "/"), params = params, runSlice = runSlice,
               slurmJob = Sys.getenv("SLURM_JOB_ID", NA_character_),
               gitRepoHead = git(getwd()),
               moduleHeads = if (is.null(modulePaths)) NULL else vapply(modulePaths, git, character(1)),
               sessionInfo = utils::capture.output(print(utils::sessionInfo())))
  saveRDS(info, file.path(outputDir, sprintf("runProvenance_task%s_%s.rds",
                                             if (is.null(runSlice)) "all" else runSlice[1],
                                             format(Sys.time(), "%Y%m%d%H%M%S"))))
  invisible(info)
}
