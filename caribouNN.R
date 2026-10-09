defineModule(sim, list(
  name = "caribouNN",
  description = "Performs an experiment on models with different forecasting settings",
  keywords = "",
  authors = structure(list(list(given = "Tati", family = "Micheletti", role = c("aut", "cre"), 
                                email = "tati.micheletti@gmail.com", comment = NULL)), 
                      class = "person"),
  childModules = character(0),
  version = list(caribouNN = "0.0.1"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "caribouNN.Rmd"),
  reqdPkgs = list("SpaDES.core (>= 3.0.4)", "ggplot2", "data.table", "torch"),
  parameters = bindrows(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used - e.g., a hash of the study",
                          "area obtained using `reproducible::studyAreaName()`"),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?"),
    defineParameter("epoch", "numeric", 50, 1, 1000,
                    "Maximum number of epochs per model (the best validation epoch is kept)."),
    defineParameter("batchSize", "numeric", 128, 32, 4096,
                    "Batch size"),
    defineParameter("learningRate", "numeric", 0.01, 0.0001, 0.1,
                    "Initial learning rate; halved when the validation loss stops improving."),
    defineParameter("earlyStopPatience", "numeric", Inf, 1, Inf,
                    "Stop after this many epochs without a new best validation loss (Inf = never)."),
    defineParameter("zClip", "numeric", 10, 1, Inf, "Standardised covariates are clipped to +/- zClip."),
    defineParameter("regimeArm", "character", "", NA, NA,
                    paste0("Extra regime on the finished main design: \"FutureTaintedSpatial\" = the status quo with SPATIALLY blocked ",
                           "validation inside the same training years (R/spatialRegime.R). Models go to testedModels_regime_<name>.")),
    defineParameter("featureSetArm", "logical", FALSE, NA, NA,
                    paste0("Follow-up experiment (needs a finished design in the output folder): train the status quo and PreVal with ",
                           "re-ordered / ablated covariate sets (see R/featureSets.R) on the SAME splits. Models go to ",
                           "testedModels_featureSets[_tag]; the main experiment is not touched.")),
    defineParameter("featureSetTag", "character", "", NA, NA,
                    paste0("Name of this feature-set experiment (empty = the first experiment). Outputs go to folders and files with ",
                           "this suffix, so several experiments can live in one output folder.")),
    defineParameter("featureSetNames", "character", "habitatOnly,habitatFirst,movementFirst,randomA,randomB", NA, NA,
                    "Comma-separated feature sets of the arm."),
    defineParameter("featureSetLevels", "character", "2,5,10,20", NA, NA,
                    "Comma-separated covariate counts of the arm (truncated to the length of each set)."),
    defineParameter("onlyMissing", "logical", FALSE, NA, NA,
                    "Mop-up: run only the models that have no result yet (then slice them with runSlice)."),
    defineParameter("extendFrom", "numeric", NA, NA, NA,
                    paste0("If set (e.g. 50): models whose saved result stopped exactly at this epoch cap are re-trained from ",
                           "scratch with the larger `epoch` cap (same seeds); the earlier result is kept as *_finalDT_cap<N>.csv.")),
    defineParameter("startYear", "numeric", 2013, NA, NA, "First analysis year."),
    defineParameter("endYear", "numeric", 2022, NA, NA, "Last year with data."),
    defineParameter("complexityLevels", "numeric", c(2, 5, 10, Inf), NA, NA,
                    "Numbers of covariates (Inf = all)."),
    defineParameter("nReplicates", "numeric", 1, 1, 20,
                    "Independent network initialisations per cell (same splits)."),
    defineParameter("testFraction", "numeric", 0.5, 0.05, 1,
                    "Share of the test year strata used as the shared test set (temporal arm)."),
    defineParameter("matchAnimals", "logical", TRUE, NA, NA,
                    paste0("If TRUE (default), Internal draws only from animals present in the ",
                           "FutureUnseen/FutureTainted pool, so all regimes use the same animals. ",
                           "FALSE lets Internal use every animal in its (longer) window.")),
    defineParameter("spatialTestYears", "numeric", c(2018, 2020, 2022), NA, NA,
                    "Test years that also get a spatial arm (spatial blocks held out in ALL regimes)."),
    defineParameter("spatialHorizons", "numeric", 1, NA, NA, "Horizons (testYear - historyEnd) with a spatial arm."),
    defineParameter("blockKm", "numeric", 100, 1, NA, "Spatial block size (km)."),
    defineParameter("bufferKm", "numeric", 10, 0, NA, "Buffer around held-out blocks removed from training/validation (km)."),
    defineParameter("nFolds", "numeric", 4, 2, NA, "Number of spatial folds."),
    defineParameter("runSlice", "numeric", NA, NA, NA,
                    paste0("c(taskId, nTasks) to run a share of the models in this process (SLURM array). ",
                           "NA runs everything.")),
    defineParameter("torchThreads", "numeric", 1, 1, NA, "Threads per process for torch."),
    defineParameter("stopOnError", "logical", TRUE, NA, NA,
                    "Raise an error after the run if any model failed (failures are always written to disk)."),
    defineParameter("reRunModels", "logical", FALSE, NA, NA,
                    "Should models with existing results be re-run?"),
    defineParameter("useSavedPlan", "logical", TRUE, NA, NA,
                    "Use the saved plan and split manifests if they exist (never re-sample silently)."),
    defineParameter("modComplex", "character", "all", NA, NA,
                    "Run only this number of covariates ('all' runs every level)."),
    defineParameter("useGPU", "logical", FALSE, NA, NA,
                    "Use a GPU if available. CPU is the default and is what the EVE job scripts use."),
    defineParameter("stage", "character", "all", NA, NA,
                    paste0("'all' (design + train + analyse in one session), 'design' (plan, manifests, ",
                           "tensor store; run once), 'train' (one SLURM task: needs runSlice), 'analyze'."))
  ),
  inputObjects = bindrows(
    expectsInput("featurePriority", "character", 
                  "Ordered list of variable names based on importance")
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "experimentPlan", objectClass = "data.table", 
                  desc = paste0("Data.table containing the experiment plan, including",
                                " a column with the link to the folder with each model's",
                                " results")),
    createsOutput(objectName = "experimentDatasets", objectClass = "list", 
                  desc = paste0("List of data.tables containing the specific datasets ",
                                " for each experiment planned")),
    createsOutput(objectName = "fittedModelsPaths", objectClass = "list", 
                  desc = paste0("Named list of saved model object paths",
                                " generated by the experiment, named with each experiment")),
    createsOutput("preparedDataFinal", "data.table", 
                 paste0("Data table containing Dataset of raw ",
                        "features after preparation (interactions added, etc.) ",
                        "This is generally done by another module and copied here.",
                        "This should be restructured at some point to improve modularity.")),
    createsOutput(objectName = "modelComparisons", objectClass = "list", 
                  desc = paste0("Named list of model comparisons.",
                                "TO BE DECIDED HOW SPECIFIC!")) # <~~~~~~~~~~~~~~~~~~~~~ DOCUMENT WHEN READY!
  )
))

doEvent.caribouNN = function(sim, eventTime, eventType) {
  stage <- P(sim)$stage
  if (!stage %in% c("all", "design", "train", "analyze"))
    stop("caribouNN parameter 'stage' must be one of: all, design, train, analyze.")
  switch(
    eventType,
    init = {
      if (stage %in% c("all", "design", "train"))
        sim <- scheduleEvent(sim, time(sim), "caribouNN", "prepareExperiment")
      if (stage %in% c("all", "train"))
        sim <- scheduleEvent(sim, time(sim), "caribouNN", "trainExperiment")
      if (stage %in% c("all", "analyze"))
        sim <- scheduleEvent(sim, time(sim), "caribouNN", "compareExperiment")
    },
    prepareExperiment = {
      outDir <- outputPath(sim)
      planPath <- file.path(outDir, "experimentPlan.csv")
      manifestDir <- file.path(outDir, "splits")
      storeDir <- file.path(outDir, "store")
      slice <- if (anyNA(P(sim)$runSlice)) NULL else P(sim)$runSlice
      havePlan <- all(P(sim)$useSavedPlan, file.exists(planPath), dir.exists(manifestDir),
                      file.exists(file.path(storeDir, "store_meta.rds")))
      if (havePlan) {
        message("Saved plan, split manifests and tensor store found; using them.")
        sim$experimentPlan <- fread(planPath)
        if (is.null(slice)) {   # a training task skips this; the analysis stage re-verifies everything
          meta <- readRDS(file.path(storeDir, "store_meta.rds"))
          dp <- readRDS(file.path(outDir, "designParams.rds"))
          au <- auditSplitManifests(meta$index, sim$experimentPlan, manifestDir, spatial = dp$spatial)
          fwrite(au, file.path(outDir, "splitChecks_reaudit.csv"))
        }
      } else {
        if (stage == "train") stop("stage = 'train' needs the output of stage = 'design' in ", outDir)
        if (all(!is.null(sim$preparedData$preparedDataFinal), is.null(sim$preparedDataFinal)))
          sim$preparedDataFinal <- sim$preparedData$preparedDataFinal
        if (is.null(sim$preparedDataFinal)) stop("preparedDataFinal is NULL. Run caribouNN_Global first.")
        message("Creating the experiment design (disjoint splits, shared test sets, spatial arm)...")
        strataIdx <- buildStrataIndex(sim$preparedDataFinal)
        seedFile <- file.path(outDir, "seedRegistry.csv")
        des <- generateExperimentPlan(strataIdx,
                                      startYear = P(sim)$startYear, endYear = P(sim)$endYear,
                                      numberOfCovariatesList = P(sim)$complexityLevels,
                                      nReplicates = P(sim)$nReplicates, testFraction = P(sim)$testFraction,
                                      spatialTestYears = P(sim)$spatialTestYears,
                                      spatialHorizons = P(sim)$spatialHorizons,
                                      blockKm = P(sim)$blockKm, bufferKm = P(sim)$bufferKm,
                                      nFolds = P(sim)$nFolds, matchAnimals = P(sim)$matchAnimals,
                                      registryPath = seedFile)
        sim$experimentPlan <- des$plan
        writeManifests(des$bundles, strataIdx, manifestDir)
        fwrite(des$plan, planPath)
        fwrite(des$splitSummary, file.path(outDir, "splitSummary.csv"))
        fwrite(des$checks, file.path(outDir, "splitChecks.csv"))
        if (!is.null(des$skipped)) fwrite(des$skipped, file.path(outDir, "splitsSkipped.csv"))
        spatialObj <- if (any(des$plan$arm == "spatial"))
          assignSpatialBlocks(strataIdx, blockKm = P(sim)$blockKm, bufferKm = P(sim)$bufferKm,
                              nFolds = P(sim)$nFolds, seed = stringSeed("spatialBlocks")) else NULL
        saveRDS(list(spatial = spatialObj, params = P(sim)), file.path(outDir, "designParams.rds"))
        fwrite(unique(strataIdx[, .(id, idIndex)]), file.path(outDir, "masterIdMap.csv"))
        au <- auditSplitManifests(strataIdx, sim$experimentPlan, manifestDir, spatial = spatialObj)
        fwrite(au, file.path(outDir, "splitChecks_reaudit.csv"))
        message("Saving the tensor store for the training tasks...")
        saveStrataStore(buildStrataStore(sim$preparedDataFinal, featureNames = sim$featurePriority$Feature),
                        storeDir)
      }
      if (nzchar(P(sim)$regimeArm)) {
        if (isTRUE(P(sim)$featureSetArm)) stop("Use either regimeArm or featureSetArm, not both.")
        if (!havePlan) stop("regimeArm needs the finished main design (plan, splits, tensor store) in ", outDir)
        rgPlan <- file.path(outDir, paste0("experimentPlan_regime_", P(sim)$regimeArm, ".csv"))
        if (is.null(slice)) {   # design step: build the manifests of the new regime once (verified), write its plan
          metaRg <- readRDS(file.path(storeDir, "store_meta.rds")); dpRg <- readRDS(file.path(outDir, "designParams.rds"))
          if (is.null(dpRg$spatial)) stop("The main design has no spatial block object (designParams$spatial); run it with spatialTestYears.")
          desRg <- makeSpatialRegimeDesign(metaRg$index, fread(planPath), manifestDir, dpRg$spatial, regime = P(sim)$regimeArm)
          fwrite(desRg$plan, rgPlan)
          fwrite(desRg$summary, file.path(outDir, paste0("regimeArm_", P(sim)$regimeArm, "_splits.csv")))
          fwrite(desRg$checks, file.path(outDir, paste0("regimeArm_", P(sim)$regimeArm, "_checks.csv")))
          if (!is.null(desRg$skipped)) fwrite(desRg$skipped, file.path(outDir, paste0("regimeArm_", P(sim)$regimeArm, "_skipped.csv")))
          message(sprintf("Regime arm %s: %d models on %d splits.", P(sim)$regimeArm, nrow(desRg$plan), nrow(desRg$summary)))
        }
        if (!file.exists(rgPlan)) stop("Regime plan not found (run the design step first): ", rgPlan)
        sim$experimentPlan <- fread(rgPlan)
      }
      if (isTRUE(P(sim)$featureSetArm)) {
        if (!havePlan) stop("featureSetArm needs the finished main design (plan, splits, tensor store) in ", outDir)
        fsNames <- trimws(strsplit(P(sim)$featureSetNames, ",")[[1]])
        fsLevels <- as.numeric(strsplit(P(sim)$featureSetLevels, ",")[[1]])
        fsets <- buildFeatureSets(sim$featurePriority, sets = fsNames)
        sim$experimentPlan <- makeFeatureSetPlan(sim$experimentPlan, fsets, levels = fsLevels,
                                                 registryPath = if (is.null(slice)) file.path(outDir, "seedRegistry.csv") else NULL)
        sim$featureSetsTable <- fsets
        if (is.null(slice)) {   # written once by the design step; training tasks rebuild the identical plan (seeded)
          fsTag <- if (nzchar(P(sim)$featureSetTag)) paste0("_", P(sim)$featureSetTag) else ""
          fwrite(fsets, file.path(outDir, paste0("featureSets", fsTag, ".csv")))
          fwrite(sim$experimentPlan, file.path(outDir, paste0("experimentPlan_featureSets", fsTag, ".csv")))
          message(sprintf("Feature-set arm: %d models (%s; levels %s).", nrow(sim$experimentPlan),
                          paste(fsNames, collapse = ", "), paste(fsLevels, collapse = ", ")))
        }
      }
      if (!P(sim)$modComplex %in% c("all", as.character(sim$experimentPlan$numberOfCovariates)))
        stop("modComplex = ", P(sim)$modComplex, ". Available: all, ",
             paste(unique(sim$experimentPlan$numberOfCovariates), collapse = ", "))
    },
    trainExperiment = {
      slice <- if (anyNA(P(sim)$runSlice)) NULL else P(sim)$runSlice
      fsArm <- isTRUE(P(sim)$featureSetArm)
      rgArm <- nzchar(P(sim)$regimeArm)
      fsTag <- if (nzchar(P(sim)$featureSetTag)) paste0("_", P(sim)$featureSetTag) else ""
      savedPath <- file.path(outputPath(sim), if (rgArm) paste0("fittedModelPaths_regime_", P(sim)$regimeArm, ".csv") else if (fsArm) paste0("fittedModelPaths_featureSets", fsTag, ".csv") else "fittedModelPaths.csv")
      if (is.null(slice) && !P(sim)$reRunModels && file.exists(savedPath) &&
          nrow(fread(savedPath)) == nrow(sim$experimentPlan)) {
        message("Final results table found; loading.")
        sim$fittedModelsPaths <- fread(savedPath)
      } else {
        store <- loadStrataStore(file.path(outputPath(sim), "store"))
        if (!all(sim$featurePriority$Feature %in% store$featureNames))
          stop("featurePriority contains features that are not in the saved tensor store.")
        sim$fittedModelsPaths <- theExperiment(
          strataStore = store, plan = sim$experimentPlan,
          manifestDir = file.path(outputPath(sim), "splits"),
          featurePriority = sim$featurePriority, batchSize = P(sim)$batchSize, epoch = P(sim)$epoch,
          learningRate = P(sim)$learningRate,
          outputDir = checkPath(file.path(outputPath(sim), if (rgArm) paste0("testedModels_regime_", P(sim)$regimeArm) else if (fsArm) paste0("testedModels_featureSets", fsTag) else "testedModels"), create = TRUE),
          featureSets = if (fsArm) sim$featureSetsTable else NULL,
          reRunModels = P(sim)$reRunModels, modComplex = P(sim)$modComplex, runSlice = slice,
          useGPU = P(sim)$useGPU, torchThreads = P(sim)$torchThreads, zClip = P(sim)$zClip,
          earlyStopPatience = P(sim)$earlyStopPatience, stopOnError = P(sim)$stopOnError, extendFrom = P(sim)$extendFrom, onlyMissing = P(sim)$onlyMissing,
          registryPath = file.path(outputPath(sim), "seedRegistry.csv"),
          modulePaths = c(caribouNN = file.path(modulePath(sim), "caribouNN"),
                          caribouNN_Global = file.path(modulePath(sim), "caribouNN_Global")))
        if (is.null(slice)) fwrite(sim$fittedModelsPaths, savedPath)
      }
    },
    compareExperiment = {
      outDir <- outputPath(sim)
      if (nzchar(P(sim)$regimeArm)) {
        planRg <- fread(file.path(outDir, paste0("experimentPlan_regime_", P(sim)$regimeArm, ".csv")))
        doneRg <- list.files(file.path(outDir, paste0("testedModels_regime_", P(sim)$regimeArm)), pattern = "_finalDT\\.csv$")
        missingRg <- setdiff(paste0(planRg$modelName, "_finalDT.csv"), doneRg)
        if (length(missingRg)) {
          writeLines(missingRg, file.path(outDir, paste0("modelsMissing_regime_", P(sim)$regimeArm, ".txt")))
          stop(length(missingRg), " regime-arm models have no result (see modelsMissing_regime_", P(sim)$regimeArm, ".txt).")
        }
        sim$modelComparisons <- analyzeRegimeArm(modelDir = file.path(outDir, paste0("testedModels_regime_", P(sim)$regimeArm)),
                                                 mainDir = file.path(outDir, "testedModels"),
                                                 outDir = file.path(outDir, paste0("analysis_regime_", P(sim)$regimeArm)),
                                                 regime = P(sim)$regimeArm)
        return(invisible(sim))
      }
      if (isTRUE(P(sim)$featureSetArm)) {
        fsTag <- if (nzchar(P(sim)$featureSetTag)) paste0("_", P(sim)$featureSetTag) else ""
        planFs <- fread(file.path(outDir, paste0("experimentPlan_featureSets", fsTag, ".csv")))
        doneFs <- list.files(file.path(outDir, paste0("testedModels_featureSets", fsTag)), pattern = "_finalDT\\.csv$")
        missingFs <- setdiff(paste0(planFs$modelName, "_finalDT.csv"), doneFs)
        if (length(missingFs)) {
          writeLines(missingFs, file.path(outDir, paste0("modelsMissing_featureSets", fsTag, ".txt")))
          stop(length(missingFs), " feature-set models have no result (see modelsMissing_featureSets", fsTag, ".txt).")
        }
        sim$modelComparisons <- analyzeFeatureSets(modelDir = file.path(outDir, paste0("testedModels_featureSets", fsTag)),
                                                   mainDir = file.path(outDir, "testedModels"),
                                                   outDir = file.path(outDir, paste0("analysis_featureSets", fsTag)))
        return(invisible(sim))
      }
      plan <- fread(file.path(outDir, "experimentPlan.csv"))
      done <- list.files(file.path(outDir, "testedModels"), pattern = "_finalDT\\.csv$")
      missing <- setdiff(paste0(plan$modelName, "_finalDT.csv"), done)
      if (length(missing)) {
        writeLines(missing, file.path(outDir, "modelsMissing.txt"))
        stop(length(missing), " models have no result (see modelsMissing.txt). Run or resubmit stage 'train'.")
      }
      meta <- readRDS(file.path(outDir, "store", "store_meta.rds"))
      dp <- readRDS(file.path(outDir, "designParams.rds"))
      au <- auditSplitManifests(meta$index, plan, file.path(outDir, "splits"), spatial = dp$spatial)
      fwrite(au, file.path(outDir, "splitChecks_final.csv"))
      sim$modelComparisons <- analyzeExperiment(modelDir = file.path(outDir, "testedModels"),
                                                outDir = file.path(outDir, "analysis"))
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  stage <- P(sim)$stage
  if (!suppliedElsewhere("featurePriority", sim = sim)) {
    ft <- file.path(outputPath(sim), "featureTable.csv")
    if (!file.exists(ft))
      stop("featurePriority not supplied and ", ft, " not found. Run caribouNN_Global first.")
    sim$featurePriority <- fread(ft)
  }
  if (stage %in% c("all", "design") && !suppliedElsewhere("preparedData", sim = sim) &&
      !suppliedElsewhere("preparedDataFinal", sim = sim))
    stop("No defaults have been implemented yet... Please run caribouNN_Global.")
  return(invisible(sim))
}
