# Tests for the refit pipeline.
# Run:  Rscript test_refit.R [optional path to a prepared strata table (.rds/.csv)]
# Without an argument a synthetic dataset is used (no real data needed). Needs data.table + torch.
suppressMessages({library(data.table); library(torch)})
args <- commandArgs(trailingOnly = TRUE)
fileArg <- grep("--file=", commandArgs(FALSE), value = TRUE)
here <- if (length(fileArg)) dirname(normalizePath(sub("--file=", "", fileArg))) else getwd()
modDir <- normalizePath(file.path(here, ".."))
globalDir <- normalizePath(file.path(modDir, "..", "caribouNN_Global"), mustWork = FALSE)
for (f in c("stratumNet.R", "buildSplits.R", "verifySplits.R", "generateExperimentPlan.R", "strataStore.R",
            "trainingExperimentNN.R", "theExperiment.R", "analyzeExperiment.R", "featureSets.R", "spatialRegime.R")) source(file.path(modDir, "R", f))
nFail <- 0L
check <- function(cond, msg) {
  ok <- isTRUE(cond)
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg))
  if (!ok) nFail <<- nFail + 1L
}

# ---- 0. the shared helper is identical in both modules ------------------------------------
if (file.exists(file.path(globalDir, "R", "stratumNet.R")))
  check(unname(tools::md5sum(file.path(modDir, "R", "stratumNet.R"))) ==
          unname(tools::md5sum(file.path(globalDir, "R", "stratumNet.R"))),
        "stratumNet.R is identical in caribouNN and caribouNN_Global")

# ---- synthetic data: 6 years, 12 animals, strata of 11 rows --------------------------------
makeSynthetic <- function(seed = 1, perYear = 1500L, nAnimals = 12L, years = 2013:2018) {
  set.seed(seed)
  n <- perYear * length(years)
  strata <- data.table(year = rep(years, each = perYear))
  strata[, id := sample(sprintf("A%02d", seq_len(nAnimals)), .N, replace = TRUE,
                        prob = c(rep(3, 3), rep(1, nAnimals - 3)))]
  strata[id %in% c("A11", "A12") & year < 2017, id := "A01"]   # late animals -> strangers for PreVal
  strata[, burst_ := sample(1:40, .N, replace = TRUE)]
  strata[, indiv_step_id := paste0(id, "_", seq_len(.N))]
  strata[, `:=`(x1_ = runif(.N, 0, 1e6), y1_ = runif(.N, 0, 1e6))]
  d <- strata[rep(seq_len(.N), each = 11L)]
  d[, case_ := rep(c(TRUE, rep(FALSE, 10)), times = n)]
  d[, idIndex := as.numeric(factor(id, levels = sprintf("A%02d", seq_len(nAnimals))))]
  for (k in 1:6) set(d, j = paste0("f", k), value = rnorm(nrow(d)) + 0.3 * k * as.numeric(d$case_) * (k <= 3))
  d
}
features <- NULL
if (length(args) && file.exists(args[1])) {
  d <- if (grepl("rds$", args[1])) as.data.table(readRDS(args[1])) else fread(args[1])
} else {
  d <- makeSynthetic(); features <- paste0("f", 1:6)
}
setorder(d, indiv_step_id, -case_)
idx <- buildStrataIndex(d)
check(nrow(idx) == nrow(d) / 11, "buildStrataIndex: one row per stratum")

# ---- 1. splits are disjoint and pass the gate ------------------------------------------------
des <- generateExperimentPlan(idx, startYear = 2013L, endYear = 2018L, numberOfCovariatesList = c(2, Inf),
                              spatialTestYears = 2018L, spatialHorizons = 1L, blockKm = 300, bufferKm = 20,
                              nFolds = 3L, minTrain = 100L, minVal = 50L, minTest = 50L)
check(nrow(des$checks) > 0 && all(des$checks$ok), "verifySplits passes for every split of the design")
check(any(des$plan$arm == "spatial"), "design contains a spatial arm")
dup <- FALSE
for (sid in names(des$bundles)) for (rg in names(des$bundles[[sid]]$splits))
  if (anyDuplicated(des$bundles[[sid]]$splits[[rg]]$row)) dup <- TRUE
check(!dup, "no stratum is in two sets in any regime (direct check)")
check(all(vapply(des$bundles, function(b) identical(sort(b$splits$FutureUnseen[set == "test", row]),
                                                    sort(b$splits$Internal[set == "test", row])), logical(1))),
      "shared test set identical across regimes")
check(all(vapply(des$bundles, function(b) identical(sort(b$splits$FutureTainted[set != "test", row]),
                                                    sort(b$splits$FutureUnseen[set != "test", row])), logical(1))),
      "FutureTainted uses exactly the FutureUnseen strata (re-allocated)")
check(!anyDuplicated(des$plan$modelName) &&
        !anyDuplicated(unique(des$plan[, .(splitId, numberOfCovariates, modelSeed)])$modelSeed),
      "unique model names and seeds")

# ---- 2. the gate really fails when a leak is injected ----------------------------------------
sid <- names(des$bundles)[1]; b <- des$bundles[[sid]]; g <- des$plan[splitId == sid][1]
runGate <- function(bundle) tryCatch(verifySplits(idx, bundle, g$windowStart, g$historyEnd, g$testYear, sid),
                                     error = function(e) "failed")
cloneBundle <- function(x) { x$splits <- lapply(x$splits, data.table::copy); x }
leak <- cloneBundle(b)
leak$splits$FutureTainted <- rbind(leak$splits$FutureTainted,
                                   data.table(row = b$splits$FutureTainted[set == "train", row][1], set = "val"))
check(identical(runGate(leak), "failed"), "gate STOPS when a stratum is in train and validation")
leak2 <- cloneBundle(b)
nTr <- sum(leak2$splits$Internal$set == "train")
leak2$splits$Internal[set == "train", row := b$splits$Internal[set == "test", row][seq_len(nTr)]]
check(identical(runGate(leak2), "failed"), "gate STOPS when test strata are used for training (Internal)")
leak3 <- cloneBundle(b)
nVa <- sum(leak3$splits$FutureUnseen$set == "val")
leak3$splits$FutureUnseen[set == "val", row := b$splits$FutureUnseen[set == "train", row][seq_len(nVa)]]
check(identical(runGate(leak3), "failed"), "gate STOPS when PreVal validation is not the last history year")
# matchAnimals (default): Internal must use only animals of the FutureUnseen pool
check(all(vapply(des$bundles, function(b) isTRUE(b$matchAnimals), logical(1))), "matchAnimals is on by default")
sid2 <- "s2013_e2015_T2018_temporal"; b2 <- des$bundles[[sid2]]; g2 <- des$plan[splitId == sid2][1]
bU <- unique(idx$id[b2$splits$FutureUnseen[set != "test", row]])
check(all(idx$id[b2$splits$Internal[set != "test", row]] %in% bU), "Internal uses only animals of the FutureUnseen pool")
outside <- which(!(idx$id %in% bU) & idx$year >= g2$windowStart & idx$year <= g2$testYear)
check(length(outside) > 0, "test setup: animals outside the pool exist in this split")
leak4 <- cloneBundle(b2)
leak4$splits$Internal[which(leak4$splits$Internal$set == "train")[1], row := outside[1]]
check(identical(tryCatch(verifySplits(idx, leak4, g2$windowStart, g2$historyEnd, g2$testYear, sid2), error = function(e) "failed"), "failed"),
      "gate STOPS when Internal uses an animal outside the pool")
# biased draw: Internal strata taken from the first animals alphabetically (concentrated on few animals)
bi <- cloneBundle(b)
pool <- setdiff(which(idx$year >= g$windowStart & idx$year <= g$testYear), b$splits$Internal[set == "test", row])
n1 <- b$sizes[["nTrain"]]; n2 <- b$sizes[["nVal"]]
dr <- pool[order(idx$id[pool], idx$year[pool])][seq_len(n1 + n2)]
bi$splits$Internal <- rbind(data.table(row = dr[(n2 + 1):(n1 + n2)], set = "train"),
                            data.table(row = dr[seq_len(n2)], set = "val"), b$splits$Internal[set == "test"])
check(identical(runGate(bi), "failed"), "gate STOPS on a sampling-biased Internal draw")

# ---- 3. torch: indexing, stranger handling ------------------------------------------------------
m12 <- matrix(1:12, 4)
check(identical(as.array(torch_tensor(m12)$index_select(1, torch_tensor(c(3L, 1L), dtype = torch_long()))), m12[c(3, 1), ]),
      "torch index_select is 1-based like R")
net <- makeStratumNet()(nIn = 3, nAnimals = 5)
W <- net$idEmb$weight
before <- as.matrix(W$detach())
x <- torch_randn(6, 11, 3)
idt <- torch_tensor(c(1L, 2L, 5L, 5L, 3L, 1L), dtype = torch_long())
sc <- scoreStrata(net, x, idt, trainIdsSeen = c(1L, 2L, 3L), nAnimals = 5)
after <- as.matrix(W$detach())
check(isTRUE(all.equal(after[6, ], colMeans(before[c(1, 2, 3), ]), tolerance = 1e-6)),
      "unseen-animal embedding = mean of the trained animals' rows")
check(isTRUE(all.equal(after[1:5, ], before[1:5, ])), "trained embeddings are not modified by scoring")
check(identical(sc$seen, c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE)), "seen flags correct (animal 5 unseen)")
check(all(abs(sc$pTrue - exp(-sc$loss)) < 1e-12) && all(sc$rankTrue >= 1 & sc$rankTrue <= 11),
      "per-stratum loss / pTrue / rank are consistent")

# ---- 4. end-to-end on the synthetic data -----------------------------------------------------------
if (!is.null(features)) {
  store <- buildStrataStore(d, features)
  out <- file.path(Sys.getenv("REFIT_TEST_OUT", tempdir()), "refitTest"); dir.create(out, showWarnings = FALSE, recursive = TRUE)
  manDir <- file.path(out, "splits"); writeManifests(des$bundles, idx, manDir)
  tmp <- des$plan[arm == "temporal"]
  au <- auditSplitManifests(idx, tmp, manDir)
  check(all(au$ok), "auditSplitManifests re-verifies the manifests from disk")
  fp <- data.table(Feature = features, Importance = 6:1)
  sub <- des$plan[splitId == "s2013_e2015_T2016_temporal" & numberOfCovariates == 2]
  res <- theExperiment(store, sub, manDir, fp, batchSize = 64, epoch = 4, learningRate = 0.01,
                       outputDir = file.path(out, "models"))
  check(nrow(res) == 3 && all(is.finite(res$testLossMean)), "3 regimes trained and scored")
  check(all(file.exists(file.path(out, "models", paste0(sub$modelName, "_perStratum.rds")))), "per-stratum outputs written")
  ps <- readRDS(file.path(out, "models", paste0(sub$modelName[1], "_perStratum.rds")))
  check(all(c("indiv_step_id", "id", "burstKey", "year", "loss", "seenAnimal") %in% names(ps)),
        "per-stratum output has ids, burst, year, loss and the seen flag")
  res2 <- theExperiment(store, sub, manDir, fp, batchSize = 64, epoch = 4, learningRate = 0.01,
                        outputDir = file.path(out, "models2"))
  check(isTRUE(all.equal(res$testLossMean, res2$testLossMean, tolerance = 1e-6)),
        "re-running with the same seeds reproduces the test loss")
  check(!anyNA(res$trainLossBest) && !anyNA(res$valLossBest), "train / validation / test losses recorded")

  # slice runner: two tasks together cover the plan exactly once
  sub2 <- des$plan[arm == "temporal" & testYear >= 2017 & numberOfCovariates == 2]
  r1 <- theExperiment(store, sub2, manDir, fp, batchSize = 64, epoch = 2, learningRate = 0.01,
                      outputDir = file.path(out, "models3"), runSlice = c(1, 2))
  r2 <- theExperiment(store, sub2, manDir, fp, batchSize = 64, epoch = 2, learningRate = 0.01,
                      outputDir = file.path(out, "models3"), runSlice = c(2, 2))
  check(!any(r1$modelName %in% r2$modelName) && setequal(c(r1$modelName, r2$modelName), sub2$modelName),
        "runSlice splits the plan into disjoint tasks that cover every model")

  # analysis runs on the collected results (complexity 2 and all covariates, several test years)
  sub3 <- des$plan[arm == "temporal" & testYear >= 2016]
  theExperiment(store, sub3, manDir, fp, batchSize = 64, epoch = 2, learningRate = 0.01,
                outputDir = file.path(out, "models4"))
  an <- analyzeExperiment(file.path(out, "models4"), file.path(out, "analysis"))
  check(all(c("H1_reported_realized", "H1_forecast_vs_reference", "H1_optimism_difference", "H2_contrasts", "H2_sameInformation", "H3_penalty_vs_simplest",
              "selection_regret", "training_behaviour", "table1_design") %in% names(an)),
        "analyzeExperiment returns the pre-specified tables")
  check(all(file.exists(file.path(out, "analysis", c("allModels.csv", "H2_contrasts.csv", "pairedContrasts_perSplit.csv", "sameInformation_perSplit.csv")))),
        "analyzeExperiment writes its files")
  check(all(is.finite(an$H2_contrasts$estimate)) && all(an$H2_contrasts$pSignFlip >= 0 & an$H2_contrasts$pSignFlip <= 1, na.rm = TRUE),
        "paired contrasts and exact sign-flip p-values are valid")
  figs <- c("fig1_H1_cv_is_misleading.png", "fig1b_H1_per_year.png", "fig2_forest_contrasts.png", "fig3_complexity_curves.png", "fig4_horizon.png",
            "fig5_learning_curves.png", "fig6_how_training_ended.png", "fig7_same_information.png", "fig8_skill_top1.png")
  check(!requireNamespace("ggplot2", quietly = TRUE) || all(file.exists(file.path(out, "analysis", figs))),
        paste("all 8 figures written", paste(figs[!file.exists(file.path(out, "analysis", figs))], collapse = ", ")))
  check(!requireNamespace("ggplot2", quietly = TRUE) || file.exists(file.path(out, "analysis", "fig10_validation_vs_future_per_epoch.png")) ||
          !file.exists(file.path(out, "analysis", "fig6_how_training_ended.png")), "per-epoch validation-vs-future diagnostic figure written")
  # exact sign-flip: 3 years, all positive -> p = 2/8
  x <- c(1, 2, 3); sg <- as.matrix(expand.grid(rep(list(c(-1, 1)), 3))); pp <- mean(abs(sg %*% x / 3) >= abs(mean(x)) - 1e-12)
  check(abs(pp - 0.25) < 1e-12, "sign-flip arithmetic (3 years, all positive: p = 0.25)")
  # extension pass: a model that stopped at the cap is re-trained with the larger cap, old result kept
  m1 <- list.files(file.path(out, "models4"), pattern = "_finalDT[.]csv$")[1]
  nm <- sub("_finalDT.csv", "", m1); r0 <- fread(file.path(out, "models4", m1))
  planRow <- des$plan[modelName == nm]
  r1 <- trainingExperimentNN(store, readManifest(manDir, planRow$splitId, planRow$typeValidation), planRow, fp$Feature[seq_len(2)],
                             batchSize = 64, learningRate = 0.01, epochs = r0$epochsRun + 3, outputDir = file.path(out, "models4"),
                             extendFrom = r0$epochsRun)
  check(r1$epochsRun[1] > r0$epochsRun[1] || r1$epochsRun[1] == r1$epochCap[1], "extension pass re-trains a cap-bound model")
  check(file.exists(file.path(out, "models4", paste0(nm, "_finalDT_cap", r0$epochsRun, ".csv"))), "extension pass keeps the earlier result")
  # per-epoch test-loss diagnostic: logged for every epoch, and it does not change the fit
  h <- fread(file.path(out, "models4", paste0(nm, "_history.csv")))
  check("diagLoss" %in% names(h) && all(is.finite(h$diagLoss)), "history logs the per-epoch test loss (diagnostic)")
  torch::torch_manual_seed(1)
  xa <- torch::torch_randn(120, 11, 3); ia <- torch::torch_tensor(sample(1:6, 120, TRUE), dtype = torch::torch_long())
  xv <- torch::torch_randn(40, 11, 3);  iv <- torch::torch_tensor(sample(1:6, 40, TRUE), dtype = torch::torch_long())
  xd <- torch::torch_randn(40, 11, 3);  id <- torch::torch_tensor(sample(1:6, 40, TRUE), dtype = torch::torch_long())
  f0 <- fitStratumNet(xa, ia, xv, iv, nAnimals = 6L, lr = 0.01, epochs = 6, seed = 7, verbose = FALSE)
  f1 <- fitStratumNet(xa, ia, xv, iv, nAnimals = 6L, lr = 0.01, epochs = 6, seed = 7, verbose = FALSE, diag = list(x = xd, id = id))
  check(identical(f0$bestEpoch, f1$bestEpoch) && isTRUE(all.equal(f0$history$valLoss, f1$history$valLoss)) &&
          all(is.na(f0$history$diagLoss)) && all(is.finite(f1$history$diagLoss)),
        "the diagnostic never changes the fit (identical best epoch and validation path)")

  # ---- feature-set arm (re-ordered / ablated covariate sets) -------------------------------------------------------
  ft <- data.table(Feature = features, Importance = 6:1)
  fsA <- buildFeatureSets(ft, movementPattern = "^f[12]$")
  fsB <- buildFeatureSets(ft, movementPattern = "^f[12]$")
  check(identical(fsA, fsB), "feature sets are deterministic (seeded)")
  check(!any(fsA[set == "habitatOnly"]$Feature %in% c("f1", "f2")) && nrow(fsA[set == "habitatOnly"]) == 4,
        "habitatOnly removes the movement covariates (ablation)")
  check(identical(fsA[set == "habitatFirst"]$Feature[1:4], c("f3", "f4", "f5", "f6")) &&
          identical(fsA[set == "movementFirst"]$Feature[1:2], c("f1", "f2")), "habitatFirst / movementFirst order the families")
  check(all(sapply(c("randomA", "randomB"), function(s) setequal(fsA[set == s]$Feature, features))) &&
          !identical(fsA[set == "randomA"]$Feature, fsA[set == "randomB"]$Feature), "random orders are permutations and differ")
  ft2 <- data.table(Feature = c("a_endLog", "inter_logSl_x_a_startLog", "logSl", "b_end", "inter_logSl_x_b_start", "a_startLog", "b_start", "c_end"))
  fs2 <- buildFeatureSets(ft2, sets = c("interactionsOnly", "endOnly", "startOnly", "endPlusInteractions"))
  check(identical(fs2[set == "interactionsOnly"]$Feature, c("inter_logSl_x_a_startLog", "inter_logSl_x_b_start")) &&
          identical(fs2[set == "endOnly"]$Feature, c("a_endLog", "b_end")) &&
          identical(fs2[set == "startOnly"]$Feature, c("a_startLog", "b_start")) &&
          identical(fs2[set == "endPlusInteractions"]$Feature, c("inter_logSl_x_a_startLog", "a_endLog", "inter_logSl_x_b_start", "b_end")),
        "interaction experiment sets: interactions, matching end covariates, matching start covariates, and the pairs")
  armPlan <- makeFeatureSetPlan(des$plan[testYear >= 2016], fsA, levels = c(2, 5, 10, 20))
  check(all(armPlan$arm == "temporal") && setequal(unique(armPlan$typeValidation), c("FutureUnseen", "FutureTainted")) &&
          !anyDuplicated(armPlan$modelName) && all(armPlan$numberOfCovariates <= 6),
        "feature-set plan: temporal arm, two forecasting regimes, unique names, levels truncated to the set size")
  check(identical(makeFeatureSetPlan(des$plan[testYear >= 2016], fsA, levels = c(2, 5, 10, 20)), armPlan),
        "feature-set plan is reproducible (tasks rebuild it identically)")
  sel <- armPlan[featureSet %in% c("habitatOnly", "randomA") & splitId %in% unique(splitId)[1:2]]
  armRes <- theExperiment(store, sel, manDir, fp, batchSize = 64, epoch = 2, learningRate = 0.01,
                          outputDir = file.path(out, "armModels"), featureSets = fsA)
  check(nrow(armRes) == nrow(sel) && all(is.finite(armRes$testLossMean)), "feature-set models train and score")
  prv <- readRDS(file.path(out, "armModels", paste0(sel[featureSet == "habitatOnly" & numberOfCovariates == 4]$modelName[1], "_provenance.rds")))
  check(setequal(prv$features, c("f3", "f4", "f5", "f6")), "a habitatOnly model really uses only the habitat covariates")
  anFs <- analyzeFeatureSets(file.path(out, "armModels"), file.path(out, "models4"), file.path(out, "analysisFs"))
  check(all(c("levels", "penalty", "shape_per_regime") %in% names(anFs)) && file.exists(file.path(out, "analysisFs", "FS_penalty.csv")),
        "analyzeFeatureSets writes its tables")

  # ---- spatially blocked status-quo regime ("FutureTaintedSpatial") ---------------------------------------------------------
  spB <- assignSpatialBlocks(idx, blockKm = 250, bufferKm = 20, nFolds = 3L, seed = 11L)
  rgd <- makeSpatialRegimeDesign(idx, des$plan[testYear >= 2016], manDir, spB, minTrain = 100L, minTrainFraction = 0.3)
  check(nrow(rgd$checks) > 0 && all(rgd$checks$ok), "spatial regime: gate passes for every split")
  check(all(rgd$summary$nTrain <= rgd$summary$nTrainStatusQuo) && all(rgd$summary$nVal > 0),
        "spatial regime: training is the status-quo pool minus the validation blocks and their buffer")
  check(!anyDuplicated(rgd$plan$modelName) && all(rgd$plan$typeValidation == "FutureTaintedSpatial") &&
          all(grepl("_FutureTaintedSpatial$", rgd$plan$modelName)), "spatial regime plan: own regime and unique names")
  sidT <- rgd$summary$splitId[1]
  mT <- readManifest(manDir, sidT, "FutureTaintedSpatial"); unT <- readManifest(manDir, sidT, "FutureUnseen")
  check(setequal(mT$row[mT$set == "test"], unT$row[unT$set == "test"]), "spatial regime: same shared test set as PreVal")
  vb <- unique(spB$key[mT$row[mT$set == "val"]])
  check(!any(.blockZone(idx, spB, vb)[mT$row[mT$set == "train"]]), "spatial regime: no training stratum inside a validation block or its buffer")
  leak <- data.table::copy(mT)
  vrow <- mT$row[mT$set == "val"][1]; leak[row == mT$row[mT$set == "train"][1], row := vrow][, set := set]   # a duplicated stratum
  check(inherits(tryCatch(verifySpatialTainted(idx, leak, unT, spB, "leak"), error = function(e) e), "error"), "spatial regime: the gate stops an injected leak")
  rgSel <- rgd$plan[numberOfCovariates %in% c(2, Inf)]
  rgRes <- theExperiment(store, rgSel, manDir, fp, batchSize = 64, epoch = 2, learningRate = 0.01, outputDir = file.path(out, "models_rg"))
  check(nrow(rgRes) == nrow(rgSel) && all(rgRes$typeValidation == "FutureTaintedSpatial"), "spatial regime: models train with their own manifests")
  anRg <- analyzeRegimeArm(file.path(out, "models_rg"), file.path(out, "models4"), file.path(out, "analysis_rg"))
  check(all(c("levels", "contrasts", "optimism_pooled") %in% names(anRg)) && file.exists(file.path(out, "analysis_rg", "RA_contrasts.csv")),
        "analyzeRegimeArm writes its tables")
}
cat(sprintf("\n%d failure(s)\n", nFail))
if (nFail) quit(status = 1)
