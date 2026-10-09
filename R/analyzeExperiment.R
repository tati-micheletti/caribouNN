#' Pre-specified analysis of the experiment (see ANALYSIS_PLAN.md and LIMITATIONS.md).
#' Everything is computed from the per-model result files; nothing is tuned after seeing results.
#'
#' Quantities per model (cross-entropy loss, lower = better; chance = log(11)):
#'   reported = loss on the regime's own validation set at the best epoch (what that regime would report;
#'              NOTE: the same set drives early stopping, see LIMITATIONS.md)
#'   realized = loss on the shared test set (the future year's strata)
#'   optimism = realized - reported
#'   skill    = log(11) - realized ;  skillTop1 = top-1 accuracy - 1/11
#'
#' Inference: the independent unit is the TEST YEAR (splits that share a test year share their test strata).
#' Every estimate is therefore first averaged within test year; across the (about 8) years we report the mean, a
#' t interval, and an EXACT sign-flip permutation p-value (no bootstrap: a bootstrap over ~8 clusters is unreliable).
#' Primary H1 contrast = difference in optimism between regimes (not "optimism > 0": the reported loss is the
#' minimum over epochs on the set that stopped training, so every regime's optimism is biased upwards).
#' Primary H2 contrast = paired realized-loss difference PreVal - comparator. Primary H3 test = the DIFFERENCE between
#' regimes in the complexity slope (H3_slope_difference); each regime's own slope with the equivalence test is secondary.
#' @param sameInfo also compute contrasts restricted to test strata of animals PreVal saw in training (reads the
#'   per-stratum files; takes a few minutes)
analyzeExperiment <- function(modelDir, outDir, margin = 0.005, chance = log(11), sameInfo = TRUE) {
  stopifnot(requireNamespace("data.table", quietly = TRUE))
  dir.create(outDir, recursive = TRUE, showWarnings = FALSE)
  files <- list.files(modelDir, pattern = "_finalDT\\.csv$", full.names = TRUE)
  if (!length(files)) stop("No result files in ", modelDir)
  M <- data.table::rbindlist(lapply(files, data.table::fread), fill = TRUE)
  if (!"epochCap" %in% names(M)) M[, epochCap := 50L]            # results from before the cap was recorded
  M[is.na(epochCap), epochCap := 50L]                             # mix of old results (no cap recorded) and newer ones
  M[, `:=`(reported = valLossBest, realized = testLossMean)]
  M[, `:=`(optimism = realized - reported, skill = chance - realized, skillTop1 = testAccuracy - 1 / 11,
           trainTestGap = realized - trainLossBest)]
  M[, `:=`(complexity = ifelse(is.infinite(numberOfCovariates), 30, numberOfCovariates),
           windowLength = historyEnd - windowStart + 1L, capBound = epochsRun >= epochCap)]
  data.table::fwrite(M, file.path(outDir, "allModels.csv"))
  regimes <- c("FutureUnseen", "FutureTainted", "Internal")
  out <- list()

  # ---- helpers ---------------------------------------------------------------------------------
  yearTest <- function(x, equivMargin = NULL) {
    x <- x[is.finite(x)]; n <- length(x)
    if (n == 0) return(data.table::data.table(estimate = NA_real_, lo = NA_real_, hi = NA_real_, pSignFlip = NA_real_,
                                              nYears = 0L, nPositive = 0L, equivalent = NA))
    est <- mean(x); se <- if (n > 1) stats::sd(x) / sqrt(n) else NA_real_
    tc <- if (n > 1) stats::qt(0.975, n - 1) else NA_real_
    p <- NA_real_
    if (n >= 2 && n <= 16) {                                       # exact sign-flip test of "mean difference = 0"
      sg <- as.matrix(expand.grid(rep(list(c(-1, 1)), n)))
      p <- mean(abs(sg %*% x / n) >= abs(est) - 1e-12)
    }
    eq <- NA
    if (!is.null(equivMargin) && n > 1) { t90 <- stats::qt(0.95, n - 1); eq <- (est - t90 * se > -equivMargin) && (est + t90 * se < equivMargin) }
    data.table::data.table(estimate = est, lo = est - tc * se, hi = est + tc * se, pSignFlip = p, nYears = n,
                           nPositive = sum(x > 0), equivalent = eq)
  }
  byYear <- function(dt, col, by, equivMargin = NULL) {
    y <- dt[, .(v = mean(get(col), na.rm = TRUE)), by = c(by, "testYear")]
    y[, yearTest(v, equivMargin), by = by]
  }
  perYear <- function(dt, col, by) dt[, .(value = mean(get(col), na.rm = TRUE)), by = c(by, "testYear")]

  # ---- wide table: one row per (arm, split, complexity, replicate), regimes side by side ---------------
  id <- c("arm", "splitId", "complexity", "replicate", "testYear", "horizon", "windowLength")
  W <- data.table::dcast(M, stats::as.formula(paste(paste(id, collapse = " + "), "~ typeValidation")),
                         value.var = c("realized", "reported", "optimism", "skillTop1", "bestEpoch"))
  W[, H1_forecast_minus_reference := realized_FutureTainted - realized_Internal]   # H1: forecasting with CV vs the reference CV
  W[, `:=`(PreVal_minus_Tainted = realized_FutureUnseen - realized_FutureTainted,
           PreVal_minus_Internal = realized_FutureUnseen - realized_Internal,
           optGapTainted = optimism_FutureTainted - optimism_FutureUnseen,
           optGapInternal = optimism_Internal - optimism_FutureUnseen)]
  data.table::fwrite(W, file.path(outDir, "pairedContrasts_perSplit.csv"))

  # ---- Table 1: design (strata / animals / bursts per set) ---------------------------------------------
  out$table1_design <- M[, .(nTrain = mean(nTrain), nVal = mean(nVal), nTest = mean(nTest),
                             animalsTrain = mean(nAnimalsTrain), burstsTrain = mean(nBurstsTrain),
                             animalsTest = mean(nAnimalsTest), models = .N), by = .(arm, typeValidation)]

  # ---- H1 (does NOT involve PreVal): is CV a misleading measure of forecast quality? -------------------------------
  # Primary: on the identical test strata, is the loss when CV-trained models FORECAST (FutureTainted) larger than the
  # reference CV (Internal)? Positive = forecasting is harder than the CV reference suggests.
  # Secondary: the CV regime's own reported error against its realized forecast error (Tainted; carries the selection bias
  # described in LIMITATIONS.md).
  out$H1_forecast_vs_reference <- byYear(W, "H1_forecast_minus_reference", c("arm", "complexity"))
  out$H1_forecast_vs_reference_pooled <- byYear(W, "H1_forecast_minus_reference", "arm")
  out$H1_forecast_vs_reference_perYear <- perYear(W, "H1_forecast_minus_reference", c("arm", "complexity"))
  out$H1_forecast_vs_reference_by_horizon <- byYear(W[, horizonBin := cut(horizon, c(0, 1, 2, 4, Inf), labels = c("1", "2", "3-4", "5+"))],
                                                    "H1_forecast_minus_reference", c("arm", "horizonBin"))
  out$H1_own_optimism_tainted <- byYear(M[typeValidation == "FutureTainted"], "optimism", c("arm", "complexity"))
  out$H1_own_optimism_tainted_pooled <- byYear(M[typeValidation == "FutureTainted"], "optimism", "arm")
  # ---- descriptive: reported vs realized for all regimes and how much more optimistic CV is than PreVal ---------------
  out$H1_reported_realized <- data.table::rbindlist(lapply(c("reported", "realized", "optimism"), function(v)
    byYear(M, v, c("arm", "typeValidation", "complexity"))[, measure := v]))
  out$H1_optimism_difference <- data.table::rbindlist(lapply(c("optGapTainted", "optGapInternal"), function(v)
    byYear(W, v, c("arm", "complexity"))[, contrast := v]))
  out$H1_optimism_difference_pooled <- data.table::rbindlist(lapply(c("optGapTainted", "optGapInternal"), function(v)
    byYear(W, v, "arm")[, contrast := v]))

  # ---- H2a: PreVal vs the two comparators on identical test strata ----------------------------------------------
  Wl <- data.table::melt(W, id.vars = id, measure.vars = c("PreVal_minus_Tainted", "PreVal_minus_Internal"),
                         variable.name = "contrast", value.name = "diff")
  out$H2_contrasts <- byYear(Wl, "diff", c("arm", "contrast", "complexity"))
  out$H2_contrasts_pooled <- byYear(Wl, "diff", c("arm", "contrast"))
  out$H2_perYear <- perYear(Wl, "diff", c("arm", "contrast", "complexity"))
  data.table::fwrite(out$H2_perYear, file.path(outDir, "H2_perYear_for_forestplot.csv"))

  # ---- Same-information contrast (PreVal never sees animals that appear only in its validation year) -----------
  if (sameInfo) {
    si <- sameInformationContrasts(M, modelDir)
    if (!is.null(si) && nrow(si)) {
      data.table::fwrite(si, file.path(outDir, "sameInformation_perSplit.csv"))
      sl <- data.table::melt(si, id.vars = c("arm", "splitId", "complexity", "replicate", "testYear"),
                             measure.vars = c("dTainted_seen", "dInternal_seen", "dTainted_unseen", "dInternal_unseen"),
                             variable.name = "contrast", value.name = "diff")
      out$H2_sameInformation <- byYear(sl, "diff", c("arm", "contrast", "complexity"))
      out$H2_sameInformation_pooled <- byYear(sl, "diff", c("arm", "contrast"))
      out$H2_sameInformation_perYear <- perYear(sl, "diff", c("arm", "contrast", "complexity"))
      out$sameInformation_share <- si[, .(shareSeenByPreVal = mean(nSeen / (nSeen + nUnseen))), by = arm]
    }
  }

  # ---- H3: complexity ------------------------------------------------------------------------------------------
  M2 <- data.table::copy(M)
  M2[, realizedVsSimplest := realized - realized[which.min(complexity)], by = .(arm, typeValidation, splitId, replicate)]
  out$H3_penalty_vs_simplest <- byYear(M2, "realizedVsSimplest", c("arm", "typeValidation", "complexity"))
  slopes <- M[, .(slope = if (.N >= 3) unname(stats::coef(stats::lm(realized ~ log2(complexity)))[2]) else NA_real_,
                  slopeSkillTop1 = if (.N >= 3) unname(stats::coef(stats::lm(skillTop1 ~ log2(complexity)))[2]) else NA_real_,
                  testYear = testYear[1], horizon = horizon[1], windowLength = windowLength[1]),
              by = .(arm, typeValidation, splitId, replicate)]
  slopes <- slopes[is.finite(slope)]
  out$H3_slope_realized <- byYear(slopes, "slope", c("arm", "typeValidation"), equivMargin = margin)
  out$H3_slope_skillTop1 <- byYear(slopes, "slopeSkillTop1", c("arm", "typeValidation"))
  sw <- data.table::dcast(slopes, arm + splitId + replicate + testYear ~ typeValidation, value.var = "slope")
  if (all(regimes %in% names(sw))) {
    sw[, `:=`(PreVal_minus_Tainted = FutureUnseen - FutureTainted, PreVal_minus_Internal = FutureUnseen - Internal)]
    out$H3_slope_difference <- data.table::rbindlist(lapply(c("PreVal_minus_Tainted", "PreVal_minus_Internal"), function(v)
      byYear(sw, v, "arm")[, contrast := v]))
  }
  # decision relevance: choose the complexity with the best REPORTED loss within each regime
  sel <- M[, {k <- which.min(reported); .(chosenComplexity = complexity[k], realizedChosen = realized[k], oracle = min(realized),
                                          testYear = testYear[1], horizon = horizon[1])},
           by = .(arm, typeValidation, splitId, replicate)]
  sel[, regret := realizedChosen - oracle]
  out$selection_regret <- byYear(sel, "regret", c("arm", "typeValidation"))
  sw2 <- data.table::dcast(sel, arm + splitId + replicate + testYear ~ typeValidation, value.var = "realizedChosen")
  if (all(regimes %in% names(sw2))) {
    sw2[, `:=`(PreVal_minus_Tainted = FutureUnseen - FutureTainted, PreVal_minus_Internal = FutureUnseen - Internal)]
    out$selection_realized_difference <- data.table::rbindlist(lapply(c("PreVal_minus_Tainted", "PreVal_minus_Internal"), function(v)
      byYear(sw2, v, "arm")[, contrast := v]))
  }

  # ---- Horizon and training-window length (H1 predicts CV optimism grows with the horizon) ----------------------------
  Wl[, horizonBin := cut(horizon, c(0, 1, 2, 4, Inf), labels = c("1", "2", "3-4", "5+"))]
  Wl[, windowBin := cut(windowLength, c(0, 2, 4, 6, Inf), labels = c("2", "3-4", "5-6", "7+"))]
  out$H2_by_horizon <- byYear(Wl, "diff", c("arm", "contrast", "horizonBin"))
  out$H2_by_window <- byYear(Wl, "diff", c("arm", "contrast", "windowBin"))
  W[, horizonBin := cut(horizon, c(0, 1, 2, 4, Inf), labels = c("1", "2", "3-4", "5+"))]
  out$H1_optimism_difference_by_horizon <- data.table::rbindlist(lapply(c("optGapTainted", "optGapInternal"), function(v)
    byYear(W, v, c("arm", "horizonBin"))[, contrast := v]))

  # ---- Training behaviour (cap, epochs) ---------------------------------------------------------------------------------
  out$training_behaviour <- M[, .(share_stopped_at_cap = mean(capBound), median_best_epoch = stats::median(bestEpoch),
                                  median_epochs_run = stats::median(epochsRun), models = .N), by = .(arm, typeValidation)]

  # ---- Absolute usefulness ----------------------------------------------------------------------------------------------------
  out$skill <- byYear(M, "skill", c("arm", "typeValidation", "complexity"))
  out$skill_top1 <- byYear(M, "skillTop1", c("arm", "typeValidation", "complexity"))
  # Top-1 hit rate above chance (1/11) per regime: mean across test years, t interval and exact sign-flip test against 0
  # (the independent unit is the test year, as everywhere else). Paired difference PreVal - status quo on identical test strata.
  if (all(c("FutureUnseen", "FutureTainted") %in% M$typeValidation)) {
    tw <- data.table::dcast(M, arm + splitId + complexity + replicate + testYear ~ typeValidation, value.var = "skillTop1")
    tw[, top1_PreVal_minus_Tainted := FutureUnseen - FutureTainted]
    out$H2_skill_top1_contrast <- byYear(tw, "top1_PreVal_minus_Tainted", c("arm", "complexity"))
    out$H2_skill_top1_contrast_pooled <- byYear(tw, "top1_PreVal_minus_Tainted", "arm")
  }

  # ---- Mixed model on the split-level contrasts (needs lme4; run it locally if EVE lacks it) ---------------------------
  if (requireNamespace("lme4", quietly = TRUE) && uniqueN(Wl$testYear) >= 3) {
    txt <- character()
    for (cn in c("PreVal_minus_Tainted", "PreVal_minus_Internal")) {
      d <- Wl[contrast == cn & arm == "temporal"]
      if (nrow(d) < 30) next
      fit <- tryCatch(lme4::lmer(diff ~ log2(complexity) + horizon + windowLength + (1 | testYear) + (1 | splitId), data = d),
                      error = function(e) NULL)
      if (!is.null(fit)) txt <- c(txt, paste0("== ", cn, " (temporal arm): diff ~ log2(complexity) + horizon + windowLength + (1|testYear) + (1|splitId)"),
                                  utils::capture.output(print(summary(fit)$coefficients)), "")
    }
    if (length(txt)) writeLines(txt, file.path(outDir, "mixed_models.txt"))
  }
  # ---- H3 v2: SHAPE of the complexity curve (amendment after the first look at the data; see ANALYSIS_PLAN.md) ------------
  # The straight-line slope on log2(covariates) cannot see a curve that rises and then falls. Two numbers per split,
  # paired within the split, are used instead: END = loss(most complex) - loss(simplest); PEAK = mean loss of the
  # intermediate levels - loss(simplest) (the worst part of the ladder). Compared between regimes within the split.
  lv <- sort(unique(M$complexity)); kmin <- as.character(min(lv)); kmax <- as.character(max(lv))
  kmid <- as.character(setdiff(lv, c(min(lv), max(lv))))
  if (length(lv) >= 3) {
    wd <- data.table::dcast(M, arm + typeValidation + splitId + replicate + testYear + horizon ~ complexity, value.var = "realized")
    wd[, end := get(kmax) - get(kmin)]
    wd[, peak := rowMeans(.SD) - get(kmin), .SDcols = kmid]
    wd[, horizonBin := cut(horizon, c(0, 1, 2, 4, Inf), labels = c("1", "2", "3-4", "5+"))]
    out$H3_shape_per_regime <- data.table::rbindlist(lapply(c("end", "peak"), function(v)
      byYear(wd, v, c("arm", "typeValidation"))[, measure := v]))
    out$H3_shape_per_regime_splitSummary <- wd[, .(endMedian = stats::median(end), endMean = mean(end), endShareAbove0 = mean(end > 0),
                                                   peakMedian = stats::median(peak), peakMean = mean(peak), peakShareAbove0 = mean(peak > 0),
                                                   nSplits = .N), by = .(arm, typeValidation)]
    out$H3_shape_by_horizon <- wd[, .(endMedian = stats::median(end), peakMedian = stats::median(peak), nSplits = .N),
                                  by = .(arm, typeValidation, horizonBin)][order(arm, typeValidation, horizonBin)]
    sh <- data.table::dcast(wd, arm + splitId + replicate + testYear + horizonBin ~ typeValidation, value.var = c("end", "peak"))
    if (all(c("end_FutureUnseen", "end_FutureTainted", "end_Internal") %in% names(sh))) {
      sh[, `:=`(end_PreVal_minus_Tainted = end_FutureUnseen - end_FutureTainted, end_PreVal_minus_Internal = end_FutureUnseen - end_Internal,
                peak_PreVal_minus_Tainted = peak_FutureUnseen - peak_FutureTainted, peak_PreVal_minus_Internal = peak_FutureUnseen - peak_Internal)]
      out$H3_shape_difference <- data.table::rbindlist(lapply(c("end_PreVal_minus_Tainted", "end_PreVal_minus_Internal",
                                                                "peak_PreVal_minus_Tainted", "peak_PreVal_minus_Internal"), function(v)
        byYear(sh, v, "arm")[, contrast := v]))
      out$H3_shape_difference_by_horizon <- data.table::rbindlist(lapply(c("end_PreVal_minus_Tainted", "peak_PreVal_minus_Tainted"), function(v)
        sh[, .(median = stats::median(get(v)), mean = mean(get(v)), shareNegative = mean(get(v) < 0), nSplits = .N), by = .(arm, horizonBin)][, contrast := v]))
    }
    # paired penalty per split relative to the simplest model (for the figure)
    pl <- data.table::melt(wd, id.vars = c("arm", "typeValidation", "splitId", "replicate", "testYear", "horizon", "horizonBin"),
                           measure.vars = as.character(lv), variable.name = "complexity", value.name = "loss")
    pl[, complexity := as.numeric(as.character(complexity))]
    pl[, penalty := loss - loss[which.min(complexity)], by = .(arm, typeValidation, splitId, replicate)]
    out$H3_penalty_perSplit <- pl
    # mixed models on all splits (partial pooling across test years): regime x complexity interaction = difference in penalty
    if (requireNamespace("lme4", quietly = TRUE)) {
      mixedRows <- list()
      for (cmp in c("FutureTainted", "Internal")) {
        d <- M[arm == "temporal" & typeValidation %in% c("FutureUnseen", cmp)]
        if (uniqueN(d$testYear) < 3 || uniqueN(d$splitId) < 10) next
        d[, `:=`(regime = factor(typeValidation, c("FutureUnseen", cmp), c("PreVal", "Comparator")), k = factor(complexity))]
        fit <- tryCatch(lme4::lmer(realized ~ regime * k + (1 | testYear) + (1 | splitId), data = d), error = function(e) NULL)
        if (is.null(fit)) next
        cf <- summary(fit)$coefficients; x <- cf[grepl("regimeComparator:k", rownames(cf)), , drop = FALSE]
        mixedRows[[cmp]] <- data.table::data.table(comparator = cmp, term = rownames(x), estimate = x[, 1], lo = x[, 1] - 1.96 * x[, 2],
                                                   hi = x[, 1] + 1.96 * x[, 2], z = x[, 3])
        # endpoint penalty by horizon
        d2 <- d[complexity %in% c(min(lv), max(lv))][, `:=`(k2 = factor(complexity), hz = horizon - 1)]
        fit2 <- tryCatch(lme4::lmer(realized ~ regime * k2 * hz + (1 | testYear) + (1 | splitId), data = d2), error = function(e) NULL)
        if (!is.null(fit2)) { cf2 <- summary(fit2)$coefficients
          mixedRows[[paste0(cmp, "_horizon")]] <- data.table::data.table(comparator = paste0(cmp, " (endpoint x horizon)"), term = rownames(cf2),
                                                                         estimate = cf2[, 1], lo = cf2[, 1] - 1.96 * cf2[, 2], hi = cf2[, 1] + 1.96 * cf2[, 2], z = cf2[, 3]) }
      }
      if (length(mixedRows)) out$H3_mixed_interactions <- data.table::rbindlist(mixedRows)
    }
  }
  for (nm in names(out)) data.table::fwrite(out[[nm]], file.path(outDir, paste0(nm, ".csv")))
  if (requireNamespace("ggplot2", quietly = TRUE)) tryCatch(analysisFigures(M, W, Wl, out, modelDir, outDir, chance),
                                                            error = function(e) message("Figures failed: ", conditionMessage(e)))
  else message("ggplot2 is not installed: tables written, figures skipped (rerun analyzeExperiment locally on the downloaded folder).")
  invisible(out)
}

#' Contrasts restricted to the test strata whose animal PreVal saw during training ("same information").
#' PreVal cannot learn animals that first appear in its validation year; Tainted and Internal can. On the strata of
#' animals PreVal knows, all three regimes have the same animal information, so a difference there is not an
#' information-deficit artefact. Strata of animals unknown to PreVal are reported separately.
sameInformationContrasts <- function(M, modelDir) {
  keyCols <- c("arm", "splitId", "numberOfCovariates", "replicate")
  groups <- split(M, by = keyCols, drop = TRUE)
  rows <- lapply(groups, function(g) {
    if (!all(c("FutureUnseen", "FutureTainted", "Internal") %in% g$typeValidation)) return(NULL)
    rd <- function(rg) {
      f <- file.path(modelDir, paste0(g$modelName[g$typeValidation == rg], "_perStratum.rds"))
      if (!file.exists(f)) return(NULL)
      x <- readRDS(f); x <- x[set == "test", .(indiv_step_id, loss, seenAnimal)]
      data.table::setkey(x, indiv_step_id); x
    }
    u <- rd("FutureUnseen"); t <- rd("FutureTainted"); i <- rd("Internal")
    if (is.null(u) || is.null(t) || is.null(i)) return(NULL)
    d <- u[t, on = "indiv_step_id", nomatch = NULL][i, on = "indiv_step_id", nomatch = NULL]
    # columns: loss (PreVal), seenAnimal (PreVal), i.loss (Tainted), i.seenAnimal, i.loss.1 (Internal) -> rename by position
    data.table::setnames(d, c("indiv_step_id", "lossU", "seenU", "lossT", "seenT", "lossI", "seenI"))
    s <- d$seenU
    data.table::data.table(arm = g$arm[1], splitId = g$splitId[1], complexity = g$complexity[1], replicate = g$replicate[1],
                           testYear = g$testYear[1], horizon = g$horizon[1], windowLength = g$windowLength[1],
                           nSeen = sum(s), nUnseen = sum(!s),
                           dTainted_seen = if (any(s)) mean(d$lossU[s] - d$lossT[s]) else NA_real_,
                           dInternal_seen = if (any(s)) mean(d$lossU[s] - d$lossI[s]) else NA_real_,
                           dTainted_unseen = if (any(!s)) mean(d$lossU[!s] - d$lossT[!s]) else NA_real_,
                           dInternal_unseen = if (any(!s)) mean(d$lossU[!s] - d$lossI[!s]) else NA_real_)
  })
  data.table::rbindlist(rows)
}

#' Figures (ggplot2). One panel = one message; every figure shows the spread across test years, not only a mean.
analysisFigures <- function(M, W, Wl, out, modelDir, outDir, chance) {
  library(ggplot2)
  cols <- c(FutureUnseen = "#1F6FB5", FutureTainted = "#E08A00", Internal = "#B23A48")
  labs <- c(FutureUnseen = "PreVal (FutureUnseen)", FutureTainted = "Status quo (FutureTainted)", Internal = "Random CV (Internal)")
  thm <- theme_bw(base_size = 11) + theme(legend.position = "bottom", panel.grid.minor = element_blank())
  save <- function(p, f, w = 8, h = 5) ggsave(file.path(outDir, f), p, width = w, height = h, dpi = 200)
  Mt <- M[arm == "temporal"]
  Mt[, regime := factor(typeValidation, names(cols), labs[names(cols)])]
  colv <- stats::setNames(cols, labs[names(cols)])

  # 1. H1 (no PreVal): CV as a measure of forecast quality
  Wt <- W[arm == "temporal"]
  d1 <- data.table::rbindlist(list(
    Wt[, .(panel = "A. Forecasting with CV (y) vs the reference CV (x), same test strata", x = realized_Internal, y = realized_FutureTainted, complexity)],
    Wt[, .(panel = "B. CV's own reported error (x) vs its forecast error (y)", x = reported_FutureTainted, y = realized_FutureTainted, complexity)]))
  save(ggplot(d1, aes(x, y, colour = factor(complexity))) + geom_abline(slope = 1, intercept = 0, linetype = 2) +
         geom_point(alpha = 0.6, size = 1.2) + facet_wrap(~ panel, scales = "free") + thm +
         labs(x = "Loss (lower is better)", y = "Loss when forecasting", colour = "Covariates",
              title = "Is cross-validation a misleading measure of forecast quality?",
              subtitle = "Dashed line: no difference. Dots above the line: forecasting is worse than what CV suggests. One dot = one split and complexity"),
       "fig1_H1_cv_is_misleading.png", 11, 5)
  h1y <- out$H1_forecast_vs_reference_perYear[arm == "temporal"]; h1m <- out$H1_forecast_vs_reference[arm == "temporal"]
  save(ggplot(h1y, aes(value, factor(testYear))) + geom_vline(xintercept = 0, linetype = 2) + geom_point(alpha = 0.7) +
         geom_errorbarh(data = h1m, aes(xmin = lo, xmax = hi, y = 0.4), inherit.aes = FALSE, height = 0.25, colour = "#B23A48") +
         geom_point(data = h1m, aes(estimate, 0.4), inherit.aes = FALSE, shape = 18, size = 3, colour = "#B23A48") +
         facet_wrap(~ complexity, labeller = labeller(complexity = function(x) paste(x, "covariates"))) + thm +
         labs(x = "Forecast loss minus reference-CV loss (positive = CV is misleading)", y = "Test year",
              title = "H1 per forecast year", subtitle = "Dots: test years. Red diamond and bar: mean and t interval across years"),
       "fig1b_H1_per_year.png", 10, 6)

  # 2. Forest plot: per-test-year contrasts (PreVal minus comparator; negative = PreVal better)
  # Random CV (Internal) is a leaky reference, not a forecast: PreVal is only ever compared with another forecasting regime.
  fy <- out$H2_perYear[arm == "temporal" & contrast == "PreVal_minus_Tainted"]; fm <- out$H2_contrasts[arm == "temporal" & contrast == "PreVal_minus_Tainted"]
  fy[, cLabel := "PreVal - Status quo"]; fm[, cLabel := "PreVal - Status quo"]
  save(ggplot(fy, aes(value, factor(testYear))) + geom_vline(xintercept = 0, linetype = 2) + geom_point(alpha = 0.7) +
         geom_errorbarh(data = fm, aes(xmin = lo, xmax = hi, y = 0.4), inherit.aes = FALSE, height = 0.25, colour = "#1F6FB5") +
         geom_point(data = fm, aes(estimate, 0.4), inherit.aes = FALSE, shape = 18, size = 3, colour = "#1F6FB5") +
         facet_wrap(~ complexity, nrow = 1, labeller = labeller(complexity = function(x) paste(x, "covariates"))) + thm +
         labs(x = "Difference in realized test loss (negative = PreVal better)", y = "Test year",
              title = "PreVal vs the status quo (both forecast the same test year), one dot per test year",
              subtitle = "Blue diamond and bar: mean and t interval across test years (paired on identical test strata)"),
       "fig2_forest_contrasts.png", 11, 3.8)

  # 3. Complexity curves relative to the simplest model, one thin line per test year
  cp <- Mt[, .(v = mean(realized)), by = .(regime, complexity, testYear)]
  cp[, v := v - v[which.min(complexity)], by = .(regime, testYear)]
  cm <- cp[, .(v = mean(v)), by = .(regime, complexity)]
  save(ggplot(cp, aes(factor(complexity), v)) + geom_hline(yintercept = 0, linetype = 2) +
         geom_line(aes(group = testYear), alpha = 0.35, colour = "grey40") +
         geom_line(data = cm, aes(group = regime, colour = regime), linewidth = 1.2) +
         geom_point(data = cm, aes(colour = regime), size = 2) + facet_wrap(~ regime) +
         scale_colour_manual(values = colv, guide = "none") + thm +
         labs(x = "Number of covariates", y = "Realized loss minus the simplest model's (per test year)",
              title = "Does adding covariates hurt out-of-sample?",
              subtitle = "Grey: one line per test year. Colour: mean. Above 0 = worse than the simplest model"),
       "fig3_complexity_curves.png", 10, 4.5)

  # 4. Contrast against forecast horizon
  Wh <- Wl[arm == "temporal" & contrast == "PreVal_minus_Tainted"]; Wh[, cLabel := "PreVal - Status quo"]
  save(ggplot(Wh, aes(horizon, diff)) + geom_hline(yintercept = 0, linetype = 2) + geom_jitter(width = 0.1, alpha = 0.25, size = 0.8) +
         geom_smooth(method = "lm", formula = y ~ x, colour = "#1F6FB5", se = TRUE) + facet_wrap(~ cLabel) + thm +
         scale_x_continuous(breaks = 1:10) +
         labs(x = "Forecast horizon (test year minus last history year)", y = "Difference in realized test loss",
              title = "Does the PreVal advantage depend on how far ahead we forecast?"),
       "fig4_horizon.png", 6, 4.5)

  # 5. Learning curves of example models (largest split, most covariates) + how training ended
  ex <- Mt[arm == "temporal"][which.max(nTrain)]
  exm <- Mt[splitId == ex$splitId & numberOfCovariates == max(numberOfCovariates) & replicate == min(replicate)]   # one replicate: overlaid replicates make a saw-tooth
  hist <- data.table::rbindlist(lapply(seq_len(nrow(exm)), function(i) {
    f <- file.path(modelDir, paste0(exm$modelName[i], "_history.csv")); if (!file.exists(f)) return(NULL)
    h <- data.table::fread(f); h[, `:=`(regime = exm$regime[i], bestEpoch = exm$bestEpoch[i])]
  }))
  if (nrow(hist)) save(ggplot(hist, aes(epoch)) + geom_line(aes(y = trainLoss, linetype = "training")) +
                         geom_line(aes(y = valLoss, linetype = "validation")) +
                         geom_vline(aes(xintercept = bestEpoch), colour = "red", alpha = 0.6) + facet_wrap(~ regime) + thm +
                         labs(x = "Epoch", y = "Loss", linetype = NULL, title = paste("Learning curves,", ex$splitId),
                              subtitle = "Red line: epoch kept (best validation loss)"),
                       "fig5_learning_curves.png", 11, 4)
  tb <- Mt[, .(`stopped at cap` = mean(capBound), `stopped by patience` = 1 - mean(capBound)), by = regime]
  tb <- data.table::melt(tb, id.vars = "regime")
  save(ggplot(tb, aes(regime, value, fill = variable)) + geom_col() + thm + scale_fill_manual(values = c("grey60", "grey85")) +
         labs(x = NULL, y = "Share of models", fill = NULL, title = "How did training end?",
              subtitle = "Dark: training hit the epoch cap while validation loss was still improving") + theme(axis.text.x = element_text(angle = 15, hjust = 1)),
       "fig6_how_training_ended.png", 7, 4)

  # 6b. Diagnostic of WHY the status quo overfits: validation loss vs the future-year (test) loss after every epoch.
  # Only models trained after the diagnostic was added have `diagLoss`; it is never used for stopping or selection.
  hd <- data.table::rbindlist(lapply(seq_len(nrow(Mt)), function(i) {
    f <- file.path(modelDir, paste0(Mt$modelName[i], "_history.csv")); if (!file.exists(f)) return(NULL)
    h <- data.table::fread(f); if (!"diagLoss" %in% names(h) || all(is.na(h$diagLoss))) return(NULL)
    h[, .(epoch, valLoss, diagLoss, regime = Mt$regime[i], complexity = Mt$complexity[i], bestEpoch = Mt$bestEpoch[i], modelName = Mt$modelName[i])]
  }))
  if (nrow(hd)) {
    hd[, `:=`(val = valLoss - valLoss[1], test = diagLoss - diagLoss[1]), by = modelName]
    hs <- hd[, .(val = mean(val), test = mean(test), nModels = .N, shareStillRunning = .N / uniqueN(modelName)), by = .(regime, complexity, epoch)]
    hs <- hs[nModels >= 10]
    data.table::fwrite(hs, file.path(outDir, "epoch_diagnostic_val_vs_test.csv"))
    hl <- data.table::melt(hs, id.vars = c("regime", "complexity", "epoch"), measure.vars = c("val", "test"), variable.name = "set", value.name = "change")
    hl[, set := factor(set, c("val", "test"), c("Validation (what early stopping sees)", "Future year (what we care about)"))]
    save(ggplot(hl, aes(epoch, change, colour = set)) + geom_hline(yintercept = 0, linetype = 2) + geom_line(linewidth = 1) +
           facet_grid(complexity ~ regime, labeller = labeller(complexity = function(x) paste(x, "cov."))) + thm +
           scale_colour_manual(values = c("grey30", "#B23A48")) +
           labs(x = "Epoch", y = "Loss relative to epoch 1 (mean over models)", colour = NULL,
                title = "Does the validation loss keep improving while the future-year loss gets worse?",
                subtitle = "If the red line rises while the grey line falls, validation cannot see what hurts the forecast"),
         "fig10_validation_vs_future_per_epoch.png", 11, 8)
  }

  # 7. Same-information contrasts
  if (!is.null(out$H2_sameInformation_perYear)) {
    sy <- out$H2_sameInformation_perYear[arm == "temporal" & contrast == "dTainted_seen"]
    sm <- out$H2_sameInformation[arm == "temporal" & contrast == "dTainted_seen"]
    sy[, cLabel := "PreVal - Status quo"]; sm[, cLabel := "PreVal - Status quo"]
    save(ggplot(sy, aes(value, factor(testYear))) + geom_vline(xintercept = 0, linetype = 2) + geom_point(alpha = 0.7) +
           geom_errorbarh(data = sm, aes(xmin = lo, xmax = hi, y = 0.4), inherit.aes = FALSE, height = 0.25, colour = "#1F6FB5") +
           geom_point(data = sm, aes(estimate, 0.4), inherit.aes = FALSE, shape = 18, size = 3, colour = "#1F6FB5") +
           facet_wrap(~ complexity, nrow = 1, labeller = labeller(complexity = function(x) paste(x, "covariates"))) + thm +
           labs(x = "Difference in test loss on animals PreVal saw in training (negative = PreVal better)", y = "Test year",
                title = "Same-information comparison", subtitle = "Only test strata of animals present in PreVal's training set"),
         "fig7_same_information.png", 11, 3.8)
  }


  # 9. Paired penalty curves: loss minus the same split's simplest-model loss (removes between-split differences)
  if (!is.null(out$H3_penalty_perSplit)) {
    pp <- out$H3_penalty_perSplit[arm == "temporal"]
    pp <- data.table::rbindlist(list(data.table::copy(pp)[, panel := "All horizons"], data.table::copy(pp)[, panel := paste("Horizon", horizonBin)]), fill = TRUE)
    pp[, panel := factor(panel, c("All horizons", paste("Horizon", c("1", "2", "3-4", "5+"))))]
    pp[, regime := factor(typeValidation, names(cols), labs[names(cols)])]
    ps <- pp[, .(med = stats::median(penalty), q25 = stats::quantile(penalty, .25), q75 = stats::quantile(penalty, .75)), by = .(panel, regime, complexity)]
    save(ggplot(ps, aes(complexity, med, colour = regime, fill = regime)) + geom_hline(yintercept = 0, linetype = 2) +
           geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.12, colour = NA) + geom_line(linewidth = 1.1) + geom_point(size = 2.5) +
           facet_wrap(~ panel, nrow = 1) + scale_colour_manual(values = colv) + scale_fill_manual(values = colv) +
           scale_x_continuous(breaks = sort(unique(ps$complexity))) + thm +
           labs(x = "Number of covariates", y = "Loss minus the same split's loss with the fewest covariates",
                title = "Does adding covariates hurt? Paired within each split",
                subtitle = "Line: median across splits. Band: middle 50% of splits. Above 0 = worse than the simplest model"),
         "fig9_paired_penalty_curves.png", 13, 4.8)
  }

  # 8. Absolute usefulness
  sk <- Mt[, .(v = mean(skillTop1)), by = .(regime, complexity, testYear)]
  ski <- out$skill_top1[arm == "temporal"]; ski[, regime := factor(typeValidation, names(cols), labs[names(cols)])]
  save(ggplot(sk, aes(factor(complexity), v, colour = regime)) + geom_hline(yintercept = 0, linetype = 2) +
         geom_point(alpha = 0.35, position = position_dodge(0.6)) +
         geom_errorbar(data = ski, aes(x = factor(complexity), ymin = lo, ymax = hi, colour = regime), inherit.aes = FALSE,
                       width = 0.25, linewidth = 0.9, position = position_dodge(0.6)) +
         geom_point(data = ski, aes(x = factor(complexity), y = estimate, colour = regime), inherit.aes = FALSE, size = 3, shape = 18,
                    position = position_dodge(0.6)) +
         scale_colour_manual(values = colv) + thm +
         labs(x = "Number of covariates", y = "Top-1 accuracy minus chance (1/11)", colour = NULL,
              title = "How much does the model actually know?", subtitle = "Dots: test years. Diamonds, bars: mean and t interval across years. Above 0 = better than guessing"),
       "fig8_skill_top1.png", 8, 4.5)
}
