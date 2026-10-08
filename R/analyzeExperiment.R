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
#' @param sameInfo also compute contrasts restricted to test strata of animals PreVal saw in training (reads the
#'   per-stratum files; takes a few minutes)
analyzeExperiment <- function(modelDir, outDir, margin = 0.005, chance = log(11), sameInfo = TRUE) {
  stopifnot(requireNamespace("data.table", quietly = TRUE))
  dir.create(outDir, recursive = TRUE, showWarnings = FALSE)
  files <- list.files(modelDir, pattern = "_finalDT\\.csv$", full.names = TRUE)
  if (!length(files)) stop("No result files in ", modelDir)
  M <- data.table::rbindlist(lapply(files, data.table::fread), fill = TRUE)
  if (!"epochCap" %in% names(M)) M[, epochCap := 50L]            # results from before the cap was recorded
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
  W[, `:=`(PreVal_minus_Tainted = realized_FutureUnseen - realized_FutureTainted,
           PreVal_minus_Internal = realized_FutureUnseen - realized_Internal,
           optGapTainted = optimism_FutureTainted - optimism_FutureUnseen,
           optGapInternal = optimism_Internal - optimism_FutureUnseen)]
  data.table::fwrite(W, file.path(outDir, "pairedContrasts_perSplit.csv"))

  # ---- Table 1: design (strata / animals / bursts per set) ---------------------------------------------
  out$table1_design <- M[, .(nTrain = mean(nTrain), nVal = mean(nVal), nTest = mean(nTest),
                             animalsTrain = mean(nAnimalsTrain), burstsTrain = mean(nBurstsTrain),
                             animalsTest = mean(nAnimalsTest), models = .N), by = .(arm, typeValidation)]

  # ---- H1: reported vs realized, optimism, and the primary contrast (difference in optimism) --------------
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

  # 1. Reported vs realized: points below the 1:1 line = the regime under-reports its future error
  save(ggplot(Mt, aes(reported, realized, colour = regime)) + geom_abline(slope = 1, intercept = 0, linetype = 2) +
         geom_point(alpha = 0.5, size = 1) + facet_wrap(~ complexity, scales = "free", labeller = labeller(complexity = function(x) paste(x, "covariates"))) +
         scale_colour_manual(values = colv) + thm +
         labs(x = "Reported loss (own validation set)", y = "Realized loss (shared test set)", colour = NULL,
              title = "Does the regime's own estimate match the future?",
              subtitle = "Dashed line: reported = realized. Above the line = future error is worse than reported"),
       "fig1_reported_vs_realized.png", 9, 6.5)

  # 2. Forest plot: per-test-year contrasts (PreVal minus comparator; negative = PreVal better)
  fy <- out$H2_perYear[arm == "temporal"]; fm <- out$H2_contrasts[arm == "temporal"]
  fy[, cLabel := ifelse(contrast == "PreVal_minus_Tainted", "PreVal - Status quo", "PreVal - Random CV")]
  fm[, cLabel := ifelse(contrast == "PreVal_minus_Tainted", "PreVal - Status quo", "PreVal - Random CV")]
  save(ggplot(fy, aes(value, factor(testYear))) + geom_vline(xintercept = 0, linetype = 2) + geom_point(alpha = 0.7) +
         geom_errorbarh(data = fm, aes(xmin = lo, xmax = hi, y = 0.4), inherit.aes = FALSE, height = 0.25, colour = "#1F6FB5") +
         geom_point(data = fm, aes(estimate, 0.4), inherit.aes = FALSE, shape = 18, size = 3, colour = "#1F6FB5") +
         facet_grid(cLabel ~ complexity, labeller = labeller(complexity = function(x) paste(x, "covariates"))) + thm +
         labs(x = "Difference in realized test loss (negative = PreVal better)", y = "Test year",
              title = "PreVal vs the comparators, one dot per test year",
              subtitle = "Blue diamond and bar: mean and t interval across test years (paired on identical test strata)"),
       "fig2_forest_contrasts.png", 11, 6.5)

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
  Wh <- Wl[arm == "temporal"]; Wh[, cLabel := ifelse(contrast == "PreVal_minus_Tainted", "PreVal - Status quo", "PreVal - Random CV")]
  save(ggplot(Wh, aes(horizon, diff)) + geom_hline(yintercept = 0, linetype = 2) + geom_jitter(width = 0.1, alpha = 0.25, size = 0.8) +
         geom_smooth(method = "lm", formula = y ~ x, colour = "#1F6FB5", se = TRUE) + facet_wrap(~ cLabel) + thm +
         scale_x_continuous(breaks = 1:10) +
         labs(x = "Forecast horizon (test year minus last history year)", y = "Difference in realized test loss",
              title = "Does the PreVal advantage depend on how far ahead we forecast?"),
       "fig4_horizon.png", 9, 4.5)

  # 5. Learning curves of example models (largest split, most covariates) + how training ended
  ex <- Mt[arm == "temporal"][which.max(nTrain)]
  exm <- Mt[splitId == ex$splitId & numberOfCovariates == max(numberOfCovariates)]
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
              subtitle = "After the extension pass no model should be stopped by the cap") + theme(axis.text.x = element_text(angle = 15, hjust = 1)),
       "fig6_how_training_ended.png", 7, 4)

  # 7. Same-information contrasts
  if (!is.null(out$H2_sameInformation_perYear)) {
    sy <- out$H2_sameInformation_perYear[arm == "temporal" & grepl("_seen", contrast)]
    sm <- out$H2_sameInformation[arm == "temporal" & grepl("_seen", contrast)]
    sy[, cLabel := ifelse(grepl("Tainted", contrast), "PreVal - Status quo", "PreVal - Random CV")]
    sm[, cLabel := ifelse(grepl("Tainted", contrast), "PreVal - Status quo", "PreVal - Random CV")]
    save(ggplot(sy, aes(value, factor(testYear))) + geom_vline(xintercept = 0, linetype = 2) + geom_point(alpha = 0.7) +
           geom_errorbarh(data = sm, aes(xmin = lo, xmax = hi, y = 0.4), inherit.aes = FALSE, height = 0.25, colour = "#1F6FB5") +
           geom_point(data = sm, aes(estimate, 0.4), inherit.aes = FALSE, shape = 18, size = 3, colour = "#1F6FB5") +
           facet_grid(cLabel ~ complexity, labeller = labeller(complexity = function(x) paste(x, "covariates"))) + thm +
           labs(x = "Difference in test loss on animals PreVal saw in training", y = "Test year",
                title = "Same-information comparison", subtitle = "Only test strata of animals present in PreVal's training set"),
         "fig7_same_information.png", 11, 6.5)
  }

  # 8. Absolute usefulness
  sk <- Mt[, .(v = mean(skillTop1)), by = .(regime, complexity, testYear)]
  save(ggplot(sk, aes(factor(complexity), v, colour = regime)) + geom_hline(yintercept = 0, linetype = 2) +
         geom_point(alpha = 0.4, position = position_dodge(0.5)) +
         stat_summary(fun = mean, geom = "point", size = 3, shape = 18, position = position_dodge(0.5)) +
         scale_colour_manual(values = colv) + thm +
         labs(x = "Number of covariates", y = "Top-1 accuracy minus chance (1/11)", colour = NULL,
              title = "How much does the model actually know?", subtitle = "Dots: test years. Diamonds: mean. 0 = guessing"),
       "fig8_skill_top1.png", 8, 4.5)
}
