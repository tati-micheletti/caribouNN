#' Pre-specified analysis of the experiment (see ANALYSIS_PLAN.md). Everything is computed from
#' the per-model result files; nothing is tuned after seeing results.
#'
#' Quantities per model (all in cross-entropy loss, lower = better; chance = log(11)):
#'   reported  = loss on the regime's own validation set at the best epoch (what that regime would report)
#'   realized  = loss on the shared test set (the future year's strata)
#'   optimism  = realized - reported   (H1: larger and growing with complexity for Tainted / Internal)
#'   skill     = log(11) - realized    (absolute usefulness; reported next to every contrast)
#' Uncertainty: percentile bootstrap resampling TEST YEARS (the unit that is truly independent;
#' splits sharing a test year share their test strata). Per-split contrasts are paired on the
#' identical test strata and complexity.
analyzeExperiment <- function(modelDir, outDir, nBoot = 2000L, seed = stringSeed("analysis"), chance = log(11)) {
  dir.create(outDir, recursive = TRUE, showWarnings = FALSE)
  files <- list.files(modelDir, pattern = "_finalDT\\.csv$", full.names = TRUE)
  if (!length(files)) stop("No result files in ", modelDir)
  M <- data.table::rbindlist(lapply(files, data.table::fread), fill = TRUE)
  M[, `:=`(reported = valLossBest, realized = testLossMean)]
  M[, `:=`(optimism = realized - reported, skill = chance - realized, trainTestGap = realized - trainLossBest)]
  M[, complexity := ifelse(is.infinite(numberOfCovariates), 30, numberOfCovariates)]
  data.table::fwrite(M, file.path(outDir, "allModels.csv"))

  boot <- function(dt, valueCol, byCols = character(0)) {
    # dt: one row per (split, ...) with testYear; resample test years
    f <- function(d) {
      if (nrow(d) == 0) return(data.table::data.table(estimate = NA_real_, lo = NA_real_, hi = NA_real_,
                                                      nSplits = 0L, nTestYears = 0L))   # e.g. slopes need >= 3 complexity levels
      yrs <- unique(d$testYear)
      est <- mean(d[[valueCol]])
      bs <- withSeed(seed, replicate(nBoot, { y <- yrs[sample.int(length(yrs), replace = TRUE)]   # NOT sample(yrs): breaks for one year
                                              mean(unlist(lapply(y, function(k) d[[valueCol]][d$testYear == k]))) }))
      data.table::data.table(estimate = est, lo = unname(stats::quantile(bs, 0.025)),
                             hi = unname(stats::quantile(bs, 0.975)), nSplits = nrow(d), nTestYears = length(yrs))
    }
    if (length(byCols)) dt[, f(.SD), by = byCols, .SDcols = names(dt)] else f(dt)
  }

  out <- list()
  # --- H1: optimism by regime x complexity x arm -----------------------------------------
  h1 <- M[, .(optimism = mean(optimism), testYear = testYear[1]), by = .(arm, typeValidation, complexity, splitId)]
  out$H1_optimism <- boot(h1, "optimism", c("arm", "typeValidation", "complexity"))
  # --- absolute realized loss and skill ----------------------------------------------------
  out$realized <- boot(M[, .(realized = mean(realized), skill = mean(skill), testYear = testYear[1]),
                         by = .(arm, typeValidation, complexity, splitId)], "realized", c("arm", "typeValidation", "complexity"))
  # --- H2: paired contrasts on identical test strata -----------------------------------------
  W <- data.table::dcast(M[, .(arm, splitId, complexity, replicate, testYear, typeValidation, realized, reported)],
                         arm + splitId + complexity + replicate + testYear ~ typeValidation,
                         value.var = c("realized", "reported"))
  W[, `:=`(PreVal_minus_Tainted = realized_FutureUnseen - realized_FutureTainted,
           PreVal_minus_Internal = realized_FutureUnseen - realized_Internal)]
  Wl <- data.table::melt(W, id.vars = c("arm", "splitId", "complexity", "replicate", "testYear"),
                         measure.vars = c("PreVal_minus_Tainted", "PreVal_minus_Internal"),
                         variable.name = "contrast", value.name = "diff")
  Wl <- Wl[, .(diff = mean(diff), testYear = testYear[1]), by = .(arm, contrast, complexity, splitId)]
  out$H2_contrasts <- boot(Wl, "diff", c("arm", "contrast", "complexity"))
  out$H2_contrasts_all <- boot(Wl[, .(diff = mean(diff), testYear = testYear[1]), by = .(arm, contrast, splitId)],
                               "diff", c("arm", "contrast"))
  # --- H3: complexity slope of realized loss (per unit log2 covariates), per regime ---------------
  sl <- M[, .(slope = if (.N >= 3) stats::coef(stats::lm(realized ~ log2(complexity)))[2] else NA_real_,
              slopeGap = if (.N >= 3) stats::coef(stats::lm(trainTestGap ~ log2(complexity)))[2] else NA_real_,
              testYear = testYear[1]), by = .(arm, typeValidation, splitId, replicate)]
  sl <- sl[!is.na(slope), .(slope = mean(slope), slopeGap = mean(slopeGap), testYear = testYear[1]), by = .(arm, typeValidation, splitId)]
  out$H3_slope_realized <- boot(sl, "slope", c("arm", "typeValidation"))
  out$H3_slope_trainTestGap <- boot(sl, "slopeGap", c("arm", "typeValidation"))
  # --- Selection regret: pick the complexity with the best REPORTED loss within each regime ------
  sel <- M[, {
    k <- which.min(reported)
    .(chosen = complexity[k], realizedChosen = realized[k], oracle = min(realized), testYear = testYear[1])
  }, by = .(arm, typeValidation, splitId, replicate)]
  sel[, regret := realizedChosen - oracle]
  sel <- sel[, .(realizedChosen = mean(realizedChosen), regret = mean(regret), testYear = testYear[1]), by = .(arm, typeValidation, splitId)]
  out$selection_regret <- boot(sel, "regret", c("arm", "typeValidation"))
  out$selection_realized <- boot(sel, "realizedChosen", c("arm", "typeValidation"))
  # --- Seen vs unseen animals ---------------------------------------------------------------------
  out$seen_unseen <- M[, .(seenShare = mean(seenShare), testLossSeen = mean(testLossSeen, na.rm = TRUE),
                           testLossUnseen = mean(testLossUnseen, na.rm = TRUE)), by = .(arm, typeValidation)]
  for (nm in names(out)) data.table::fwrite(out[[nm]], file.path(outDir, paste0(nm, ".csv")))

  if (requireNamespace("ggplot2", quietly = TRUE)) {
    g <- function(d, y, ylab, file) {
      p <- ggplot2::ggplot(d, ggplot2::aes(factor(complexity), estimate, colour = typeValidation, group = typeValidation)) +
        ggplot2::geom_pointrange(ggplot2::aes(ymin = lo, ymax = hi), position = ggplot2::position_dodge(0.4)) +
        ggplot2::geom_line(position = ggplot2::position_dodge(0.4)) + ggplot2::facet_wrap(~arm) +
        ggplot2::labs(x = "Number of covariates", y = ylab, colour = "Regime") + ggplot2::theme_bw()
      ggplot2::ggsave(file.path(outDir, file), p, width = 8, height = 4.5, dpi = 200)
    }
    g(out$H1_optimism, "optimism", "Optimism = realized - reported loss", "fig_H1_optimism.png")
    g(out$realized, "realized", "Realized loss on the shared test set (chance = 2.398)", "fig_realized_loss.png")
  }
  invisible(out)
}
