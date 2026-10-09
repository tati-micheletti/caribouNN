#' Follow-up experiment: does the complexity "hump" of the status quo follow the COVARIATES or the COUNT?
#'
#' The main ladder adds covariates in the order of the global importance ranking (2, 5, 10, 20, 30). The status quo
#' shows a hump at 5-10 covariates (loss rises, then returns at 30), PreVal does not. Two readings:
#'   B. the covariates that enter at positions 6-10 (mostly step-length interactions, `inter_logSl_x_*`) carry signal
#'      that does not transfer to a later year, and a validation set drawn from the same years cannot see that;
#'   C. the plain habitat covariates that enter later carry stable signal and pull the curve back down.
#' If the hump follows the count, it will show up in every ordering. If it follows the covariates, it moves with them.
#'
#' Feature sets (all share the splits, regimes' strata and seeds' scheme of the main design; only the covariate list changes):
#'   habitatOnly   habitat covariates only (movement covariates removed = the ablation of the interaction terms)
#'   habitatFirst  all covariates, habitat covariates first, then movement ones
#'   movementFirst all covariates, movement covariates first, then habitat ones
#'   randomA/B     all covariates in a seeded random order
#' Only the two forecasting regimes (PreVal = FutureUnseen and the status quo = FutureTainted) are run: the random-CV
#' reference is not a forecast and is never compared with PreVal.

movementFamily <- function(features, movementPattern = "^(logSl|cosTa|sinTa|inter_logSl_x_)") {
  grepl(movementPattern, features)
}

#' @param featureTable data.table with column `Feature` in importance order (the main ladder order)
#' @return data.table(set, position, Feature): the ordered covariate list of every set
buildFeatureSets <- function(featureTable, sets = c("habitatOnly", "habitatFirst", "movementFirst", "randomA", "randomB"),
                             movementPattern = "^(logSl|cosTa|sinTa|inter_logSl_x_)") {
  f <- as.character(featureTable$Feature)
  mv <- movementFamily(f, movementPattern)
  habitat <- f[!mv]; movement <- f[mv]
  ord <- list(habitatOnly = habitat, habitatFirst = c(habitat, movement), movementFirst = c(movement, habitat))
  # Interaction experiment. Each `inter_logSl_x_<X>_start*` term multiplies the log step length with a covariate measured at the
  # START of the step. Start covariates are the same for all 11 candidate steps of a stratum, so on their own they cannot tell the
  # candidates apart (they can only act together with step-varying covariates). The matching `_end*` covariate is measured at the
  # candidate's end point and does discriminate. Sets, all in the importance order of the interaction terms:
  #   interactionsOnly   the interaction terms
  #   endOnly            the matching end-point covariates (habitat selection)
  #   startOnly          the matching start covariates (constant within a stratum)
  #   endPlusInteractions  both, interleaved in pairs
  inter <- f[grepl("^inter_logSl_x_", f)]
  startOf <- sub("^inter_logSl_x_", "", inter)
  endOf <- sub("_startLog$", "_endLog", sub("_start$", "_end", startOf))
  if (length(inter) && all(startOf %in% f) && all(endOf %in% f)) {
    ord$interactionsOnly <- inter
    ord$endOnly <- endOf
    ord$startOnly <- startOf
    ord$endPlusInteractions <- as.vector(rbind(inter, endOf))
  }
  for (s in grep("^random", sets, value = TRUE))
    ord[[s]] <- withSeed(stringSeed(paste("featureSet", s)), sample(f))
  stopifnot(all(sets %in% names(ord)))
  data.table::rbindlist(lapply(sets, function(s) data.table::data.table(set = s, position = seq_along(ord[[s]]), Feature = ord[[s]])))
}

#' Build the plan of the feature-set arm from the saved main plan (same splits, same manifests and tensor store).
#' @param plan the main experiment plan (data.table)
#' @param featureSets output of buildFeatureSets
#' @param levels covariate counts (truncated to the length of each set)
#' @param regimes regimes to run
#' @param arms design arms to run ("temporal" only by default: the spatial arm has too few test years)
makeFeatureSetPlan <- function(plan, featureSets, levels = c(2, 5, 10, 20), regimes = c("FutureUnseen", "FutureTainted"),
                               arms = "temporal", registryPath = NULL) {
  base <- unique(plan[replicate == 1 & arm %in% arms & typeValidation %in% regimes & numberOfCovariates == min(numberOfCovariates)])
  stopifnot(nrow(base) > 0)
  out <- data.table::rbindlist(lapply(unique(featureSets$set), function(s) {
    n <- featureSets[set == s, .N]
    lv <- sort(unique(pmin(levels, n)))
    data.table::rbindlist(lapply(lv, function(k) {
      x <- data.table::copy(base); x[, `:=`(featureSet = s, numberOfCovariates = k, replicate = 1L)]; x
    }))
  }))
  out[, groupId := paste0("FS_", featureSet, "_", numberOfCovariates, "_", windowStart, "_", historyEnd, "_", testYear)]
  out[, modelName := paste0(groupId, "_", typeValidation)]
  out[, modelSeed := mapply(function(sid, s, k) stringSeed(paste(sid, "fs", s, "cov", k)), splitId, featureSet, numberOfCovariates)]
  if (anyDuplicated(out$modelName)) stop("Duplicated model names in the feature-set plan.")
  if (!is.null(registryPath)) {
    u <- unique(out[, .(splitId, featureSet, numberOfCovariates, modelSeed)])
    registerSeeds(registryPath, "caribouNN::featureSetModels", paste(u$splitId, "fs", u$featureSet, "cov", u$numberOfCovariates),
                  u$modelSeed, "torch seed of the feature-set arm (shared by the two regimes of a cell)")
  }
  out[]
}

# ---- analysis of the arm ---------------------------------------------------------------------------------------------

# mean across test years of the within-year means, t interval, exact sign-flip p (the test year is the independent unit)
.yearSummary <- function(value, year) {
  y <- tapply(value, year, mean); y <- y[is.finite(y)]; n <- length(y)
  if (n < 2) return(data.table::data.table(estimate = mean(y), lo = NA_real_, hi = NA_real_, pSignFlip = NA_real_, nYears = n, nPositive = sum(y > 0)))
  m <- mean(y); se <- stats::sd(y) / sqrt(n); q <- stats::qt(0.975, n - 1)
  signs <- as.matrix(expand.grid(rep(list(c(-1, 1)), n)))
  p <- mean(abs(signs %*% y / n) >= abs(m) - 1e-12)
  data.table::data.table(estimate = m, lo = m - q * se, hi = m + q * se, pSignFlip = p, nYears = n, nPositive = sum(y > 0))
}

#' Analyse the feature-set arm, with the main ladder (replicate 1, temporal, same two regimes) as the reference set "main".
#' Writes tables and one figure to outDir.
analyzeFeatureSets <- function(modelDir, mainDir, outDir, regimes = c("FutureUnseen", "FutureTainted")) {
  dir.create(outDir, recursive = TRUE, showWarnings = FALSE)
  rd <- function(d) data.table::rbindlist(lapply(list.files(d, pattern = "_finalDT\\.csv$", full.names = TRUE), data.table::fread), fill = TRUE)
  A <- rd(modelDir)
  if (!nrow(A)) stop("No feature-set results in ", modelDir)
  if (!"featureSet" %in% names(A)) A[, featureSet := sub("^FS_([A-Za-z0-9]+)_.*", "\\1", modelName)]
  M <- rd(mainDir)[arm == "temporal" & replicate == 1 & typeValidation %in% regimes]
  M[, featureSet := "main"]
  M[, numberOfCovariates := ifelse(is.infinite(numberOfCovariates), 30, numberOfCovariates)]
  D <- data.table::rbindlist(list(A, M), fill = TRUE)
  D[, `:=`(realized = testLossMean, k = as.numeric(numberOfCovariates))]
  data.table::fwrite(D[, .(featureSet, typeValidation, splitId, testYear, horizon, k, realized, bestEpoch, epochsRun)],
                     file.path(outDir, "featureSets_allModels.csv"))
  out <- list()
  # levels
  out$levels <- D[, .(meanLoss = mean(realized), medianLoss = stats::median(realized), nSplits = .N),
                  by = .(featureSet, typeValidation, k)][order(featureSet, typeValidation, k)]
  # penalty relative to the same set's smallest level, paired within the split
  D[, penalty := realized - realized[which.min(k)], by = .(featureSet, typeValidation, splitId)]
  out$penalty_by_year <- D[k > min(k) | TRUE, .(penalty = mean(penalty)), by = .(featureSet, typeValidation, k, testYear)]
  out$penalty <- D[, .yearSummary(penalty, testYear), by = .(featureSet, typeValidation, k)][order(featureSet, typeValidation, k)]
  # PEAK (mean of the intermediate levels) and END (largest level), PreVal and status quo, and their difference per split
  sh <- D[, {
    lv <- sort(unique(k)); mid <- lv[-c(1, length(lv))]
    .(peak = if (length(mid)) mean(penalty[k %in% mid]) else NA_real_, end = penalty[k == max(k)][1])
  }, by = .(featureSet, typeValidation, splitId, testYear, horizon)]
  out$shape_per_regime <- data.table::rbindlist(lapply(c("peak", "end"), function(v)
    sh[, .yearSummary(get(v), testYear), by = .(featureSet, typeValidation)][, measure := v]))
  w <- data.table::dcast(sh, featureSet + splitId + testYear + horizon ~ typeValidation, value.var = c("peak", "end"))
  if (all(c("peak_FutureUnseen", "peak_FutureTainted") %in% names(w))) {
    w[, `:=`(peak_PreVal_minus_Tainted = peak_FutureUnseen - peak_FutureTainted, end_PreVal_minus_Tainted = end_FutureUnseen - end_FutureTainted)]
    out$shape_difference <- data.table::rbindlist(lapply(c("peak_PreVal_minus_Tainted", "end_PreVal_minus_Tainted"), function(v)
      w[, .yearSummary(get(v), testYear), by = featureSet][, contrast := v]))
  }
  for (nm in names(out)) data.table::fwrite(out[[nm]], file.path(outDir, paste0("FS_", nm, ".csv")))
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    library(ggplot2)
    cols <- c(FutureUnseen = "#1F6FB5", FutureTainted = "#E08A00"); labs <- c(FutureUnseen = "PreVal", FutureTainted = "Status quo")
    pp <- D[, .(med = stats::median(penalty), q25 = stats::quantile(penalty, .25), q75 = stats::quantile(penalty, .75)), by = .(featureSet, typeValidation, k)]
    pp[, regime := factor(typeValidation, names(cols), labs[names(cols)])]
    pp[, featureSet := factor(featureSet, c("main", setdiff(unique(featureSet), "main")))]
    g <- ggplot(pp, aes(k, med, colour = regime, fill = regime)) + geom_hline(yintercept = 0, linetype = 2) +
      geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.12, colour = NA) + geom_line(linewidth = 1) + geom_point(size = 2) +
      facet_wrap(~ featureSet, nrow = if (length(unique(pp$featureSet)) > 4) 2 else 1) + scale_colour_manual(values = unname(setNames(cols, labs[names(cols)]))) +
      scale_fill_manual(values = unname(setNames(cols, labs[names(cols)]))) + theme_bw(base_size = 12) + theme(legend.position = "bottom") +
      labs(x = "Number of covariates", y = "Loss minus the same split's loss with the set's smallest level",
           title = "Does the hump follow the covariates or the count?",
           subtitle = "One panel per ordering of the covariates (main = importance order). Line: median over splits; band: middle 50%")
    ggplot2::ggsave(file.path(outDir, "figFS_hump_by_covariate_order.png"), g, width = 11, height = 6, dpi = 200)
  }
  invisible(out)
}
