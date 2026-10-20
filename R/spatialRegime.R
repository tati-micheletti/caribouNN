#' Extra regime: the status quo with SPATIALLY blocked validation ("FutureTaintedSpatial").
#'
#' Question: is spatial blocking enough? The status quo (FutureTainted) validates on a random part of the training years.
#' A modeller who knows about spatial dependence would hold out whole spatial blocks instead, but still from the same years
#' as training. This regime does exactly that, on the SAME pool and the SAME shared test set as the other regimes:
#'   * pool = train + validation strata of PreVal (years s..e), the same pool as the status quo;
#'   * validation = whole spatial blocks (random order, seeded) until the validation size of PreVal is reached
#'     (the last block is trimmed at random so the size is exact);
#'   * train = the rest of the pool, minus every stratum inside a validation block or its buffer;
#'   * test = the shared test set (year T) of the split, unchanged.
#' Model seeds are shared with the status-quo cell (same initial weights), so only the allocation rule differs.
#' Training is slightly smaller than the status quo's (the buffer is removed); sizes are reported per split. Splits where blocking
#' would leave less than 80% of the status-quo training set are skipped, so the contrast is not confounded by training size.

# strata inside the given blocks or within the buffer around them
.blockZone <- function(strataIdx, spatial, blocks) {
  B <- spatial$blockKm * 1000; buf <- spatial$bufferKm * 1000
  excluded <- spatial$key %in% blocks
  if (buf > 0 && length(blocks)) {
    parts <- do.call(rbind, strsplit(blocks, " ", fixed = TRUE))
    tbx <- as.numeric(parts[, 1]); tby <- as.numeric(parts[, 2])
    x <- strataIdx$x1_; y <- strataIdx$y1_
    for (k in seq_along(tbx)) {
      x0 <- tbx[k] * B; x1 <- x0 + B; y0 <- tby[k] * B; y1 <- y0 + B
      near <- !excluded & abs(spatial$bx - tbx[k]) <= 1 & abs(spatial$by - tby[k]) <= 1
      if (!any(near)) next
      cand <- which(near)
      hit <- x[cand] >= x0 - buf & x[cand] <= x1 + buf & y[cand] >= y0 - buf & y[cand] <= y1 + buf
      excluded[cand[hit]] <- TRUE
    }
  }
  excluded
}

#' @param unseen manifest (row, set) of the FutureUnseen regime of the split
#' @param spatial list from assignSpatialBlocks()
buildSpatialTainted <- function(strataIdx, unseen, spatial, seed, minTrain = 500L, minTrainFraction = 0.8) {
  U <- unseen$row[unseen$set %in% c("train", "val")]
  nVal <- sum(unseen$set == "val"); test <- unseen$row[unseen$set == "test"]
  key <- spatial$key[U]
  ord <- withSeed(seed + 4L, sample(unique(key)))
  cnt <- as.numeric(table(key)[ord])
  nB <- which(cumsum(cnt) >= nVal)[1]
  if (is.na(nB)) return(list(ok = FALSE, reason = "validation size larger than the pool"))
  valBlocks <- ord[seq_len(nB)]
  valRows <- U[key %in% valBlocks]
  if (length(valRows) > nVal) {                       # trim the last block at random so the size equals PreVal's validation size
    last <- U[key == ord[nB]]
    drop <- last[withSeed(seed + 5L, sample.int(length(last), length(valRows) - nVal))]
    valRows <- setdiff(valRows, drop)
  }
  zone <- .blockZone(strataIdx, spatial, valBlocks)
  trainRows <- U[!zone[U]]
  if (length(trainRows) < minTrain) return(list(ok = FALSE, reason = sprintf("training too small after the buffer (%d)", length(trainRows))))
  # keep the comparison with the status quo about equal in data: skip splits where blocking removes too much of the training set
  if (length(trainRows) < minTrainFraction * sum(unseen$set == "train"))
    return(list(ok = FALSE, reason = sprintf("spatial blocking leaves only %d of %d status-quo training strata", length(trainRows), sum(unseen$set == "train"))))
  list(ok = TRUE, nValBlocks = nB, nRemoved = length(U) - length(trainRows) - length(valRows),
       split = data.table::rbindlist(list(data.table::data.table(row = sort(trainRows), set = "train"),
                                          data.table::data.table(row = sort(valRows), set = "val"),
                                          data.table::data.table(row = test, set = "test"))),
       sizes = c(nTrain = length(trainRows), nVal = length(valRows), nTest = length(test)))
}

#' Hard gate for the new regime (stops on failure).
verifySpatialTainted <- function(strataIdx, m, unseen, spatial, splitId = "") {
  tr <- m$row[m$set == "train"]; va <- m$row[m$set == "val"]; te <- m$row[m$set == "test"]
  U <- unseen$row[unseen$set %in% c("train", "val")]
  res <- list(); add <- function(n, ok) res[[length(res) + 1L]] <<- data.table::data.table(splitId = splitId, check = n, ok = isTRUE(ok))
  add("R1 train, validation and test are disjoint", !anyDuplicated(m$row))
  add("R2 test set identical to the shared test set", setequal(te, unseen$row[unseen$set == "test"]))
  add("R3 train + validation inside the status-quo pool", all(c(tr, va) %in% U))
  add("R4 validation size equals PreVal's validation size", length(va) == sum(unseen$set == "val"))
  vb <- unique(spatial$key[va]); zone <- .blockZone(strataIdx, spatial, vb)
  add("R5 no training stratum inside a validation block or its buffer", !any(zone[tr]))
  add("R6 validation strata are whole-block strata (every validation stratum in a validation block)", all(spatial$key[va] %in% vb))
  add("R7 the test year is not in the pool", !any(strataIdx$year[U] == unique(strataIdx$year[te])))
  out <- data.table::rbindlist(res)
  if (any(!out$ok)) stop("verifySpatialTainted FAILED for ", splitId, ":\n", paste(utils::capture.output(print(out[ok == FALSE])), collapse = "\n"))
  out
}

#' Build and write the manifests of the new regime for every temporal split and derive its plan from the status-quo rows.
#' Manifests are written once and never overwritten with different content.
makeSpatialRegimeDesign <- function(strataIdx, plan, manifestDir, spatial, regime = "FutureTaintedSpatial", minTrain = 500L, minTrainFraction = 0.8) {
  tp <- plan[arm == "temporal" & typeValidation == "FutureTainted"]
  summ <- list(); checks <- list(); skipped <- list()
  for (sid in unique(tp$splitId)) {
    un <- readManifest(manifestDir, sid, "FutureUnseen")
    seed <- tp[splitId == sid]$splitSeed[1]
    b <- buildSpatialTainted(strataIdx, un, spatial, seed, minTrain, minTrainFraction)
    if (!isTRUE(b$ok)) { skipped[[sid]] <- data.table::data.table(splitId = sid, reason = b$reason); next }
    checks[[sid]] <- verifySpatialTainted(strataIdx, b$split, un, spatial, sid)
    m <- data.table::copy(b$split); m[, indiv_step_id := strataIdx$indiv_step_id[match(row, strataIdx$row)]]
    p <- file.path(manifestDir, paste0(sid, "__", regime, ".rds"))
    if (file.exists(p)) {
      old <- readRDS(p)
      if (!isTRUE(all.equal(old[order(set, row)], m[order(set, row)], check.attributes = FALSE)))
        stop("Existing manifest differs from the newly built one: ", p)
    } else saveRDS(m, p)
    summ[[sid]] <- data.table::data.table(splitId = sid, nTrain = b$sizes[["nTrain"]], nVal = b$sizes[["nVal"]], nTest = b$sizes[["nTest"]],
                                          nTrainStatusQuo = sum(un$set == "train"), nValBlocks = b$nValBlocks, nRemovedBuffer = b$nRemoved)
  }
  summ <- data.table::rbindlist(summ)
  if (!nrow(summ)) stop("No split could be built for the regime ", regime)
  x <- merge(tp, summ[, .(splitId, nTrainNew = nTrain)], by = "splitId")
  x[, `:=`(typeValidation = regime, nTrainS = nTrainNew, modelName = sub("_FutureTainted$", paste0("_", regime), modelName))]
  x[, nTrainNew := NULL]
  if (anyDuplicated(x$modelName)) stop("Duplicated model names in the regime plan.")
  list(plan = x[], summary = summ[], checks = data.table::rbindlist(checks),
       skipped = if (length(skipped)) data.table::rbindlist(skipped) else NULL)
}

#' Analyse the regime arm against the main experiment (PreVal = FutureUnseen and the status quo = FutureTainted).
analyzeRegimeArm <- function(modelDir, mainDir, outDir, regime = "FutureTaintedSpatial") {
  dir.create(outDir, recursive = TRUE, showWarnings = FALSE)
  rd <- function(d) data.table::rbindlist(lapply(list.files(d, pattern = "_finalDT\\.csv$", full.names = TRUE), data.table::fread), fill = TRUE)
  A <- rd(modelDir)
  if (!nrow(A)) stop("No results in ", modelDir)
  M <- rd(mainDir)[arm == "temporal" & typeValidation %in% c("FutureUnseen", "FutureTainted")]
  D <- data.table::rbindlist(list(A, M), fill = TRUE)
  D[, `:=`(realized = testLossMean, reported = valLossBest, k = ifelse(is.infinite(numberOfCovariates), 30, numberOfCovariates))]
  D[, optimism := realized - reported]
  data.table::fwrite(D[, .(typeValidation, splitId, replicate, testYear, horizon, k, realized, reported, optimism, nTrain, nVal, bestEpoch, epochsRun)],
                     file.path(outDir, "regimeArm_allModels.csv"))
  cell <- c("splitId", "k", "replicate", "testYear", "horizon")
  W <- data.table::dcast(D, splitId + k + replicate + testYear + horizon ~ typeValidation, value.var = c("realized", "optimism"))
  rS <- paste0("realized_", regime); oS <- paste0("optimism_", regime)
  stopifnot(all(c(rS, "realized_FutureTainted", "realized_FutureUnseen") %in% names(W)))
  W <- W[is.finite(W[[rS]]) & is.finite(realized_FutureTainted) & is.finite(realized_FutureUnseen)]
  W[, `:=`(spatial_minus_status = get(rS) - realized_FutureTainted, preval_minus_spatial = realized_FutureUnseen - get(rS),
           preval_minus_status = realized_FutureUnseen - realized_FutureTainted,
           optimism_spatial_minus_status = get(oS) - optimism_FutureTainted, optimism_preval_minus_spatial = optimism_FutureUnseen - get(oS))]
  out <- list()
  out$levels <- D[, .(meanLoss = mean(realized), medianLoss = stats::median(realized), meanReported = mean(reported),
                      meanOptimism = mean(optimism), nModels = .N), by = .(typeValidation, k)][order(typeValidation, k)]
  summ <- function(v) data.table::rbindlist(list(
    W[, .yearSummary(get(v), testYear), by = k][, scope := "by covariates"][order(k)],
    W[, .yearSummary(get(v), testYear)][, `:=`(k = NA_real_, scope = "pooled")]), fill = TRUE)[, contrast := v]
  out$contrasts <- data.table::rbindlist(lapply(c("spatial_minus_status", "preval_minus_spatial", "preval_minus_status",
                                                  "optimism_spatial_minus_status", "optimism_preval_minus_spatial"), summ))
  out$optimism_by_regime <- D[, .yearSummary(optimism, testYear), by = .(typeValidation, k)][order(typeValidation, k)]
  out$optimism_pooled <- D[, .yearSummary(optimism, testYear), by = typeValidation]
  # complexity shape per regime (penalty vs the smallest level, paired within split and replicate)
  D[, penalty := realized - realized[which.min(k)], by = .(typeValidation, splitId, replicate)]
  out$penalty <- D[, .yearSummary(penalty, testYear), by = .(typeValidation, k)][order(typeValidation, k)]
  sh <- D[, {lv <- sort(unique(k)); mid <- lv[-c(1, length(lv))]
             .(peak = if (length(mid)) mean(penalty[k %in% mid]) else NA_real_, end = penalty[k == max(k)][1])},
          by = .(typeValidation, splitId, replicate, testYear, horizon)]
  out$shape <- data.table::rbindlist(lapply(c("peak", "end"), function(v) sh[, .yearSummary(get(v), testYear), by = typeValidation][, measure := v]))
  sw <- data.table::dcast(sh, splitId + replicate + testYear + horizon ~ typeValidation, value.var = c("peak", "end"))
  for (v in c("peak", "end")) {
    cu <- paste0(v, "_FutureUnseen"); cs <- paste0(v, "_", regime); ct <- paste0(v, "_FutureTainted")
    if (all(c(cu, cs, ct) %in% names(sw))) {
      sw[, paste0(v, "_preval_minus_spatial") := get(cu) - get(cs)]
      sw[, paste0(v, "_spatial_minus_status") := get(cs) - get(ct)]
    }
  }
  out$shape_difference <- data.table::rbindlist(lapply(grep("_minus_", names(sw), value = TRUE), function(v)
    sw[is.finite(get(v)), .yearSummary(get(v), testYear)][, contrast := v]))
  out$sizes <- D[typeValidation %in% c("FutureTainted", regime), .(meanTrain = mean(nTrain), meanVal = mean(nVal)), by = typeValidation]
  for (nm in names(out)) data.table::fwrite(out[[nm]], file.path(outDir, paste0("RA_", nm, ".csv")))
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    library(ggplot2)
    cols <- c(FutureUnseen = "#1F6FB5", FutureTainted = "#E08A00", "#7B3F9E"); names(cols)[3] <- regime
    labs <- c(FutureUnseen = "PreVal (validate on the next year)", FutureTainted = "Status quo (random CV, same years)"); labs[regime] <- "Spatial-block CV (same years)"
    D[, regimeLab := factor(typeValidation, names(cols), labs[names(cols)])]
    cp <- D[, .(med = stats::median(realized), q25 = stats::quantile(realized, .25), q75 = stats::quantile(realized, .75)), by = .(regimeLab, k)]
    g1 <- ggplot(cp, aes(k, med, colour = regimeLab, fill = regimeLab)) + geom_hline(yintercept = log(11), linetype = 3) +
      geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.12, colour = NA) + geom_line(linewidth = 1.1) + geom_point(size = 2.5) +
      scale_colour_manual(values = unname(cols[names(labs)]), name = NULL) + scale_fill_manual(values = unname(cols[names(labs)]), name = NULL) +
      scale_x_continuous(breaks = sort(unique(cp$k))) + theme_bw(base_size = 12) + theme(legend.position = "bottom") +
      labs(x = "Number of covariates", y = "Forecast loss on the test year (median over splits)",
           title = "Is spatial blocking enough?", subtitle = "Dotted line: chance. Band: middle 50% of splits")
    ggplot2::ggsave(file.path(outDir, "figRA_loss_by_complexity.png"), g1, width = 8.5, height = 5.5, dpi = 200)
    op <- out$optimism_by_regime; op[, regimeLab := factor(typeValidation, names(cols), labs[names(cols)])]
    g2 <- ggplot(op, aes(k, estimate, colour = regimeLab)) + geom_hline(yintercept = 0, linetype = 2) +
      geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.6, alpha = 0.7) + geom_line(linewidth = 1) + geom_point(size = 2.5) +
      scale_colour_manual(values = unname(cols[names(labs)]), name = NULL) + scale_x_continuous(breaks = sort(unique(op$k))) +
      theme_bw(base_size = 12) + theme(legend.position = "bottom") +
      labs(x = "Number of covariates", y = "Forecast loss minus the loss the validation reported",
           title = "How much does each validation understate the forecast error?",
           subtitle = "Zero = the reported error is honest. Mean and 95% interval across test years")
    ggplot2::ggsave(file.path(outDir, "figRA_optimism.png"), g2, width = 8.5, height = 5.5, dpi = 200)
    yr <- W[, .(spatial_minus_status = mean(spatial_minus_status), preval_minus_spatial = mean(preval_minus_spatial)), by = testYear]
    yl <- data.table::melt(yr, id.vars = "testYear", variable.name = "contrast", value.name = "difference")
    yl[, contrast := factor(contrast, c("spatial_minus_status", "preval_minus_spatial"),
                            c("Spatial-block CV minus random CV", "PreVal minus spatial-block CV"))]
    g3 <- ggplot(yl, aes(difference, factor(testYear))) + geom_vline(xintercept = 0, linetype = 2) + geom_point(size = 2.5) +
      facet_wrap(~ contrast) + theme_bw(base_size = 12) +
      labs(x = "Difference in forecast loss (negative = first is better)", y = "Test year", title = "Per-year contrasts (all covariate levels pooled)")
    ggplot2::ggsave(file.path(outDir, "figRA_per_year.png"), g3, width = 9, height = 4.5, dpi = 200)
  }
  invisible(out)
}
