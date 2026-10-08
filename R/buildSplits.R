#' Disjoint, deterministic train/validation/test splits for the three validation regimes.
#'
#' Design (per split = window start s, history end e, test year T > e):
#'  * ONE shared test set per split: a fixed random fraction (default 50%) of test-year strata.
#'    All three regimes are scored on exactly these strata (paired comparison).
#'  * FutureUnseen (PreVal): train = years s..e-1, validation = year e (all strata).
#'  * FutureTainted (status quo): the SAME strata as FutureUnseen (years s..e), randomly
#'    re-allocated to train/validation with the same sizes. Identical data, animals and
#'    bursts; only the split rule differs.
#'  * Internal (random CV over the whole window incl. the test year): train + validation drawn
#'    at random, without replacement, from years s..T minus the test strata, same sizes.
#'  * No stratum is ever in two sets (verified by verifySplits()).
#'  * Optional spatial arm: strata inside held-out spatial blocks (plus a buffer) are removed
#'    from every training/validation pool of EVERY regime; the test set is the year-T strata
#'    inside the held-out blocks.
#'
#' All sampling is stratum-level (a stratum = 1 observed step + 10 random steps, 11 rows).

#' Strata index from the prepared table (one row per stratum, in the order used by the tensors).
#' @param preparedData data.table with 11 rows per stratum, observed step first.
buildStrataIndex <- function(preparedData, steps = 11L) {
  needed <- c("indiv_step_id", "id", "idIndex", "year", "case_")
  miss <- setdiff(needed, names(preparedData))
  if (length(miss)) stop("buildStrataIndex: missing columns: ", paste(miss, collapse = ", "))
  nR <- nrow(preparedData)
  if (nR %% steps != 0) stop("buildStrataIndex: number of rows is not a multiple of ", steps)
  first <- seq.int(1L, nR, by = steps)
  last <- first + steps - 1L
  sid <- preparedData$indiv_step_id
  if (!all(sid[first] == sid[last])) stop("buildStrataIndex: strata are not stored in contiguous blocks of ", steps)
  if (!all(preparedData$case_[first] == TRUE)) stop("buildStrataIndex: observed step is not first in every stratum")
  if (sum(preparedData$case_) != length(first)) stop("buildStrataIndex: not exactly one observed step per stratum")
  extra <- intersect(c("burst_", "x1_", "y1_", "x2_", "y2_", "t1_"), names(preparedData))
  idx <- preparedData[first, c(needed[needed != "case_"], extra), with = FALSE]
  idx[, row := seq_len(.N)]
  idx[, indiv_step_id := as.character(indiv_step_id)]
  idx[, id := as.character(id)]
  idx[, burstKey := if ("burst_" %in% names(idx)) paste(id, burst_, sep = "_") else indiv_step_id]
  if (anyDuplicated(idx$indiv_step_id)) stop("buildStrataIndex: duplicated stratum ids")
  idx[]
}

#' Spatial blocks over strata (by observed-step start coordinates, projected metres).
#' @return list(blockOf = block id per stratum row, bx, by, blockFold, blockKm, bufferKm, nFolds, seed)
assignSpatialBlocks <- function(strataIdx, blockKm = 100, bufferKm = 10, nFolds = 4L, seed) {
  if (!all(c("x1_", "y1_") %in% names(strataIdx))) stop("assignSpatialBlocks needs x1_ and y1_")
  B <- blockKm * 1000
  bx <- floor(strataIdx$x1_ / B); by <- floor(strataIdx$y1_ / B)
  key <- paste(bx, by)
  blocks <- sort(unique(key))
  fold <- withSeed(seed, sample(rep_len(seq_len(nFolds), length(blocks))))
  names(fold) <- blocks
  list(key = key, bx = bx, by = by, blockFold = fold, blockKm = blockKm, bufferKm = bufferKm,
       nFolds = nFolds, seed = seed)
}

#' Which strata fall in held-out blocks (test) and which must be excluded from every
#' training/validation pool (held-out blocks + a buffer around them), for fold `foldId`.
spatialMasksXY <- function(strataIdx, spatial, foldId) {
  B <- spatial$blockKm * 1000; buf <- spatial$bufferKm * 1000
  testBlocks <- names(spatial$blockFold)[spatial$blockFold == foldId]
  inTest <- spatial$key %in% testBlocks
  excluded <- inTest
  if (buf > 0 && length(testBlocks)) {
    parts <- do.call(rbind, strsplit(testBlocks, " ", fixed = TRUE))
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
  list(inTest = inTest, excluded = excluded, testBlocks = testBlocks)
}

#' Build the three regimes' splits for one (s, e, testYear).
#' @param spatial NULL (temporal arm) or list(spatial = assignSpatialBlocks(...), foldId = k)
#' @param matchAnimals if TRUE, Internal draws only from animals present in the FutureUnseen pool
#' @return list(ok, reason, splits = list(FutureUnseen=, FutureTainted=, Internal=) each a
#'   data.table(row, set), test = rows, sizes = c(nTrain, nVal, nTest))
buildGroupSplits <- function(strataIdx, s, e, testYear, seed, testFraction = 0.5,
                             minTrain = 500L, minVal = 200L, minTest = 200L,
                             spatial = NULL, matchAnimals = FALSE) {
  stopifnot(s <= e - 1L, e < testYear)
  yr <- strataIdx$year
  eligible <- rep(TRUE, nrow(strataIdx))
  testPool <- yr == testYear
  if (!is.null(spatial)) {
    m <- spatialMasksXY(strataIdx, spatial$spatial, spatial$foldId)
    eligible <- !m$excluded
    testPool <- yr == testYear & m$inTest
    testFraction <- 1
  }
  tp <- which(testPool)
  nTest <- floor(testFraction * length(tp))
  if (nTest < minTest) return(list(ok = FALSE, reason = sprintf("test set too small (%d)", nTest)))
  test <- if (nTest == length(tp)) tp else sort(withSeed(seed + 1L, sample(tp, nTest)))

  trainYearRows <- which(eligible & yr >= s & yr <= e - 1L)
  valYearRows <- which(eligible & yr == e)
  U <- c(trainYearRows, valYearRows)
  nTrain <- length(trainYearRows); nVal <- length(valYearRows)
  if (nTrain < minTrain || nVal < minVal)
    return(list(ok = FALSE, reason = sprintf("train/val too small (%d/%d)", nTrain, nVal)))

  mk <- function(tr, va) data.table::rbindlist(list(
    data.table::data.table(row = sort(tr), set = "train"),
    data.table::data.table(row = sort(va), set = "val"),
    data.table::data.table(row = test, set = "test")))

  unseen <- mk(trainYearRows, valYearRows)

  perm <- withSeed(seed + 2L, sample(U))
  tainted <- mk(perm[(nVal + 1L):length(perm)], perm[seq_len(nVal)])

  intPool <- which(eligible & yr >= s & yr <= testYear)
  intPool <- setdiff(intPool, test)
  if (matchAnimals) intPool <- intPool[strataIdx$id[intPool] %in% unique(strataIdx$id[U])]
  if (length(intPool) < nTrain + nVal)
    return(list(ok = FALSE, reason = "Internal pool smaller than train+val"))
  draw <- withSeed(seed + 3L, sample(intPool, nTrain + nVal))
  internal <- mk(draw[(nVal + 1L):length(draw)], draw[seq_len(nVal)])

  list(ok = TRUE, reason = NA_character_, matchAnimals = matchAnimals,
       splits = list(FutureUnseen = unseen, FutureTainted = tainted, Internal = internal),
       sizes = c(nTrain = nTrain, nVal = nVal, nTest = length(test)),
       spatialExcluded = if (is.null(spatial)) NULL else which(!eligible))
}

#' Per-set composition table (strata, bursts, animals, year range) for the audit trail.
summariseSplit <- function(strataIdx, split, splitId, regime) {
  d <- merge(split, strataIdx[, .(row, id, burstKey, year)], by = "row", sort = FALSE)
  d[, .(splitId = splitId, typeValidation = regime, nStrata = .N, nBursts = uniqueN(burstKey),
        nAnimals = uniqueN(id), yearMin = min(year), yearMax = max(year)), by = set]
}
