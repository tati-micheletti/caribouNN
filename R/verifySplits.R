#' Hard gate against data leakage and sampling bias. Replaces the old verifySamplingMath()
#' (which only compared counts). Every check is cheap and STOPS the run on failure.
#'
#' Checks per split (the three regimes of one s, e, T):
#'   L1  no stratum appears in two sets (train/val/test) within any regime
#'   L2  the test set is identical in the three regimes and lies entirely in the test year
#'   L3  train/val/test sizes are equal across the three regimes
#'   L4  FutureUnseen: train years in [s, e-1], validation = year e only, nothing after e
#'   L5  FutureTainted: train+val years in [s, e]; the union equals the FutureUnseen union
#'   L6  Internal: train+val years in [s, T] and no test stratum among them
#'   L7  spatial arm: nothing in the excluded zone is used for training/validation, and the
#'       test set lies in the held-out blocks
#'   S1  random draws are uniform: per-year and per-animal composition of each random set
#'       is consistent with the pool it was drawn from (|z| < zMax)
#'   L8  (matchAnimals) Internal uses only animals present in the FutureUnseen pool
#'   D1  strata are 11 rows with the observed step first (checked when the index is built)
verifySplits <- function(strataIdx, bundle, s, e, testYear, splitId, spatial = NULL,
                         zMax = 5, minExpected = 20) {
  stopifnot(isTRUE(bundle$ok))
  sp <- bundle$splits
  yr <- strataIdx$year
  res <- list()
  add <- function(check, ok, detail = "") res[[length(res) + 1L]] <<- data.table::data.table(
    splitId = splitId, check = check, ok = isTRUE(ok), detail = as.character(detail))
  rowsOf <- function(m, st) m$row[m$set == st]

  for (rg in names(sp)) {
    m <- sp[[rg]]
    add(paste0("L1 disjoint sets [", rg, "]"),
        !anyDuplicated(m$row) && all(table(m$row) == 1),
        sprintf("%d rows, %d unique", nrow(m), uniqueN(m$row)))
  }
  testSets <- lapply(sp, rowsOf, st = "test")
  add("L2 test set identical across regimes",
      identical(sort(testSets[[1]]), sort(testSets[[2]])) && identical(sort(testSets[[1]]), sort(testSets[[3]])))
  add("L2 test set lies entirely in the test year", all(yr[testSets[[1]]] == testYear))
  sz <- vapply(sp, function(m) c(sum(m$set == "train"), sum(m$set == "val"), sum(m$set == "test")), numeric(3))
  add("L3 equal sizes across regimes", all(apply(sz, 1, function(v) length(unique(v)) == 1)),
      paste(apply(sz, 2, paste, collapse = "/"), collapse = " | "))

  u <- sp$FutureUnseen
  add("L4 FutureUnseen train years in [s, e-1]", all(yr[rowsOf(u, "train")] >= s & yr[rowsOf(u, "train")] <= e - 1L))
  add("L4 FutureUnseen validation = year e only", all(yr[rowsOf(u, "val")] == e))
  tn <- sp$FutureTainted
  tnUnion <- c(rowsOf(tn, "train"), rowsOf(tn, "val"))
  add("L5 FutureTainted years in [s, e]", all(yr[tnUnion] >= s & yr[tnUnion] <= e))
  add("L5 FutureTainted union == FutureUnseen union",
      identical(sort(tnUnion), sort(c(rowsOf(u, "train"), rowsOf(u, "val")))))
  it <- sp$Internal
  itUnion <- c(rowsOf(it, "train"), rowsOf(it, "val"))
  add("L6 Internal years in [s, T]", all(yr[itUnion] >= s & yr[itUnion] <= testYear))
  add("L6 Internal contains no test stratum", !any(itUnion %in% testSets[[1]]))

  if (isTRUE(bundle$matchAnimals))
    add("L8 Internal animals are a subset of the FutureUnseen/FutureTainted pool animals",
        all(strataIdx$id[itUnion] %in% strataIdx$id[c(rowsOf(u, "train"), rowsOf(u, "val"))]))

  if (!is.null(spatial)) {
    ex <- bundle$spatialExcluded
    allTV <- unique(unlist(lapply(sp, function(m) m$row[m$set != "test"])))
    add("L7 no excluded-zone stratum in train/val", !any(allTV %in% ex))
    m <- spatialMasksXY(strataIdx, spatial$spatial, spatial$foldId)
    add("L7 test strata lie in held-out blocks", all(m$inTest[testSets[[1]]]))
  }

  # Sampling-bias checks for the random draws (uniformity within the pool)
  zCheck <- function(drawRows, poolRows, key, label) {
    N <- length(poolRows); n <- length(drawRows)
    if (n >= N) return(add(label, TRUE, "draw equals pool"))
    pk <- table(key[poolRows]); dk <- table(key[drawRows])[names(pk)]
    dk[is.na(dk)] <- 0
    p <- as.numeric(pk) / N
    expd <- n * p
    sdv <- sqrt(n * p * (1 - p) * (1 - n / N))
    keep <- expd >= minExpected & sdv > 0
    z <- (as.numeric(dk) - expd)[keep] / sdv[keep]
    add(label, length(z) == 0 || max(abs(z)) < zMax,
        sprintf("max |z| = %.2f over %d categories", if (length(z)) max(abs(z)) else 0, length(z)))
  }
  U <- c(rowsOf(u, "train"), rowsOf(u, "val"))
  zCheck(rowsOf(tn, "train"), U, yr, "S1 Tainted train: year composition uniform")
  zCheck(rowsOf(tn, "train"), U, strataIdx$id, "S1 Tainted train: animal composition uniform")
  intPool <- setdiff(which(rep(TRUE, length(yr)) & yr >= s & yr <= testYear), testSets[[1]])
  if (!is.null(spatial)) intPool <- setdiff(intPool, bundle$spatialExcluded)
  if (isTRUE(bundle$matchAnimals)) intPool <- intPool[strataIdx$id[intPool] %in% unique(strataIdx$id[U])]
  zCheck(itUnion, intPool, yr, "S1 Internal draw: year composition uniform")
  zCheck(itUnion, intPool, strataIdx$id, "S1 Internal draw: animal composition uniform")

  out <- data.table::rbindlist(res)
  if (any(!out$ok)) {
    stop("verifySplits FAILED for ", splitId, ":\n",
         paste(utils::capture.output(print(out[ok == FALSE])), collapse = "\n"))
  }
  out
}

#' Re-check a single model's manifest right before training (cheap guard against stale or
#' hand-edited manifests and against index mix-ups).
verifyModelManifest <- function(strataIdx, manifest, regime, s, e, testYear, expected) {
  yr <- strataIdx$year
  tr <- manifest$row[manifest$set == "train"]; va <- manifest$row[manifest$set == "val"]
  te <- manifest$row[manifest$set == "test"]
  stopifnot(!anyDuplicated(manifest$row))
  if (length(tr) != expected[["nTrain"]] || length(va) != expected[["nVal"]] || length(te) != expected[["nTest"]])
    stop("Manifest sizes differ from the plan for ", regime)
  if (!all(yr[te] == testYear)) stop("Test strata outside the test year")
  if (regime == "FutureUnseen") {
    if (!(all(yr[tr] >= s & yr[tr] <= e - 1L) && all(yr[va] == e))) stop("FutureUnseen year rules violated")
  } else if (regime == "FutureTainted") {
    if (!all(yr[c(tr, va)] >= s & yr[c(tr, va)] <= e)) stop("FutureTainted year rules violated")
  } else if (regime == "Internal") {
    if (!all(yr[c(tr, va)] >= s & yr[c(tr, va)] <= testYear)) stop("Internal year rules violated")
  } else stop("Unknown regime: ", regime)
  invisible(TRUE)
}
