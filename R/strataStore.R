#' Tensor store: all strata x steps x candidate features in ONE float tensor, built once per
#' process. Models select strata by row index (from the split manifests) and features by name,
#' so no per-model data.table subsetting or file re-reading is needed.
#' @param preparedData data.table, 11 rows per stratum, observed step first (see buildStrataIndex)
#' @param featureNames all candidate features (any order); tensor columns follow this order
buildStrataStore <- function(preparedData, featureNames, steps = 11L) {
  idx <- buildStrataIndex(preparedData, steps = steps)
  miss <- setdiff(featureNames, names(preparedData))
  if (length(miss)) stop("buildStrataStore: features missing from the data: ", paste(miss, collapse = ", "))
  n <- nrow(idx)
  X <- torch::torch_empty(n, steps, length(featureNames), dtype = torch::torch_float())
  for (k in seq_along(featureNames)) {
    v <- as.numeric(preparedData[[featureNames[k]]])
    if (anyNA(v)) stop("buildStrataStore: NA in feature ", featureNames[k],
                       " (impute explicitly in prepareNNdata, never silently here)")
    X[, , k] <- torch::torch_tensor(t(matrix(v, nrow = steps)), dtype = torch::torch_float())
  }
  id <- torch::torch_tensor(as.integer(idx$idIndex), dtype = torch::torch_long())
  list(index = idx, X = X, id = id, featureNames = featureNames,
       nAnimals = max(idx$idIndex), steps = steps)
}

#' Rows (strata) x selected features, as a new tensor (copy), optionally moved to a device.
storeSlice <- function(store, rows, features, device = "cpu") {
  fi <- match(features, store$featureNames)
  if (anyNA(fi)) stop("storeSlice: unknown features: ", paste(features[is.na(fi)], collapse = ", "))
  r <- torch::torch_tensor(as.integer(rows), dtype = torch::torch_long())
  x <- store$X$index_select(1, r)$index_select(3, torch::torch_tensor(fi, dtype = torch::torch_long()))
  list(x = x$to(device = device), id = store$id$index_select(1, r)$to(device = device))
}


#' Save / load the store (tensors via torch_save, index via RDS) so SLURM tasks need not re-read the table.
saveStrataStore <- function(store, dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  torch::torch_save(store$X, file.path(dir, "store_X.pt"))
  torch::torch_save(store$id, file.path(dir, "store_id.pt"))
  saveRDS(list(index = store$index, featureNames = store$featureNames, nAnimals = store$nAnimals,
               steps = store$steps), file.path(dir, "store_meta.rds"))
  invisible(dir)
}

loadStrataStore <- function(dir) {
  meta <- readRDS(file.path(dir, "store_meta.rds"))
  c(list(X = torch::torch_load(file.path(dir, "store_X.pt")), id = torch::torch_load(file.path(dir, "store_id.pt"))), meta)
}
