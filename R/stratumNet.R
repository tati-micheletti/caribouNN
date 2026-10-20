#' Shared building blocks for the stratum-ranking neural network.
#'
#' IDENTICAL copy lives in caribouNN_Global/R and caribouNN/R (a test checks this).
#' Replaces the luz-based fitting: an explicit training loop makes the learning-rate
#' scheduler's monitored quantity (validation loss), the best-epoch checkpoint, the
#' per-epoch history and the seeds fully visible and testable.
#'
#' Data layout: x = tensor [strata, steps (11), features]; id = long tensor [strata] with
#' 1-based animal indices; the observed step is always step 1 (class 1).

# ------------------------------------------------------------------------------------------
# RNG helpers
# ------------------------------------------------------------------------------------------

#' Pin R's RNG algorithms so that a seed means the same thing on every machine and worker.
pinRNG <- function() {
  suppressWarnings(RNGkind("Mersenne-Twister", "Inversion", "Rejection"))
  invisible(NULL)
}

#' Evaluate `expr` with a pinned RNG kind and `seed`, then restore the caller's RNG state.
withSeed <- function(seed, expr) {
  hadSeed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  oldSeed <- if (hadSeed) get(".Random.seed", envir = globalenv()) else NULL
  oldKind <- RNGkind()
  on.exit({
    suppressWarnings(RNGkind(oldKind[1], oldKind[2], oldKind[3]))
    if (hadSeed) assign(".Random.seed", oldSeed, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
      rm(".Random.seed", envir = globalenv())
  }, add = TRUE)
  pinRNG()
  set.seed(seed)
  force(expr)
}

#' Deterministic string -> integer seed in [1, 2147483646] (no dependency, same everywhere).
stringSeed <- function(x) {
  codes <- utf8ToInt(enc2utf8(as.character(x)))
  h <- 5381
  for (cc in codes) h <- (h * 33 + cc) %% 2147483647
  as.integer(max(1, h))
}

#' Append rows to the seed registry (one line per seed used anywhere in the pipeline).
registerSeeds <- function(registryPath, stage, name, seed, purpose) {
  if (is.null(registryPath)) return(invisible(NULL))
  rows <- data.table::data.table(stage = stage, name = name, seed = as.numeric(seed),
                                 purpose = purpose, time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  dir.create(dirname(registryPath), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(rows, registryPath, append = file.exists(registryPath))
  invisible(rows)
}

# ------------------------------------------------------------------------------------------
# Network
# ------------------------------------------------------------------------------------------

#' Same architecture as the original experiment: per-animal embedding (8-d) concatenated
#' to the step covariates -> 128 -> 64 -> 1 score per candidate step (SELU).
#' Embedding row nAnimals + 1 is reserved for animals not seen in training.
makeStratumNet <- function() {
  torch::nn_module(
    "StratumNet",
    initialize = function(nIn, nAnimals, embDim = 8L) {
      self$idEmb <- torch::nn_embedding(nAnimals + 1L, embDim)
      self$fc1 <- torch::nn_linear(nIn + embDim, 128)
      self$fc2 <- torch::nn_linear(128, 64)
      self$out <- torch::nn_linear(64, 1)
      self$act <- torch::nn_selu()
    },
    forward = function(x, id) {
      steps <- x$size(2)
      emb <- self$idEmb(id)$unsqueeze(2)$expand(c(-1, steps, -1))
      h <- self$act(self$fc1(torch::torch_cat(list(x, emb), 3)))
      h <- self$act(self$fc2(h))
      torch::torch_squeeze(self$out(h), 3)
    }
  )
}

#' Train-set standardisation statistics (mean, sd over all strata x steps, per feature).
#' sd == 0 or NA -> 1; mean NA -> 0. Computed on the TRAINING set only.
scaleStats <- function(xTrain, featureNames) {
  stats <- lapply(seq_along(featureNames), function(i) {
    v <- xTrain[, , i]$to(dtype = torch::torch_float64())
    mu <- as.numeric(v$mean()$item())
    sg <- as.numeric(v$std()$item())
    if (is.na(mu)) mu <- 0
    if (is.na(sg) || sg == 0) sg <- 1
    list(mean = mu, sd = sg)
  })
  names(stats) <- featureNames
  stats
}

#' Apply stored standardisation; clip to +/- zClip (same rule in every model/regime).
applyScale <- function(x, stats, zClip = 10) {
  mu <- torch::torch_tensor(vapply(stats, `[[`, numeric(1), "mean"), dtype = torch::torch_float())$view(c(1, 1, -1))
  sg <- torch::torch_tensor(vapply(stats, `[[`, numeric(1), "sd"), dtype = torch::torch_float())$view(c(1, 1, -1))
  xs <- (x - mu$to(device = x$device)) / sg$to(device = x$device)
  if (is.finite(zClip)) xs <- torch::torch_clamp(xs, -zClip, zClip)
  xs
}

# ------------------------------------------------------------------------------------------
# Scoring (with correct handling of animals unseen in training)
# ------------------------------------------------------------------------------------------

#' Score strata. Animals absent from the training set ("strangers") are mapped to the
#' reserved embedding row nAnimals + 1, which is set to the MEAN of the trained animals'
#' embedding rows (computed in plain R: no torch indexing conventions involved).
#' The trained rows are never modified.
scoreStrata <- function(net, x, id, trainIdsSeen, nAnimals, chunk = 4096L) {
  stopifnot(x$size(1) == id$size(1))
  spare <- nAnimals + 1L
  W <- net$idEmb$weight
  mat <- as.matrix(W$detach()$cpu())
  avg <- colMeans(mat[trainIdsSeen, , drop = FALSE])
  torch::with_no_grad(W[spare, ]$copy_(torch::torch_tensor(avg, dtype = W$dtype, device = W$device)))
  idR <- as.integer(as.array(id$cpu()))
  seen <- idR %in% trainIdsSeen
  idEff <- torch::torch_tensor(ifelse(seen, idR, spare), dtype = torch::torch_long(), device = x$device)
  wasTraining <- net$training
  net$eval()
  n <- x$size(1)
  loss <- numeric(n); rankTrue <- integer(n)
  scores <- vector("list", ceiling(n / chunk))
  k <- 0L
  torch::with_no_grad({
    for (a in seq(1L, n, by = chunk)) {
      z <- min(a + chunk - 1L, n)
      k <- k + 1L
      s <- net(x[a:z, , ], idEff[a:z])
      lp <- torch::nnf_log_softmax(s, dim = 2)
      loss[a:z] <- -as.numeric(lp[, 1]$cpu())
      rankTrue[a:z] <- as.integer(as.array((s > s[, 1]$unsqueeze(2))$sum(dim = 2)$cpu())) + 1L
      scores[[k]] <- s$cpu()
    }
  })
  if (wasTraining) net$train()
  list(loss = loss, pTrue = exp(-loss), rankTrue = rankTrue, correct = rankTrue == 1L,
       seen = seen, scores = torch::torch_cat(scores, dim = 1))
}

# ------------------------------------------------------------------------------------------
# Fitting
# ------------------------------------------------------------------------------------------

#' Fit the network. Early stopping/LR scheduling monitor the validation loss (mean over ALL
#' validation strata, no dropped batches). The best-epoch weights are kept and loaded back.
#' @param diag optional list(x, id) of strata scored after EVERY epoch and logged as `diagLoss` in the history.
#'   DIAGNOSTIC ONLY: it never enters early stopping, LR scheduling or model selection (scoring uses eval mode, no RNG,
#'   so the fit is identical with and without it).
#' @return list(net, bestState, history, bestEpoch, bestValLoss, trainIdsSeen)
fitStratumNet <- function(xTr, idTr, xVal, idVal, nAnimals, lr, epochs, batchSize = 128L,
                          seed, device = "cpu", patience = 2L, factor = 0.5, minLr = 1e-6,
                          threshold = 1e-4, earlyStopPatience = Inf, verbose = TRUE, diag = NULL) {
  torch::torch_manual_seed(seed)
  Net <- makeStratumNet()
  net <- Net(nIn = xTr$size(3), nAnimals = nAnimals)$to(device = device)
  opt <- torch::optim_adam(net$parameters, lr = lr)
  trainIdsSeen <- sort(unique(as.integer(as.array(idTr$cpu()))))
  n <- xTr$size(1)
  history <- vector("list", epochs)
  bestVal <- Inf; bestEpoch <- 0L; bestState <- NULL; bad <- 0L; curLr <- lr; sinceBest <- 0L
  for (ep in seq_len(epochs)) {
    t0 <- Sys.time()
    net$train()
    perm <- torch::torch_randperm(n, device = device) + 1L
    runLoss <- 0
    for (a in seq(1L, n, by = batchSize)) {
      z <- min(a + batchSize - 1L, n)
      ii <- perm[a:z]
      xb <- xTr[ii, , ]; ib <- idTr[ii]
      tgt <- torch::torch_ones(z - a + 1L, dtype = torch::torch_long(), device = device)
      opt$zero_grad()
      loss <- torch::nnf_cross_entropy(net(xb, ib), tgt)
      loss$backward()
      opt$step()
      runLoss <- runLoss + loss$item() * (z - a + 1L)
    }
    valLoss <- mean(scoreStrata(net, xVal, idVal, trainIdsSeen, nAnimals)$loss)
    diagLoss <- if (is.null(diag)) NA_real_ else mean(scoreStrata(net, diag$x, diag$id, trainIdsSeen, nAnimals)$loss)
    improved <- is.finite(valLoss) && valLoss < bestVal * (1 - threshold)
    if (improved) {
      bestVal <- valLoss; bestEpoch <- ep; bad <- 0L; sinceBest <- 0L
      bestState <- lapply(net$state_dict(), function(t) t$detach()$clone())
    } else {
      bad <- bad + 1L; sinceBest <- sinceBest + 1L
    }
    if (bad > patience) {
      curLr <- max(curLr * factor, minLr)
      for (g in seq_along(opt$param_groups)) opt$param_groups[[g]]$lr <- curLr
      bad <- 0L
    }
    history[[ep]] <- data.frame(epoch = ep, trainLoss = runLoss / n, valLoss = valLoss, diagLoss = diagLoss,
                                lr = curLr, isBest = improved,
                                seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")))
    if (verbose) message(sprintf("epoch %d/%d train %.4f val %.4f lr %.2e%s", ep, epochs,
                                 runLoss / n, valLoss, curLr, if (improved) " *" else ""))
    if (sinceBest >= earlyStopPatience) break
  }
  history <- do.call(rbind, history[!vapply(history, is.null, logical(1))])
  if (is.null(bestState)) stop("fitStratumNet: validation loss never became finite; no best epoch.")
  net$load_state_dict(bestState)
  list(net = net, bestState = bestState, history = history, bestEpoch = bestEpoch,
       bestValLoss = bestVal, trainIdsSeen = trainIdsSeen)
}

#' Save best weights, reload them into a FRESH network and verify that the reloaded network
#' reproduces the validation loss logged at the best epoch (guards the save/reload path).
checkWeightsRoundTrip <- function(bestState, weightsPath, nIn, nAnimals, xVal, idVal,
                                  trainIdsSeen, expectedValLoss, device = "cpu", tol = 1e-4) {
  torch::torch_save(bestState, weightsPath)
  fresh <- makeStratumNet()(nIn = nIn, nAnimals = nAnimals)$to(device = device)
  fresh$load_state_dict(torch::torch_load(weightsPath))
  got <- mean(scoreStrata(fresh, xVal, idVal, trainIdsSeen, nAnimals)$loss)
  if (!is.finite(got) || abs(got - expectedValLoss) > tol)
    stop(sprintf("Weights round-trip failed: reloaded val loss %.8f vs logged best %.8f", got, expectedValLoss))
  invisible(got)
}
