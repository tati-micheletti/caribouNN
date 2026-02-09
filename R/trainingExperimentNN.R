trainingExperimentNN <- function(subsettedData, # Path
                                 neededYears,
                                 modelNaming,
                                 batchSize, 
                                 learningRate, 
                                 epochs, 
                                 experimentPlanRow,
                                 featurePriority, 
                                 weightsPath,
                                 modelPath){
  
  # ----------------------------------------------------------------------
  # 1. SETUP, BUCKETING, & HONEST SCALING
  # ----------------------------------------------------------------------

  availableKeys <- intersect(paste0("year_", neededYears), names(subsettedData))
  dt <- rbindlist(lapply(subsettedData[availableKeys], fread))
  
  # SEEDS
  groupSeed <- sum(utf8ToInt(experimentPlanRow$groupId))
  set.seed(groupSeed)
  torch_manual_seed(groupSeed)  
  
  dTrain <- sampleBalanced(pool = dt[year >= experimentPlanRow$trainStartYear & 
                                       year <= experimentPlanRow$trainEndYear],
                           nS = experimentPlanRow$nTrainS, 
                           seed = groupSeed)
  dVal   <- sampleBalanced(pool = dt[year >= experimentPlanRow$valStartYear & 
                                       year <= experimentPlanRow$valEndYear],   
                           nS = experimentPlanRow$nValS, 
                           seed = groupSeed + 1)
  dTest  <- sampleBalanced(pool = dt[year >= experimentPlanRow$testStartYear & 
                                       year <= experimentPlanRow$testEndYear],  
                           nS = experimentPlanRow$nTestS, 
                           seed = groupSeed + 2)
  
  # Assessing model complexity
  nPick <- if(is.infinite(experimentPlanRow$numberOfCovariates)) nrow(featurePriority) else 
    experimentPlanRow$numberOfCovariates
  featureCandidates <- featurePriority$Feature[1:min(nPick, nrow(featurePriority))]
  
  scalingStats <- list()
  for (f in featureCandidates) {
    mu <- mean(dTrain[[f]], na.rm = TRUE)
    sigma <- sd(dTrain[[f]], na.rm = TRUE)
    if (is.na(sigma) || sigma == 0) sigma <- 1
    scalingStats[[f]] <- list(mean = mu, sd = sigma)
    set(dTrain, j = f, value = (dTrain[[f]] - mu) / sigma)
    set(dVal,   j = f, value = (dVal[[f]]   - mu) / sigma)
    set(dTest,  j = f, value = (dTest[[f]]  - mu) / sigma)
  }
  
  # Save scaling stats
  modelScalePath <- file.path(dirname(modelPath), paste0(modelNaming,"_scl.rds"))
  message("Saving scaling statistics...")
  saveRDS(scalingStats, modelScalePath)
  
  # ----------------------------------------------------------------------
  # 2. DATASET, MODEL, & FIT (UPDATED TO MATCH GLOBAL)
  # ----------------------------------------------------------------------
  
  # A. Correct Tensor Shaping (Bypassing the R indexing bug by stripping attributes)
  to_tensor <- function(dt_sub, feat_list) {
    if(nrow(dt_sub) == 0) return(NULL)
    # The Global Model worked because the matrix was 'clean'. 
    # We force a fresh numeric matrix here to break the link to the master table.
    m_clean <- matrix(as.numeric(as.matrix(dt_sub[, ..feat_list])), nrow = nrow(dt_sub))
    nB <- nrow(m_clean) / 11
    arr <- array(m_clean, dim = c(11, nB, length(feat_list)))
    list(x = torch_tensor(aperm(arr, c(2, 1, 3)), dtype = torch_float()), 
         id = torch_tensor(as.integer(dt_sub[case_ == TRUE, idIndex]), dtype = torch_long()))
  }
  
  tTrain <- to_tensor(dTrain, featureCandidates)
  tVal   <- to_tensor(dVal,   featureCandidates)
  tTest  <- to_tensor(dTest,  featureCandidates)
  
  # B. Dataset Definition (Matching Global EXACTLY)
  ds <- dataset("ds", 
                initialize = function(x, id) {
                  self$x <- x
                  self$id <- id
                },
                .getitem = function(i) { 
                  list(
                    list(x=self$x[i,,], id=self$id[i]), 
                    torch_tensor(1, dtype=torch_long()) 
                  ) 
                },
                .length = function() { 
                  self$x$size(1) 
                }
  )
  
  # C. Model Definition (Matching Global EXACTLY)
  Net <- nn_module(
    "Net",
    initialize = function(nIn, nAnimals) {
      self$idEmb <- nn_embedding(nAnimals + 1, 8)
      self$fc1 <- nn_linear(nIn + 8, 128)
      self$fc2 <- nn_linear(128, 64)
      self$out <- nn_linear(64, 1)
      self$act <- nn_selu()
    },
    forward = function(input) {
      x <- input$x; id <- input$id
      emb <- self$idEmb(id)$unsqueeze(2)$expand(c(-1,x$shape[2],-1))
      torch_cat(list(x,emb),3) %>% 
        self$fc1() %>% self$act() %>% 
        self$fc2() %>% self$act() %>% 
        self$out() %>% torch_squeeze(3)
    }
  )
  
  # D. Fit with Loss Wrapper (Matching Global EXACTLY)
  loss_wrapper <- function(input, target) {
    nnf_cross_entropy(input, target$squeeze())
  }
  
  message("Fitting the model...")
  fitted <- Net %>%
    setup(loss = loss_wrapper, optimizer = optim_adam) %>%
    set_hparams(nIn = length(featureCandidates), nAnimals = 200) %>%
    set_opt_hparams(lr = learningRate) %>%
    fit(dataloader(ds(tTrain$x, tTrain$id), batch_size = batchSize, shuffle = TRUE),
        epochs = epochs, 
        valid_data = dataloader(ds(tVal$x, tVal$id), batch_size = batchSize),
        callbacks = list(luz_callback_model_checkpoint(path = weightsPath, save_best_only = TRUE),
                         luz_callback_lr_scheduler(lr_reduce_on_plateau, factor = 0.5, patience = 2)))
  
  message(paste0("Model fitting finished for ", modelNaming))
  
  # ----------------------------------------------------------------------
  # 3. RELOAD & STRANGER HANDLING (Matching Global EXACTLY)
  # ----------------------------------------------------------------------
  if (file.exists(weightsPath)) {
    message("Reloading weights from best epoch: ", weightsPath)
    tryCatch({
      checkpoint <- torch_load(weightsPath)
      weights_to_load <- if (is.list(checkpoint) && "model" %in% names(checkpoint)) checkpoint$model else 
        if (is.list(checkpoint) && "model_state_dict" %in% names(checkpoint)) checkpoint$model_state_dict else checkpoint
      fitted$model$load_state_dict(weights_to_load)
    }, error = function(e) message("!!! WARNING: Could not reload weights. Using Last Epoch."))
  }
  
  # Stranger handling
  ids_train <- unique(as.array(tTrain$id$cpu()))
  ids_test <- unique(as.array(tTest$id$cpu()))
  new_ids <- setdiff(ids_test, ids_train)
  if (length(new_ids) > 0) {
    w <- fitted$model$idEmb$weight
    avg_p <- w[ids_train, ]$mean(dim = 1)
    with_no_grad({ for (idx in new_ids) w[idx, ]$copy_(avg_p) })
  }
  
  # ----------------------------------------------------------------------
  # 4. EVALUATION
  # ----------------------------------------------------------------------
  message(paste0("Starting model evaluation for ", modelNaming))
  fitted$model$eval()
  
  # Use the same Dataset/Dataloader logic as training
  test_dl <- dataloader(ds(tTest$x, tTest$id), batch_size = batchSize)
  
  all_individual_losses <- c()
  correct_preds <- 0
  total_samples <- 0
  
  with_no_grad({
    coro::loop(for (b in test_dl) {
      scores <- fitted$model(b[[1]])
      loss_vector <- nnf_cross_entropy(scores, b[[2]]$squeeze(), reduction = "none")
      all_individual_losses <- c(all_individual_losses, as.numeric(loss_vector$cpu()))
      preds <- torch_argmax(scores, dim = 2) + 1L
      correct_preds <- correct_preds + as.numeric((preds == 1L)$sum()$cpu())
      total_samples <- total_samples + scores$size(1)
    })
  })
  
  # ----------------------------------------------------------------------
  # 5. PERSISTENCE
  # ----------------------------------------------------------------------
  luz_save(fitted, modelPath)
  rawLossPath <- file.path(dirname(modelPath), paste0(modelNaming,"_rawLosses.rds"))
  saveRDS(all_individual_losses, rawLossPath)
  
  return(data.table(
    groupId = experimentPlanRow$groupId,
    typeValidation = experimentPlanRow$typeValidation,
    modelName = modelNaming,
    modelPath = modelPath,
    modelWeight = weightsPath,
    modelScalePath = modelScalePath,
    rawLossPath = rawLossPath,
    testLossMean = mean(all_individual_losses),
    testLossSD = sd(all_individual_losses),
    totalSamples = total_samples,
    correctPreds = correct_preds,
    testAccuracy = correct_preds / total_samples
  ))
}