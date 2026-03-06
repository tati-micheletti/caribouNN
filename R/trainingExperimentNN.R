trainingExperimentNN <- function(subsettedData, # Path
                                 neededYears,
                                 modelNaming,
                                 batchSize, 
                                 learningRate, 
                                 epochs, 
                                 experimentPlanRow,
                                 featurePriority, 
                                 weightsPath,
                                 modelPath, 
                                 outputDir, 
                                 reRunExperiment,
                                 device = "cpu"){

  create_error_result <- function(experimentPlanRow, modelNaming, error_message) {
    data.table(
      groupId = experimentPlanRow$groupId,
      typeValidation = experimentPlanRow$typeValidation,
      modelName = modelNaming,
      groupSeed = NA_integer_,
      numberOfCovariates = experimentPlanRow$numberOfCovariates,
      trainStartYear = experimentPlanRow$trainStartYear,
      trainEndYear = experimentPlanRow$trainEndYear,
      testStartYear = experimentPlanRow$testStartYear,
      nTrain = experimentPlanRow$nTrainS,
      nTest = experimentPlanRow$nTestS,
      nVal = experimentPlanRow$nValS,
      testLossMean = NA_real_,
      testLossSD = NA_real_,
      totalSamples = 0,
      correctPreds = 0,
      testAccuracy = NA_real_,
      modelPath = NA_character_,
      modelWeight = NA_character_,
      modelScalePath = NA_character_,
      rawLossPath = NA_character_,
      errorMessage = error_message
    )
  }
  tryCatch({
  finalDTPath <- file.path(outputDir, paste0(modelNaming, "_finalDT.csv"))
  if (any(reRunExperiment,
          !file.exists(finalDTPath))) {
    if (reRunExperiment){
      message("reRunModels is set to TRUE. Rerunning models...")
    } else {
      message(paste0(modelNaming, " doesn't exist. Fitting..."))
    }
  # ----------------------------------------------------------------------
  # 1. SETUP, BUCKETING, & HONEST SCALING
  # ----------------------------------------------------------------------

  availableKeys <- intersect(paste0("year_", neededYears), names(subsettedData))
  dt <- tryCatch({
    rbindlist(lapply(subsettedData[availableKeys], fread))
  }, error = function(e) {
    warning(paste0("DATA LOAD FAILED for ", modelNaming, ": ", e$message))
    return(NULL)  # Return NULL to trigger validation below
  })
  
  # SEEDS
  groupSeed <- sum(utf8ToInt(experimentPlanRow$groupId))
  set.seed(groupSeed)
  torch_manual_seed(groupSeed)  
  
  dTrain <- tryCatch({
    sampleBalanced(pool = dt[year >= experimentPlanRow$trainStartYear & 
                               year <= experimentPlanRow$trainEndYear],
                   nS = experimentPlanRow$nTrainS, 
                   seed = groupSeed)
  }, error = function(e) {
    warning(paste0("TRAIN SAMPLING FAILED: ", e$message))
    return(NULL)
  })
  
  dVal <- tryCatch({
    sampleBalanced(pool = dt[year >= experimentPlanRow$valStartYear & 
                               year <= experimentPlanRow$valEndYear],   
                   nS = experimentPlanRow$nValS, 
                   seed = groupSeed + 1)
  }, error = function(e) {
    warning(paste0("VAL SAMPLING FAILED: ", e$message))
    return(NULL)
  })
  
  dTest <- tryCatch({
    sampleBalanced(pool = dt[year >= experimentPlanRow$testStartYear & 
                               year <= experimentPlanRow$testEndYear],  
                   nS = experimentPlanRow$nTestS, 
                   seed = groupSeed + 2)
  }, error = function(e) {
    warning(paste0("TEST SAMPLING FAILED: ", e$message))
    return(NULL)
  })
  
  #====================== CHECK
  # We check if each bucket has exactly the number of rows required by the budget.
  # Every stratum must have 11 steps.
  expectedTrainRows <- experimentPlanRow$nTrainS * 11
  expectedValRows   <- experimentPlanRow$nValS * 11
  expectedTestRows  <- experimentPlanRow$nTestS * 11
  
  # Logic: If any bucket is empty or insufficient, return NA instead of crashing
  if (any(is.null(dTrain), is.null(dVal), is.null(dTest),
          nrow(dTrain) < expectedTrainRows,
          nrow(dVal)   < expectedValRows,
          nrow(dTest)  < expectedTestRows)) {
    
    message(paste0("!!! INSUFFICIENT DATA for ", modelNaming, 
                   ". Expected T/V/T: ", expectedTrainRows, "/", expectedValRows, "/", expectedTestRows,
                   " but got: ", nrow(dTrain), "/", nrow(dVal), "/", nrow(dTest)))
    
    # Return a result row with NAs so rbindlist doesn't fail
    return(data.table(
      groupId = experimentPlanRow$groupId,
      typeValidation = experimentPlanRow$typeValidation,
      modelName = modelNaming,
      testLossMean = NA,
      testAccuracy = NA,
      totalSamples = 0,
      errorMessage = "Insufficient data after filtering/sampling"
    ))
  }
  
  # Ensure the ID Index is present and valid
  if (!"idIndex" %in% names(dTrain)) {
    message("!!! Error: idIndex missing in sampled data for ", modelNaming)
    return(data.table(groupId = experimentPlanRow$groupId, testLossMean = NA, errorMessage = "idIndex missing"))
  }
  #====================== CHECK

  # Assessing model complexity
  nPick <- if(is.infinite(experimentPlanRow$numberOfCovariates)) nrow(featurePriority) else 
    experimentPlanRow$numberOfCovariates
  featureCandidates <- featurePriority$Feature[1:min(nPick, nrow(featurePriority))]
  
  scalingStats <- list()
  scaling_result <- tryCatch({
    for (f in featureCandidates) {
      # Check if feature exists
      if (!f %in% names(dTrain)) {
        stop(paste0("Feature ", f, " not found in data"))
      }
      
      mu <- mean(dTrain[[f]], na.rm = TRUE)
      sigma <- sd(dTrain[[f]], na.rm = TRUE)
      
      # Handle invalid scaling parameters
      if (is.na(mu)) mu <- 0
      if (is.na(sigma) || sigma == 0) sigma <- 1
      
      scalingStats[[f]] <- list(mean = mu, sd = sigma)
      
      # Scale the data
      set(dTrain, j = f, value = (dTrain[[f]] - mu) / sigma)
      set(dVal,   j = f, value = (dVal[[f]]   - mu) / sigma)
      set(dTest,  j = f, value = (dTest[[f]]  - mu) / sigma)
    }
    TRUE  # Success
  }, error = function(e) {
    warning(paste0("SCALING FAILED for ", modelNaming, ": ", e$message))
    return(FALSE)
  })
  
  if (!scaling_result) {
    return(create_error_result(experimentPlanRow, modelNaming, "Scaling failed"))
  }

  # Save scaling stats
  modelScalePath <- file.path(dirname(modelPath), paste0(modelNaming,"_scl.rds"))
  message("Saving scaling statistics...")
  saveRDS(scalingStats, modelScalePath)
  
  # ----------------------------------------------------------------------
  # 2. DATASET, MODEL, & FIT (UPDATED TO MATCH GLOBAL)
  # ----------------------------------------------------------------------
  
  # FIX 1: to_tensor builds array column-by-column to avoid creating a single
  # giant matrix of nrow*ncols elements that can exceed R's 2^31 integer limit.
  to_tensor <- function(dt_sub, feat_list) {
    tryCatch({
      if(nrow(dt_sub) == 0) return(NULL)
      
      nR <- nrow(dt_sub)
if (nR %% 11 != 0)
  stop("Rows not divisible by 11 — corrupted strata")
      nB <- nR / 11
      nF <- length(feat_list)
      
      if (nB < 1) {
        warning(paste0("Insufficient batches: ", nB))
        return(NULL)
      }
      
      # Build 3D array feature-by-feature instead of one giant matrix all at once
      arr <- array(0, dim = c(11, nB, nF))
      for (fi in seq_along(feat_list)) {
        arr[,,fi] <- as.numeric(dt_sub[[feat_list[fi]]])
      }
      
      list(
        x = torch_tensor(aperm(arr, c(2, 1, 3)), dtype = torch_float()), 
        id = torch_tensor(as.integer(dt_sub[case_ == TRUE, idIndex]), 
                          dtype = torch_long())
      )
    }, error = function(e) {
      warning(paste0("Tensor creation failed: ", e$message))
      return(NULL)
    })
  }

  tTrain <- to_tensor(dTrain, featureCandidates)
  tVal   <- to_tensor(dVal,   featureCandidates)
  tTest  <- to_tensor(dTest,  featureCandidates)

    if (device == "cuda") {
  tTrain <- list(x = tTrain$x$to(device = device), id = tTrain$id$to(device = device))
  tVal   <- list(x = tVal$x$to(device = device),   id = tVal$id$to(device = device))
  tTest  <- list(x = tTest$x$to(device = device),  id = tTest$id$to(device = device))
}
  
  if (any(is.null(tTrain), is.null(tVal), is.null(tTest))) {
    return(create_error_result(experimentPlanRow, modelNaming, "Tensor creation failed"))
  }
  
  if (tTrain$x$size(1) == 0 || tVal$x$size(1) == 0 || tTest$x$size(1) == 0) {
    msg <- sprintf("Empty tensors - Train: %d, Val: %d, Test: %d",
                   tTrain$x$size(1), tVal$x$size(1), tTest$x$size(1))
    return(create_error_result(experimentPlanRow, modelNaming, msg))
  }
  
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
  
  if (tTrain$x$size(1) == 0 || tVal$x$size(1) == 0 || tTest$x$size(1) == 0) {
    print(paste0("Empty tensor batches for: ", modelNaming))
    return(create_error_result(experimentPlanRow,
  modelNaming, "Empty tensor batches"))
  }
  
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
      x <- input$x
      id <- input$id
      
      # Use size() and view() for absolute stability
      batch_sz <- x$size(1)
      steps_n  <- x$size(2)
      
      # Force ID to be a flat list, then force the embedding to be 3D [Batch, 1, 8]
      emb <- self$idEmb(id$view(-1))$view(c(batch_sz, 1, -1))
      emb <- emb$expand(c(-1, steps_n, -1))
      
      torch_cat(list(x, emb), 3) %>% 
        self$fc1() %>% self$act() %>% 
        self$fc2() %>% self$act() %>% 
        self$out() %>% torch_squeeze(3)
    }
  )
  
  # D. Fit with Loss Wrapper (Matching Global)
  if (device == "cuda"){
    calc_loss_1indexed <- nn_cross_entropy_loss()
  } else {
       calc_loss_1indexed <- nn_cross_entropy_loss()$to(device = device) 
  }
  
  loss_wrapper <- function(input, target) {
    calc_loss_1indexed(input, target$squeeze())
  }
  
  message("Fitting the model...") 
  fitted <- tryCatch({
    Net %>%
      setup(loss = loss_wrapper, optimizer = optim_adam) %>%
      set_hparams(nIn = length(featureCandidates), nAnimals = 200) %>% 
      set_opt_hparams(lr = learningRate) %>%
      fit(dataloader(ds(tTrain$x, tTrain$id), batch_size = batchSize, shuffle = TRUE),
          epochs = epochs, 
          valid_data = dataloader(ds(tVal$x, tVal$id), batch_size = batchSize),
          callbacks = list(
            luz_callback_model_checkpoint(path = weightsPath, save_best_only = TRUE),
            luz_callback_lr_scheduler(lr_reduce_on_plateau, factor = 0.5, patience = 2)
          ))
  }, error = function(e) {
    warning(paste0("MODEL TRAINING FAILED for ", modelNaming, ": ", e$message))
    return(NULL)
  })
  
  if (is.null(fitted)) {
    return(create_error_result(experimentPlanRow, modelNaming, 
                               "Model training crashed"))
  }
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
    tryCatch({
      w <- fitted$model$idEmb$weight
      avg_p <- w[ids_train, ]$mean(dim = 0)
      with_no_grad({ 
        for (idx in new_ids) {
          w[idx, ]$copy_(avg_p)
        }
      })
    }, error = function(e) {
      warning(paste0("Stranger ID handling failed: ", e$message))
      # Non-fatal - model will just use random embeddings for new IDs
    })
  }
  # ----------------------------------------------------------------------
  # 4. EVALUATION
  # ----------------------------------------------------------------------
  message(paste0("Starting model evaluation for ", modelNaming))

  # FIX 2: dataloader construction wrapped in tryCatch with smaller batch size fallback
  test_dl <- tryCatch({
    dataloader(ds(tTest$x, tTest$id), batch_size = batchSize)
  }, error = function(e) {
    warning(paste0("Dataloader creation failed, trying smaller batch: ", e$message))
    dataloader(ds(tTest$x, tTest$id), batch_size = max(1L, batchSize %/% 4L))
  })
  
  # FIX 3: Use list accumulation instead of c() to avoid long vector copies
  all_batch_losses <- list()
  batch_idx <- 0L
  correct_preds <- 0
  total_samples <- 0
  
  print("STARTING EVAL LOOP...")
  eval_success <- tryCatch({
    with_no_grad({
      coro::loop(for (b in test_dl) {
        scores <- fitted$model(b[[1]])
        
        if (device == "cuda"){
                target_1indexed <- torch_ones(scores$size(1), dtype = torch_long())$to(device = device)  
        } else {
                 target_1indexed <- torch_ones(scores$size(1), dtype = torch_long()) 
        }
        
        loss_val <- calc_loss_1indexed(scores, target_1indexed)
        
        batch_idx <- batch_idx + 1L
        # all_batch_losses[[batch_idx]] <- as.numeric(loss_val$cpu()) # New likely causes long vector problem.
        all_batch_losses[[batch_idx]] <- loss_val$item() 

        # preds <- torch_argmax(scores, dim = 2)
        # correct_preds <- correct_preds + as.numeric((preds == 1L)$sum()$cpu()) # Likely causes long vector problem. [UPDATE] Not really, error still happening.
        correct_preds <- correct_preds + 
          as.numeric(torch_sum(torch_eq(torch_argmax(scores, dim = 2), 1L))$item())
        total_samples <- total_samples + scores$size(1)
      })
    })
    TRUE  # Success flag
  }, error = function(e) {
    warning(paste0("EVALUATION FAILED for ", modelNaming, ": ", e$message))
    return(FALSE)
  })
  
  print(paste0("EVALUATION SUCCESSFUL for ", modelNaming))
  
  if (!eval_success) {
    return(create_error_result(experimentPlanRow, modelNaming, 
                               "Evaluation crashed"))
  }
  
  all_individual_losses <- unlist(all_batch_losses)  # Single allocation at end
  
  # print(paste0("UNLISTING SUCCESSFUL for ", modelNaming))
  
  correPredPath <- file.path(dirname(modelPath), paste0(modelNaming,"_correctPreds.rds"))
  saveRDS(correct_preds, correPredPath)
  totSampPath <- file.path(dirname(modelPath), paste0(modelNaming,"_totalSamps.rds"))
  saveRDS(total_samples, totSampPath)
  
  # print(paste0("SAVING SUCCESSFUL for ", modelNaming))
  
  # ----------------------------------------------------------------------
  # 5. PERSISTENCE
  # ----------------------------------------------------------------------
  tryCatch({
    luz_save(fitted, modelPath)
  }, error = function(e){
    warning(paste0("Saving of full model object using luz failed. Resorting to saving with torch."), immediate. = TRUE)
    torch_save(fitted$model$state_dict(), modelPath)
    # torch_save(fitted$optim$state_dict(),
    #            file.path(outputDir, paste0(modelNaming, "_optim.pt"))) # Fails, and not necessary
    saveRDS(list(
      nIn = length(featureCandidates),
      nAnimals = 200,
      learningRate = learningRate,
      batchSize = batchSize,
      epochs = epochs
    ),
    file.path(outputDir, paste0(modelNaming, "_config.rds")))
    print(paste0("ALTERNATIVE SAVING SUCCESSFUL for ", modelNaming))
  })
  
  rawLossPath <- file.path(dirname(modelPath), paste0(modelNaming,"_rawLosses.rds"))
  # print(paste0("LOADING RAW LOSSES SUCCESSFUL for ", modelNaming))
  
  saveRDS(all_individual_losses, rawLossPath)
  print(paste0("SAVING RAW LOSSES SUCCESSFUL for ", modelNaming))
  
  # Saving the final results
  finalDT <- data.table(
    groupId = experimentPlanRow$groupId,
    typeValidation = experimentPlanRow$typeValidation,
    modelName = modelNaming,
    groupSeed = groupSeed,
    
    # Metadata for plotting
    numberOfCovariates = experimentPlanRow$numberOfCovariates,
    trainStartYear = experimentPlanRow$trainStartYear,
    trainEndYear = experimentPlanRow$trainEndYear,
    testStartYear = experimentPlanRow$testStartYear,
    nTrain = experimentPlanRow$nTrainS,
    nTest = experimentPlanRow$nTestS,
    nVal = experimentPlanRow$nValS,
    
    # Performance Metrics
    testLossMean = mean(all_individual_losses),
    testLossSD = sd(all_individual_losses),
    totalSamples = total_samples,
    correctPreds = correct_preds,
    testAccuracy = correct_preds / total_samples,
    
    # Paths for Auditing
    modelPath = modelPath,
    modelWeight = weightsPath,
    modelScalePath = modelScalePath,
    rawLossPath = rawLossPath
  )
  print(paste0("CREATING FINAL TABLE SUCCESSFUL for ", modelNaming))
  
  write.csv(finalDT, finalDTPath)
  print(paste0("WRITING FINAL TABLE SUCCESSFUL for ", modelNaming))
  
  
  } else {
    message(paste0("Model ", modelNaming," exists and reRunModels = FALSE. Loading results..."))
    finalDT <- fread(finalDTPath)
}
  return(finalDT)
  }, error = function(e) {
    error_msg <- paste0("FATAL UNHANDLED ERROR in ", modelNaming, ": ", e$message)
    message(error_msg)
    traceback()  # Print stack trace for debugging
    return(create_error_result(experimentPlanRow, modelNaming, error_msg))
  })
}
