addSamplingCaps <- function(preparedDataFinal, 
                            experimentPlan, 
                            outDir, 
                            minThreshold = 500,
                            validationPerc = 0.2) {
  
  message("Balancing samples and generating data audit trail...")    
  
  # 1. Year-by-Year Inventory
  yearlyInventory <- preparedDataFinal[case_ == TRUE, .(
    countS = .N,            # Strata
    countB = uniqueN(burst_), # Bursts
    countA = uniqueN(id)      # Animals
  ), by = year]
  setkey(yearlyInventory, year)
  
  # 2. Helper function to calculate sums across a range of years row-wise
  # This fixes the "numerical expression has elements" error
  getRangeSum <- function(start, end, metricCol) {
    sum(yearlyInventory[year >= start & year <= end, get(metricCol)], na.rm = TRUE)
  }
  
  # 3. Calculate available volumes for every row (Row-wise)
  # We use mapply to force R to look at each row's start/end years individually
  message("  Slicing temporal windows per row...")
  
  # Training
  experimentPlan[, availTrainS := mapply(getRangeSum, trainStartYear, trainEndYear, 
                                         MoreArgs = list(metricCol="countS"))]
  experimentPlan[, availTrainB := mapply(getRangeSum, trainStartYear, trainEndYear, 
                                         MoreArgs = list(metricCol="countB"))]
  experimentPlan[, availTrainA := mapply(getRangeSum, trainStartYear, trainEndYear, 
                                         MoreArgs = list(metricCol="countA"))]
  
  # Validation
  experimentPlan[, availValS := mapply(getRangeSum, valStartYear, valEndYear, 
                                       MoreArgs = list(metricCol="countS"))]
  experimentPlan[, availValB := mapply(getRangeSum, valStartYear, valEndYear, 
                                       MoreArgs = list(metricCol="countB"))]
  experimentPlan[, availValA := mapply(getRangeSum, valStartYear, valEndYear, 
                                       MoreArgs = list(metricCol="countA"))]
  
  # Testing
  experimentPlan[, availTestS := mapply(getRangeSum, testStartYear, testEndYear, 
                                        MoreArgs = list(metricCol="countS"))]
  experimentPlan[, availTestB := mapply(getRangeSum, testStartYear, testEndYear, 
                                        MoreArgs = list(metricCol="countB"))]
  experimentPlan[, availTestA := mapply(getRangeSum, testStartYear, testEndYear, 
                                        MoreArgs = list(metricCol="countA"))]
  
  # 4. Calculate Group-Level Caps (Shared Bottlenecks)
  # Now we group by 'groupId' to find the lowest common denominator for the triplet
  message("  Finding shared bottlenecks across triplets...")
  
  groupCaps <- experimentPlan[, .(
    
    # --- TRAINING CAPS ---
    nTrainS = floor(min(
      availTrainS[typeValidation == "FutureUnseen"],
      availTrainS[typeValidation == "FutureTainted"] * (1 - validationPerc),
      availTrainS[typeValidation == "Internal"] * (1 - (validationPerc * 2)),
      na.rm = TRUE
    )),
    nTrainB = floor(min(
      availTrainB[typeValidation == "FutureUnseen"],
      availTrainB[typeValidation == "FutureTainted"] * (1 - validationPerc),
      availTrainB[typeValidation == "Internal"] * (1 - (validationPerc * 2)),
      na.rm = TRUE
    )),
    nTrainA = floor(min(
      availTrainA[typeValidation == "FutureUnseen"],
      availTrainA[typeValidation == "FutureTainted"] * (1 - validationPerc),
      availTrainA[typeValidation == "Internal"] * (1 - (validationPerc * 2)),
      na.rm = TRUE
    )),
    
    # --- VALIDATION CAPS ---
    nValS = floor(min(
      availValS[typeValidation == "FutureUnseen"], 
      availValS[typeValidation == "FutureTainted"] * validationPerc,
      availValS[typeValidation == "Internal"] * validationPerc, 
      na.rm = TRUE
    )),
    nValB = floor(min(
      availValB[typeValidation == "FutureUnseen"], 
      availValB[typeValidation == "FutureTainted"] * validationPerc,
      availValB[typeValidation == "Internal"] * validationPerc, 
      na.rm = TRUE
    )),
    nValA = floor(min(
      availValA[typeValidation == "FutureUnseen"], 
      availValA[typeValidation == "FutureTainted"] * validationPerc,
      availValA[typeValidation == "Internal"] * validationPerc, 
      na.rm = TRUE
    )),
    
    # --- TESTING CAPS ---
    nTestS = floor(min(
      availTestS[typeValidation == "FutureUnseen"], 
      availTestS[typeValidation == "Internal"] * validationPerc, 
      na.rm = TRUE
    )),
    nTestB = floor(min(
      availTestB[typeValidation == "FutureUnseen"], 
      availTestB[typeValidation == "Internal"] * validationPerc, 
      na.rm = TRUE
    )),
    nTestA = floor(min(
      availTestA[typeValidation == "FutureUnseen"], 
      availTestA[typeValidation == "Internal"] * validationPerc, 
      na.rm = TRUE
    ))
  ), by = groupId]
  
  # 5. Merge and cleanup temporary 'avail' columns
  # We remove the avail columns to keep the plan clean
  colsToRemove <- grep("^avail", names(experimentPlan), value = TRUE)
  experimentPlan[, (colsToRemove) := NULL]
  
  experimentPlan <- merge(experimentPlan, groupCaps, by = "groupId", all.x = TRUE)
  
  # 6. Low Data Flagging
  experimentPlan[, lowDataFlag := nTrainS < minThreshold]
  
  # Dropping zero-data groups
  total_before <- nrow(experimentPlan)
  experimentPlan <- experimentPlan[!is.na(nTrainS) & nTrainS > 0]
  dropped <- total_before - nrow(experimentPlan)
  
  if (dropped > 0) message(sprintf("Dropped %d rows with zero data.", dropped))
  
  # 7. Final Audit Table
  auditTable <- experimentPlan[typeValidation == "FutureUnseen", .(
    groupId, nTrainS, nTrainB, nTrainA, nValS, nValB, nValA, nTestS, nTestB, nTestA, lowDataFlag
  )]
  
  # Total potential pool for Internal (start to test year)
  auditTable[, totalPool := sapply(groupId, function(id){
    row <- experimentPlan[groupId == id & typeValidation == "Internal"]
    sum(yearlyInventory[year >= row$trainStartYear & year <= row$testEndYear, countS])
  })]
  auditTable[, totalUsed := nTrainS + nValS + nTestS]
  auditTable[, wastagePercent := round(((totalPool - totalUsed) / totalPool) * 100, 1)]
  
  fwrite(auditTable, file.path(outDir, "samplingAudit.csv"))
  
  message(sprintf("Success: Balanced %d groups.", uniqueN(experimentPlan$groupId)))
  return(experimentPlan)
}