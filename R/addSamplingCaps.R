addSamplingCaps <- function(preparedDataFinal, experimentPlan) {

  message("Calculating sampling caps based on FutureUnseen bottlenecks...")
  
  # 1. Create a Year-by-Year Inventory of available bursts
  # We only count unique bursts (where case_ == TRUE)
  # This is the "Master Bank" we draw from
  yearlyInventory <- preparedDataFinal[case_ == TRUE, .N, by = year]
  setkey(yearlyInventory, year)
  
  # 2. Identify the groups
  uniqueGroups <- unique(experimentPlan$groupId)
  
  # 3. Calculate N for each group
  # We use the FutureUnseen row as the anchor because it has the 
  # most restrictive training window.
  groupCaps <- experimentPlan[typeValidation == "FutureUnseen", .(
    maxTrainingBursts = sum(yearlyInventory[year %in% (trainStartYear:trainEndYear), N], na.rm = TRUE)
  ), by = groupId]
  
  # 4. Merge the caps back into the full Experiment Plan
  # This adds the 'maxTrainingBursts' column to every row in the group
  experimentPlan <- merge(experimentPlan, groupCaps, by = "groupId", all.x = TRUE)
  
  # 5. Data Integrity Check
  # If a group has no Unseen row (e.g., window size was too small), 
  # it won't have a cap. We should flag or remove these.
  missingCaps <- nrow(experimentPlan[is.na(maxTrainingBursts)])
  if (missingCaps > 0) {
    message(sprintf("Note: %d rows did not form a complete triplet and should be checked.", missingCaps))
  }
  
  message("Sampling caps added successfully.")
  return(experimentPlan)
}