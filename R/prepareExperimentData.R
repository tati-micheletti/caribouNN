prepareExperimentData <- function(preparedDataFinal){
  message("Preparing Data...")
  
  # 1. OPTIMIZATION: Keep only necessary columns immediately
  # (Saves RAM and makes copying faster)
  cols_needed <- c("sl_", "ta_", "indiv_step_id", "id", "case_", 
                   names(preparedDataFinal)[grep("_start|_end", names(preparedDataFinal))])
  # Add specific distance columns if regex missed them
  cols_needed <- unique(c(cols_needed, "distpaved_start", "distunpaved_start", "distpolys_start", 
                          "timeSinceFire_start", "timeSinceHarvest_start"))
  cols_needed <- intersect(cols_needed, names(preparedDataFinal))
  
  dt <- preparedDataFinal[, ..cols_needed]
  
  # 2. Cleaning
  dt <- na.omit(dt, cols = c("sl_", "ta_", "indiv_step_id", "id"))
  dt[, indiv_step_id := as.factor(indiv_step_id)]
  dt[, id := as.factor(id)]
  
  # ----------------------------------------------------------------------
  # OPTIMIZATION: FILTER BURSTS *BEFORE* MATH
  # ----------------------------------------------------------------------
  
  # 1. Count Total (Before Filtering)
  originalTotalBursts <- uniqueN(dt$indiv_step_id)
  message("Filtering valid bursts from ", originalTotalBursts, " total candidates...")
  
  # 2. Determine Mode (Target Step Count)
  counts <- dt[, .N, by = indiv_step_id]
  steps <- as.numeric(names(sort(table(counts$N), decreasing=TRUE)[1]))
  
  # 3. Identify Valid Bursts
  # Valid = (Count == Mode) AND (Sum(case_) == 1)
  valid_stats <- dt[, .(n = .N, n_case = sum(case_)), by = indiv_step_id]
  valid_ids <- valid_stats[n == steps & n_case == 1, indiv_step_id]
  
  # 4. Apply Filter
  dt <- dt[indiv_step_id %in% valid_ids]
  setorder(dt, indiv_step_id, -case_) 
  
  # 5. Print Percentage
  kept_pct <- round((length(valid_ids) / originalTotalBursts) * 100, 1)
  message("Kept ", length(valid_ids), " valid bursts (", 
          kept_pct, "% of ", originalTotalBursts, 
          "). Starting Feature Engineering...")
  
  # ----------------------------------------------------------------------
  # FEATURE ENGINEERING
  # ----------------------------------------------------------------------
  
  # Vegetation Aggregates
  dt[, prop_veg_start := rowSums(.SD, na.rm = TRUE), .SDcols = intersect(names(dt), c("prop_herbs_start", "prop_shrub_start", "prop_bryoids_start"))]
  dt[, prop_veg_end := rowSums(.SD, na.rm = TRUE), .SDcols = intersect(names(dt), c("prop_herbs_end", "prop_shrub_end", "prop_bryoids_end"))]
  dt[, prop_wets_start := prop_wetland_start]
  dt[, prop_wets_end   := prop_wetland_end]
  dt[, prop_needleleaf_start := rowSums(.SD, na.rm = TRUE), .SDcols = intersect(names(dt), c("prop_needleleaf_start", "prop_wet_treed_start"))]
  dt[, prop_needleleaf_end := rowSums(.SD, na.rm = TRUE), .SDcols = intersect(names(dt), c("prop_needleleaf_end", "prop_wet_treed_end"))]
  dt[, prop_mixedforest_start := rowSums(.SD, na.rm = TRUE), .SDcols = intersect(names(dt), c("prop_mixed_start", "prop_deciduous_start"))]
  dt[, prop_mixedforest_end := rowSums(.SD, na.rm = TRUE), .SDcols = intersect(names(dt), c("prop_mixed_end", "prop_deciduous_end"))]
  
  # Basic Transforms
  dt[, logSl := log(sl_ + 1)]
  dt[, cosTa := cos(ta_)]
  dt[, sinTa := sin(ta_)]
  
  varsToLog <- c("timeSinceFire", "timeSinceHarvest", "distpaved", "distunpaved", "distpolys")
  for(v in varsToLog) {
    if(paste0(v, "_start") %in% names(dt)) dt[, (paste0(v, "_startLog")) := log(get(paste0(v, "_start")) + 1)]
    if(paste0(v, "_end") %in% names(dt))   dt[, (paste0(v, "_endLog")) := log(get(paste0(v, "_end")) + 1)]
  }
  
  # ----------------------------------------------------------------------
  # FAST INTERACTIONS (Using set() for Speed)
  # ----------------------------------------------------------------------
  interVars <- c("prop_needleleaf_start", "prop_mixedforest_start", "prop_veg_start", "prop_wets_start",
                 "timeSinceFire_startLog", "timeSinceHarvest_startLog",
                 "distpaved_startLog", "distunpaved_startLog", "distpolys_startLog")
  
  interactionCols <- c()
  
  # Pre-calculate logSl vector to avoid repetitive lookups (Speed Boost)
  vec_logSl <- dt$logSl
  
  for (var in interVars) {
    if(var %in% names(dt)) {
      newCol <- paste0("inter_logSl_x_", var)
      # set() is much faster in loops
      set(dt, j = newCol, value = vec_logSl * dt[[var]])
      interactionCols <- c(interactionCols, newCol)
    }
  }
  
  return(dt)
}