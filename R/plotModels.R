plotModels <- function(fittedTable = NULL, 
                       DT_Master = NULL,  
                       extractedFeatures = NULL, 
                       outPath){
  if (!is.null(DT_Master)){
    print(paste0("DT master provided!"))
  } else {
    if (is.null(fittedTable)) stop(paste0("If DT_Master is not provided,",
                                          " fittedTable needs to be supplied."))
    # I decided to remove any 2007/2008 models because of the 
    # assumption of 2007 being exactly like 2008. I don't think
    # I can live with that 
    fittedTable <- fittedTable[trainStartYear != 2007]
    # Fixing to the correct value
    fittedTable[numberOfCovariates == Inf,  numberOfCovariates := 30]
    # Add Training Window Length (Inclusive counting: e.g., 2009-2011 is 3 years)
    fittedTable[, trainingWindowLength := (trainEndYear - trainStartYear + 1)]
    
    # # Fixing wrong naming convention
    # NOT USED BECAUSE I AM NOT USING THE 2007/2008 VALUES
    # fittedTable[numberOfCovariates == Inf, numberOfCovariates := 30]
    # fittedTable[trainStartYear == 2007, trainStartYear := 2008]
    # fittedTable[trainEndYear == 2007, trainEndYear := 2008]
    # fittedTable[testStartYear == 2007, testStartYear := 2008]
    
    # Completing the table with Validation and Test for completeness
    # valStartYear: 
    #   FutureUnseen: trainEndYear+1
    #   FutureTainted: trainStartYear
    #   Internal: trainStartYear
    # valEndYear
    #   FutureUnseen: trainEndYear+1
    #   FutureTainted: trainEndYear
    #   Internal: trainEndYear
    # testEndYear
    #   FutureUnseen: testStartYear
    #   FutureTainted: testStartYear
    #   Internal: trainEndYear
    
    fittedTable[typeValidation == "FutureUnseen", valStartYear := trainEndYear+1]
    fittedTable[typeValidation == "FutureUnseen", valEndYear := trainEndYear+1]
    fittedTable[typeValidation == "FutureUnseen", testEndYear := testStartYear]
    
    fittedTable[typeValidation == "FutureTainted", valStartYear := trainStartYear]
    fittedTable[typeValidation == "FutureTainted", valEndYear := trainEndYear]
    fittedTable[typeValidation == "FutureTainted", testEndYear := testStartYear]
    
    fittedTable[typeValidation == "Internal", valStartYear := trainStartYear]
    fittedTable[typeValidation == "Internal", valEndYear := trainEndYear]
    fittedTable[typeValidation == "Internal", testEndYear := trainEndYear]
    
    # Adding forecast horizon (NOTE: Even though internal is technically 0, we want to see the 
    # performance for comparison to the others, so we use the same of the group)
    fittedTable[, forecastHorizon := {
      ref_vals <- testStartYear[typeValidation %in% c("FutureUnseen", "FutureTainted")] -
        valEndYear[typeValidation %in% c("FutureUnseen", "FutureTainted")]
      if (length(unique(ref_vals)) != 1) {
        stop("Mismatch in forecast horizon within group: ", groupId[1])
      }
      ref_vals[1]
    }, by = groupId]
    
    # Fixing file names for models ran on the GPU
    fittedTable[, rawLossPath := sub("^/export", "", rawLossPath)]
    
    print("Loading all individual losses into RAM...")
    
    print(paste0("DT master not found, creating... It might take some time."))
    DT_Master <- rbindlist(lapply(1:nrow(fittedTable), function(i){
      dt <- data.table(
        groupId            = fittedTable[i, groupId],
        Complexity         = factor(fittedTable[i, numberOfCovariates], levels=c(2,5,10,30)),
        scenario           = factor(fittedTable[i, typeValidation], 
                                    levels=c("Internal", "FutureTainted", "FutureUnseen")),
        forecastHorizon    = fittedTable[i, forecastHorizon],
        trainingWindow     = fittedTable[i, trainingWindowLength],
        trainStartYear     = fittedTable[i, trainStartYear],
        trainEndYear       = fittedTable[i, trainEndYear],
        testStartYear      = fittedTable[i, testStartYear],
        loss               = readRDS(fittedTable[i, rawLossPath]) # Loading individual losses
      )
      return(dt)
    }), use.names = TRUE)
    fwrite(DT_Master, DTmaster)
  }
  # STEP 1: Summarize to the Experiment Level
  # Pair strictly by groupId (which is constant across scenarios)
  dt_exp <- DT_Master[, .(med_loss = median(loss)), 
                      by = .(groupId, Complexity, scenario, forecastHorizon)]
  
  # STEP 2: Parse the groupId string to get the experimental parameters
  # Logic: "Grp_numCov_startYear_endHistory_targetYear"
  dt_exp[, c("Prefix", "Complexity", "StartYear", "HistoryEnd", "TargetYear") := 
           tstrsplit(groupId, "_", type.convert = TRUE)]
  dt_exp[Complexity == Inf, Complexity := 30]
  
  # Remove prefix and set Complexity as a factor
  dt_exp[, Complexity := factor(Complexity, levels = c(2, 5, 10, 30))]
  
  # Calculate the "Total Time Span" (Management Budget)
  # Example: 2009 to 2012 is 4 years of total group life.
  dt_exp[, TimeSpan := (TargetYear - StartYear + 1)]
  
  # STEP 3: Pivot to Wide (The Paired Table)
  # Every groupId becomes exactly ONE row with three result columns.
  dt_paired <- dcast(dt_exp, 
                     groupId + Complexity + StartYear + forecastHorizon + HistoryEnd + TargetYear + TimeSpan ~ scenario, 
                     value.var = "med_loss")
  
  # STEP 4: Calculate the Paired Deltas (The Evidence)
  # H1 (Overfitting): How much is CV lying? (Reality - Illusion)
  dt_paired[, GenGap := FutureUnseen - Internal]
  
  # H2 (PreVal Advantage): Did PreVal beat the Status Quo? (Status Quo - PreVal)
  # Result > 0 means PreVal (Red) had a lower error than CV-Forecast (Blue)
  dt_paired[, PreValAdvantage := FutureTainted - FutureUnseen]
  # Calculate the Percent Increase in Error (The "Optimism Bias" in %)
  # Formula: (Actual Reality - Model's Illusion) / Model's Illusion * 100
  dt_paired[, OptimismBiasPct := (FutureTainted - Internal) / Internal * 100]
  dt_paired[forecastHorizon %in% 1:2, HorizonBin := "Horizon: 1-2 Years"]
  dt_paired[forecastHorizon %in% 3:4, HorizonBin := "Horizon: 3-4 Years"]
  dt_paired[forecastHorizon %in% 5:6, HorizonBin := "Horizon: 5-6 Years"]
  dt_paired[forecastHorizon %in% 7:8, HorizonBin := "Horizon: 7-8 Years"]
  dt_paired[forecastHorizon %in% 9:10, HorizonBin := "Horizon: 9-10 Years"]
  dt_paired[forecastHorizon %in% 11:12, HorizonBin := "Horizon: 11-12 Years"]
  
  # Factor Ordering
  dt_paired[, HorizonBin := factor(HorizonBin,
                                   levels = c("Horizon: 1-2 Years",
                                              "Horizon: 3-4 Years",
                                              "Horizon: 5-6 Years",
                                              "Horizon: 7-8 Years",
                                              "Horizon: 9-10 Years",
                                              "Horizon: 11-12 Years"))]
  # Setting 1: 
  # Plot A: The Overfitting Story (H1)
  # Setting 1: H1 (The Generalization Gap) - Faceted by Complexity
  # Is the behavior consistent across all forecast horizons? YES
  # I tested with dt_paired[forecastHorizon %in% c(1, 3, 5, 7, 10, 12),] and the 
  # behavior is very consistent across forecast horizons. This means that CV is 
  # constantly optimistically biased independent of how far in advance it is 
  # forecasting. Also, the more complex the model, the more biased it is. 
  P1.1 <- ggplot(dt_paired, 
                 aes(x = TimeSpan, y = OptimismBiasPct, color = Complexity, fill = Complexity)) +
    # 1. Background raw points
    geom_point(alpha = 0.6, position = position_jitter(width = 0.2), size = 1) +
    # 2. Colored ribbons (IQR)
    stat_summary(fun.data = median_hilow, geom = "ribbon", alpha = 0.15, color = NA) +
    # 3. Median Trend Line
    stat_summary(fun = median, geom = "line", linewidth = 1.2) +
    # 4. Zero Reference
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.6) +
    # 5. Separate into 4 columns
    facet_grid(~ Complexity, labeller = label_both) +
    # Aesthetics
    scale_color_viridis_d() + 
    scale_fill_viridis_d() +
    scale_y_continuous(labels = unit_format(unit = "%")) + # Format Y-axis as percentages
    theme_minimal() +
    coord_cartesian(ylim = c(0, 12)) +
    labs(title = "Model's Optimism Bias",
         # subtitle = "Percentage by which traditional CV underestimates the model's actual future prediction loss.",
         x = "Total Years of Data (TimeSpan)", 
         y = "Optimism Bias (% Increase in Error)",
         color = "Model Complexity (No. covariates)",
         fill = "Model Complexity (No. covariates)") +
    theme(legend.position = "bottom",
          strip.text = element_blank(),
          panel.grid.minor = element_blank(),
          text = element_text(size = 12),
          plot.title = element_text(face = "bold", size = 14))
  
  # Plot B: The PreVal Advantage (H2)
  # When complexity is adequate and we are forecasting very near-term (i.e., 
  # 1-3 years into the future), both CV and PV have similar performance,
  # although PV is still more stable. As soon as complexity increases a lot,
  # CV models start performing worse than random.
  # There is an ideal complexity which helps models achieve the best performance.
  # But this is also dependent on the forecast horizon. For example, highly complex
  # models are only better at very near-term forecasts IF they perform PV.
  # With longer forecast horizons, even PV models lose their predictive power.
  # Importantly: PV is considerably more robust, but also degrades very quickly if 
  # the prediction made is too far from the training data (i.e., in our case, after a 
  # decade).
  
  DT1 <- DT_Master
  
  DT1[forecastHorizon %in% 1:2, HorizonBin := "Horizon: 1-2 Years"]
  DT1[forecastHorizon %in% 3:4, HorizonBin := "Horizon: 3-4 Years"]
  DT1[forecastHorizon %in% 5:6, HorizonBin := "Horizon: 5-6 Years"]
  DT1[forecastHorizon %in% 7:8, HorizonBin := "Horizon: 7-8 Years"]
  DT1[forecastHorizon %in% 9:10, HorizonBin := "Horizon: 9-10 Years"]
  DT1[forecastHorizon %in% 11:12, HorizonBin := "Horizon: 11-12 Years"]
  
  # Factor Ordering
  DT1[, HorizonBin := factor(HorizonBin,
                             levels = c("Horizon: 1-2 Years",
                                        "Horizon: 3-4 Years",
                                        "Horizon: 5-6 Years",
                                        "Horizon: 7-8 Years",
                                        "Horizon: 9-10 Years",
                                        "Horizon: 11-12 Years"))]
  # Calculate Median
  medianDt <- DT1[, .(mu = median(loss, na.rm = TRUE)),
                  by = .(scenario, Complexity, HorizonBin)] #HorizonBin
  # medianDt2 <- DT1[, .(mu = median(loss, na.rm = TRUE)),
  #                 by = .(scenario, Complexity, forecastHorizon)] #forecastHorizon
  
  qDT <- DT1[, .(
    q25 = quantile(loss, 0.25, na.rm = TRUE),
    q75 = quantile(loss, 0.75, na.rm = TRUE)
  ), by = .(scenario, HorizonBin, Complexity)]
  
  # qDT2 <- DT1[, .(
  #   q25 = quantile(loss, 0.25, na.rm = TRUE),
  #   q75 = quantile(loss, 0.75, na.rm = TRUE)
  # ), by = .(scenario, forecastHorizon, Complexity)]
  
  P1.2 <- ggplot(DT1, aes(x = loss)) +
    # Filled Density Forms
    geom_density(aes(fill = scenario, color = scenario), alpha = 0.3, linewidth = 0.5) +
    # IQR shaded band
    geom_rect(
      data = qDT,
      aes(xmin = q25, xmax = q75, ymin = -Inf, ymax = Inf, fill = scenario),
      inherit.aes = FALSE,
      alpha = 0.15
    ) +
    # scenario Median Lines
    geom_vline(data = medianDt, aes(xintercept = mu, color = scenario),
               linetype = "dashed", linewidth = 0.7) +
    geom_vline(xintercept = 2.3979, linetype = "dotted", color = "black", linewidth = 0.8) +
    # THE GRID
    facet_grid(Complexity ~ HorizonBin) +
    # Aesthetics
    scale_fill_manual(values = c("Internal" = "#4daf4a", 
                                 "FutureTainted" = "#377eb8", 
                                 "FutureUnseen" = "#e41a1c"),
                      labels = c(
                        "Internal" = "Cross-validation",
                        "FutureTainted" = "Forecast with cross-validated model",
                        "FutureUnseen" = "Forecast with predictive validated model"
                      )) +
    scale_color_manual(values = c("Internal" = "#4daf4a", 
                                  "FutureTainted" = "#377eb8", 
                                  "FutureUnseen" = "#e41a1c"),
                       labels = c(
                         "Internal" = "Cross-validation",
                         "FutureTainted" = "Forecast with cross-validated model",
                         "FutureUnseen" = "Forecast with predictive validated model"
                       )) +
    theme_minimal() +
    coord_cartesian(xlim = c(2.2, 2.63)) +
    
    labs(
      title = "Predictive vs. Cross validation", 
      # for different forecast horizons and model complexity (median)",
      x = "Prediction Loss (Lower is better)",
      y = "Density (Strata)"
    ) +
    
    theme(
      legend.position = "bottom",
      text = element_text(size = 12),
      strip.text = element_text(face = "bold", size = 12),
      panel.spacing = unit(1, "lines"),
      plot.title = element_text(face = "bold", size = 14),
      panel.grid.minor = element_blank()
    )
  
  # ANOTHER WAY OF VIEWING P1.2
  # The variance-inflation plot does something more surgical: it computes the 
  # spread within each individual experiment group first, in isolation, and 
  # only afterward summarizes those already-isolated numbers across groups in 
  # the boxplot. That order of operations matters — it deliberately keeps 
  # "how noisy is this one model" separate from "how much do different 
  # experiments disagree with each other," where Density_Grid blends the two 
  # together. So it's less "the same information compressed into a boxplot" 
  # and more "a cleaner, decomposed version that controls for something 
  # Density_Grid doesn't." 
  # Same underlying question (is the spread of errors bigger than Internal 
  # suggests?), different and complementary way of isolating the answer.
  
  # 1. Dispersion per experiment x scenario
  dt_disp <- dt[, .(
    sd_loss  = sd(loss, na.rm = TRUE),
    iqr_loss = IQR(loss, na.rm = TRUE),
    n_obs    = .N
  ), by = .(groupId, Complexity, scenario, forecastHorizon)]
  
  # 2. Pivot wide so each groupId has one SD per scenario
  dt_disp_wide <- dcast(dt_disp,
                        groupId + factor(Complexity, levels = c(2,5,10,30)) + forecastHorizon ~ scenario,
                        value.var = c("sd_loss", "iqr_loss"))
  
  # 3. Variance Inflation: how much wider is the real forecast's spread
  #    than the "oracle" Internal spread, and does it scale with Complexity?
  dt_disp_wide[, VI_StatusQuo := sd_loss_FutureTainted / sd_loss_Internal]
  dt_disp_wide[, VI_PreVal    := sd_loss_FutureUnseen  / sd_loss_Internal]
  
  dt_vi_long <- melt(dt_disp_wide,
                     id.vars = c("groupId", "Complexity", "forecastHorizon"),
                     measure.vars = c("VI_StatusQuo", "VI_PreVal"),
                     variable.name = "Method", value.name = "VarianceInflation")
  
  # 4. Plot: does Internal's apparent stability undersell complex models'
  #    true variability more than it undersells simple models'? This is basically 
  # P1.1. summarized in a boxplot, if I understand it correctly...
  P1.3 <- ggplot(dt_vi_long, aes(x = Complexity, y = VarianceInflation, fill = Method)) +
    geom_boxplot(alpha = 0.6, outlier.alpha = 0.3) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    scale_fill_manual(values = c("VI_StatusQuo" = "#377eb8", "VI_PreVal" = "#e41a1c")) +
    theme_minimal() +
    labs(title = "Variance Inflation Relative to the Internal Baseline",
         subtitle = "Values > 1 mean the real forecast is noisier than Internal suggested",
         x = "Model Complexity (Covariates)", y = "SD(loss) / SD(loss)_Internal")
  
  
  # Setting 2: Does PreVal work better in certain historical periods (e.g., during rapid landscape change)?
  # We look at the "Advantage" vs the actual Year in history
  # Here we show that our tool’s value isn't just a methodological fluke; 
  # it’s a response to ecological volatility. While standard models worked 
  # 'okay' when the landscape was stable (pre-2015), they began to fail as 
  # environmental noise increased. Our iterative validation (PreVal) corrected 
  # this, providing a significantly more accurate forecast for the last 5 years 
  # of caribou history.
  P2.1 <- ggplot(dt_paired, aes(x = TargetYear, y = PreValAdvantage, 
                                color = Complexity, fill = Complexity)) +
    # 1. Zero Reference
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.6) +
    
    # 2. Colored Ribbon (Original Data Variation - IQR)
    stat_summary(fun.data = median_hilow, geom = "ribbon", alpha = 0.15, color = NA) +
    
    # 3. Raw Median Trend (No smoothing)
    stat_summary(fun = median, geom = "line", linewidth = 1.2) +
    stat_summary(fun = median, geom = "point", size = 2) +
    
    # 4. Fix X-axis to Rounded Years
    scale_x_continuous(breaks = seq(min(dt_paired$TargetYear), max(dt_paired$TargetYear), by = 2)) +
    
    # Aesthetics
    facet_grid(.~Complexity) +
    scale_color_viridis_d() + 
    scale_fill_viridis_d() +
    theme_minimal() +
    labs(title = "PreVal Advantage for All Years)",
         x = "Forecast Target Year", y = "Paired Delta Loss (Advantage)") +
    theme(legend.position = "none",,
          text = element_text(size = 11),
          strip.text = element_text(face = "bold"),
          panel.grid.minor = element_blank())
  P2.1
  # Setting 3: If a manager has a 3-year, 6-year, or 9-year budget, is the conclusion the same?
  # Filter for 3 levels of "Data Budgets"
  # dt_budgets <- dt_paired[TimeSpan %in% c(4, 7, 10)]
  # 
  # P3.1 <- P3.1 <- ggplot(dt_budgets, aes(x = TargetYear, y = PreValAdvantage, color = Complexity, fill = Complexity)) +
  #   # 1. Zero Reference
  #   geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.6) +
  #   
  #   # 2. IQR Ribbons (Variation across different experiments)
  #   stat_summary(fun.data = median_hilow, geom = "ribbon", alpha = 0.1, color = NA) +
  #   
  #   # 3. Median Trend (No smoothing, showing raw median per target year)
  #   stat_summary(fun = median, geom = "line", linewidth = 1.2) +
  #   
  #   # 4. Rounded X-axis years
  #   scale_x_continuous(breaks = seq(2012, 2022, by = 2)) +
  #   
  #   # 5. Facet Grid: Budget (Columns) vs Complexity (Rows)
  #   facet_grid(Complexity ~ TimeSpan, labeller = label_both) +
  #   
  #   # Aesthetics
  #   scale_color_viridis_d() + scale_fill_viridis_d() +
  #   theme_minimal() +
  #   labs(title = "Setting 3: Advantage Alignment by Forecast Target Year",
  #        subtitle = "Columns = Data Budget (TimeSpan). Rows = Model Complexity.",
  #        x = "Year Being Forecasted (TargetYear)", y = "PreVal Advantage (Blue - Red)") +
  #   theme(legend.position = "none",
  #         strip.text = element_text(face = "bold"),
  #         panel.grid.minor = element_blank())  
  # Filter for the 3 budget levels and ensure TimeSpan is a factor for coloring
  # compTimes <- unique(sort(dt_paired$TimeSpan))#c(4, 7, 10)
  # dt_budgets <- dt_paired[TimeSpan %in% compTimes]
  # dt_budgets[, TimeSpan := factor(TimeSpan, levels = compTimes)]
  # # 1. Aggregate the FULL paired dataset (Setting 1: Horizon 1)
  # # This includes every year from the minimum to the maximum TimeSpan
  # 
  # dt_full_summary <- dt_paired[forecastHorizon == 1, .(
  #   medAdvantage = median(PreValAdvantage),
  #   q25 = quantile(PreValAdvantage, 0.25),
  #   q75 = quantile(PreValAdvantage, 0.75)
  # ), by = .(Complexity, TimeSpan)]
  # 
  # # Ensure TimeSpan is numeric for a smooth continuous X-axis
  # dt_full_summary[, TimeSpan := as.numeric(TimeSpan)]
  
  
  ###########################################################

  # 1. Aggregate Advantage across both TimeSpan and Horizon
  dt_paired[, HistoryBudget := (HistoryEnd - StartYear + 1)]
  
  # 2. Plot as a Heatmap (Profitability Map)
  # 2. Aggregate the median advantage for the tiles
  dt_stress_agg <- dt_paired[, .(
    med_Advantage = median(PreValAdvantage, na.rm = TRUE)
  ), by = .(Complexity, HistoryBudget, forecastHorizon)]
  
  # 3. The Plot
  P3.1 <- ggplot(dt_stress_agg, aes(x = HistoryBudget, y = forecastHorizon, fill = med_Advantage)) +
    geom_tile(color = "white", linewidth = 0.2) + 
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, name = "PreVal\nAdvantage") +
    facet_grid(~Complexity) +
    scale_y_continuous(breaks = seq(1, 12, by = 1)) +
    scale_x_continuous(breaks = seq(2, max(dt_stress_agg$HistoryBudget), by = 2)) +
    theme_minimal() +
    labs(title = "The Profitability Map: The PreVal 'Stress Test'",
         subtitle = "Blue = PreVal wins despite having one less year of training data than the Status Quo.",
         x = "Years of Historical Data (History Budget)",
         y = "Years into the Future (Forecast Horizon)") +
    theme(strip.text = element_text(face = "bold", size = 12),
          panel.grid.minor = element_blank())
  
  # 1. Group Horizons into 3-year blocks for clean visualization
  dt_paired[, HorizonBlock := fcase(
    forecastHorizon <= 3, "Horizon: 1-3y",
    forecastHorizon > 3 & forecastHorizon <= 6, "Horizon: 4-6y",
    forecastHorizon > 6 & forecastHorizon <= 9, "Horizon: 7-9y",
    forecastHorizon > 9, "Horizon: 10-12y"
  )]
  
  dt_paired[, HorizonBlock := factor(HorizonBlock, levels = c(
    "Horizon: 1-3y", "Horizon: 4-6y", "Horizon: 7-9y", "Horizon: 10-12y"
  ))]
  
  # 2. Calculate the Advantage per Validation Year (HistoryEnd)
  dt_anchor_stress <- dt_paired[, .(
    med_Adv = median(PreValAdvantage, na.rm = TRUE),
    iqr_lower = quantile(PreValAdvantage, 0.25, na.rm = TRUE),
    iqr_upper = quantile(PreValAdvantage, 0.75, na.rm = TRUE),
    n_exp = .N
  ), by = .(HistoryEnd, Complexity, HorizonBlock)]
  
  # 3. Filter for robust data (Require at least 2 experiments to draw a bar)
  dt_anchor_filtered <- dt_anchor_stress[n_exp >= 2]
  
  # 4. The Plot
  P3.2 <- ggplot(dt_anchor_filtered, aes(x = as.factor(HistoryEnd), y = med_Adv, fill = med_Adv > 0)) +
    geom_bar(stat = "identity", color = "black", alpha = 0.8) +
    geom_errorbar(aes(ymin = iqr_lower, ymax = iqr_upper), width = 0.3, alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
    facet_grid(HorizonBlock ~ Complexity, scales = "free_y") +
    scale_fill_manual(values = c("TRUE" = "#377eb8", "FALSE" = "#e41a1c"), guide = "none") +
    coord_cartesian(ylim = c(-0.1, 0.1)) +
    theme_minimal() +
    labs(title = "Impact of the Validation Year",
         x = "Year Used for Validation", 
         y = "Median PreVal Advantage") +
    theme(strip.text = element_text(face = "bold", size = 11),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor = element_blank())
  
  ###########################################################
  
  # 1. Calculate Difficulty (Tainted) and Surprise (Tainted vs Internal)
  year_stats <- dt_paired[forecastHorizon == 1, .(
    med_loss = median(FutureTainted), # Use Tainted to show Status Quo difficulty
    q25_loss = quantile(FutureTainted, 0.25),
    q75_loss = quantile(FutureTainted, 0.75),
    med_bias = median(OptimismBiasPct) # Now based on Tainted - Internal
  ), by = .(TargetYear)]
  
  # 1. Global Year Stats (Mixing all horizons)
  year_stats_global <- dt_paired[, .(
    med_loss = median(FutureTainted),
    q25_loss = quantile(FutureTainted, 0.25),
    q75_loss = quantile(FutureTainted, 0.75),
    med_bias = median(OptimismBiasPct)
  ), by = .(TargetYear)]
  
  # # 2. Updated Plot P4.1
  # P4.1 <- ggplot(year_stats, aes(x = TargetYear)) +
  #   geom_ribbon(aes(ymin = q25_loss, ymax = q75_loss), alpha = 0.15, fill = "grey20") +
  #   geom_line(aes(y = med_loss), linewidth = 1, color = "black") +
  #   geom_point(aes(y = med_loss, size = med_bias, color = med_bias)) +
  #   scale_color_viridis_c(option = "plasma", name = "Surprise (Bias %)") +
  #   scale_size_continuous(name = "Surprise (Bias %)") +
  #   theme_minimal() +
  #   scale_x_continuous(breaks = seq(min(year_stats_global$TargetYear), max(year_stats_global$TargetYear), by = 1)) +
  #   labs(title = "Status Quo Forecasting: Difficulty and Deception",
  #        # subtitle = "Metrics (Bias) lie most when the standard model (Tainted) fails most.",
  #        x = "Target Year",
  #        y = "Median Status Quo Prediction Loss")
  
  # 2. PLOT: Absolute Difficulty with Bias as Point Scale
  P4.1 <- ggplot(year_stats_global, aes(x = TargetYear)) +
    # 1. Background Confidence Ribbon
    geom_ribbon(aes(ymin = q25_loss, ymax = q75_loss), alpha = 0.15, fill = "darkblue") +
    
    # 2. Median Difficulty Line
    geom_line(aes(y = med_loss), linewidth = 1) +
    
    # 3. Points: Size and Color both mapped to med_bias
    geom_point(aes(y = med_loss, size = med_bias, color = med_bias)) +
    
    # 4. Merging and Renaming Scales
    scale_color_viridis_c(option = "magma", name = "Optimism Bias %") +
    scale_size_continuous(name = "Optimism Bias %") + # This merges the legends
    
    # 5. Fix X-axis breaks to whole years
    scale_x_continuous(breaks = seq(min(year_stats_global$TargetYear), 
                                    max(year_stats_global$TargetYear), by = 1)) +
    
    theme_minimal() +
    labs(title = "Global Year Difficulty (Aggregate of All Horizons)",
         y = "Median Prediction Loss", 
         x = "Target Year") +
    theme(legend.position = "right",
          plot.title = element_text(face = "bold", size = 14),
          axis.text.x = element_text(angle = 45, hjust = 1)) # Tilt years if they overlap
  
  # 3. Corrected Correlation Stat
  cor_val <- round(cor(year_stats_global$med_loss, year_stats_global$med_bias), 3)
  
  # 1. Prepare the data (AGGREGATE THE STATUS QUO STORY)
  dt_cor <- dt_paired[, .(
    med_loss = median(FutureTainted), # FIXED: Use the Status Quo failure
    med_bias = median(OptimismBiasPct) # Ensure this was: (Tainted - Internal) / Internal
  ), by = .(TargetYear)]
  
  # 2. CALCULATE THE NEW CORRELATION (This is your new "r" value)
  cor_val <- round(cor(dt_cor$med_loss, dt_cor$med_bias), 3)
  
  # 3. The "Synchronization of Risk" Plot
  P4.2 <- ggplot(dt_cor, aes(x = med_bias, y = med_loss)) +
    # Linear trend line
    geom_smooth(method = "lm", color = "black", linetype = "dashed", alpha = 0.1) +
    
    # Main data points
    geom_point(size = 4, aes(color = TargetYear)) + # Added color by year for visual interest
    
    # Clean year labels
    geom_text_repel(aes(label = TargetYear), size = 4, 
                    box.padding = 1.2,       
                    point.padding = 0.5,     
                    force = 10,              
                    segment.color = "grey50", 
                    segment.alpha = 0.6,      
                    min.segment.length = 0) +
    
    # NEW Pearson correlation label
    annotate("label", x = min(dt_cor$med_bias), y = max(dt_cor$med_loss), 
             label = paste0("Pearson's r = ", cor_val),
             fill = "white", fontface = "bold", size = 6, hjust = 0) +
    
    # Aesthetics
    scale_color_viridis_c(option = "magma") + # Visualizes the progression of years
    theme_minimal() +
    labs(title = "Loss vs. Deception",
         # subtitle = "Status Quo Audit: Metrics lie most exactly when the standard model fails most significantly.",
         x = "Optimism Bias (% Underestimation of Error)", 
         y = "Median Prediction Loss Cross-Validated Forecast") +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold", size = 16),
          text = element_text(size = 12),
          axis.title = element_text(face = "bold", size = 12),
          panel.grid.minor = element_blank())
  
  ##################

  # 1. Filter for a specific History Budget (e.g., 3 years) 
  # This ensures every line has the exact same amount of training data
  dt_individual <- dt_paired[HistoryBudget == 3]
  
  # 2. Create a clean label for each specific experiment
  # e.g., "History: 2009-2011 (Val: 2011)"
  dt_individual[, Exp_Label := paste0("Hist: ", StartYear, "-", HistoryEnd, " (Val: ", HistoryEnd, ")")]
  
  # 3. Plot the individual trajectories over their Forecast Horizons
  P6.1 <- ggplot(dt_individual, aes(x = forecastHorizon, y = PreValAdvantage, color = Exp_Label)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
    
    # Draw the line for each specific experiment
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_point(size = 2) +
    
    # Facet by Complexity to isolate the C30 behavior
    facet_grid(~Complexity, scales = "free_y") +
    
    scale_color_viridis_d(option = "turbo", name = "Specific Experiment") +
    theme_minimal() +
    scale_x_continuous(breaks = 1:12) +
    labs(title = "The Lifespan of Individual Experiments (3-Year History Budget)",
         subtitle = "Tracking single (N=1) experiments over time. Do they all eventually crash?",
         x = "Years into the Future (Forecast Horizon)", 
         y = "PreVal Advantage (Blue/Above 0 = PreVal Wins)") +
    theme(legend.position = "right", 
          strip.text = element_text(face = "bold", size = 12),
          panel.grid.minor = element_blank())
  
  ##################

  # 7. ABSOLUT PREDICTIVE POWER
  
  # 1. Filter for the operational forecasting window (e.g., 1 to 5 years)
  dt_abs_perf <- dt_paired
  # 2. Calculate the Median Absolute Loss per Complexity for both methods
  dt_abs_summary <- dt_abs_perf[, .(
    Status_Quo_Loss_Med = median(FutureTainted, na.rm = TRUE),
    Status_Quo_Loss_Q25 = quantile(FutureTainted, 0.25, na.rm = TRUE),
    Status_Quo_Loss_Q75 = quantile(FutureTainted, 0.75, na.rm = TRUE),
    
    PreVal_Loss_Med = median(FutureUnseen, na.rm = TRUE),
    PreVal_Loss_Q25 = quantile(FutureUnseen, 0.25, na.rm = TRUE),
    PreVal_Loss_Q75 = quantile(FutureUnseen, 0.75, na.rm = TRUE)
  ), by = .(Complexity)]  
  # Melt for easy plotting
  dt_abs_melt_med <- melt(dt_abs_summary, id.vars = "Complexity", 
                          measure.vars = c("Status_Quo_Loss_Med", "PreVal_Loss_Med"),
                          variable.name = "Method_Med", value.name = "Absolute_Loss")
  
  # 3. Melt the Lower bounds
  dt_abs_melt_q25 <- melt(dt_abs_summary, id.vars = "Complexity", 
                          measure.vars = c("Status_Quo_Loss_Q25", "PreVal_Loss_Q25"),
                          variable.name = "Method_Q25", value.name = "Loss_Q25")
  
  # 4. Melt the Upper bounds
  dt_abs_melt_q75 <- melt(dt_abs_summary, id.vars = "Complexity", 
                          measure.vars = c("Status_Quo_Loss_Q75", "PreVal_Loss_Q75"),
                          variable.name = "Method_Q75", value.name = "Loss_Q75")  
  # 3. Plot Absolute Performance
  dt_plot_abs <- dt_abs_melt_med
  dt_plot_abs[, Method := ifelse(grepl("Status", Method_Med), "Status Quo", "PreVal")]
  dt_plot_abs[, Loss_Q25 := dt_abs_melt_q25$Loss_Q25]
  dt_plot_abs[, Loss_Q75 := dt_abs_melt_q75$Loss_Q75]
  
  # This allows geom_ribbon to draw a continuous polygon across the categories
  dt_plot_abs[, Comp_Num := as.numeric(as.character(Complexity))]
  
  # 3. The Ribbon Plot
  P5.1 <- ggplot(dt_plot_abs, aes(x = Comp_Num, color = Method, fill = Method)) +
    
    # The IQR Polygon (Transparency set to 0.15 so lines pop)
    geom_ribbon(aes(ymin = Loss_Q25, ymax = Loss_Q75), alpha = 0.15, color = NA) +
    
    # Median Lines and Points
    geom_line(aes(y = Absolute_Loss), linewidth = 1.5) +
    geom_point(aes(y = Absolute_Loss), size = 4) +
    
    scale_color_manual(values = c("Status Quo" = "#377eb8", "PreVal" = "#e41a1c")) +
    scale_fill_manual(values = c("Status Quo" = "#377eb8", "PreVal" = "#e41a1c")) +
    
    # Force X-axis to only show the exact complexity labels
    scale_x_continuous(breaks = c(2, 5, 10, 30)) +
    
    theme_minimal() +
    labs(title = "Absolute Predictive Power: Parsimony vs. Complexity",
         subtitle = "Lines = Median Loss. Shaded Area = Interquartile Range (25th to 75th percentile).",
         x = "Model Complexity (Covariates)",
         y = "Absolute Prediction Loss (Lower is Better)") +
    theme(legend.position = "bottom", 
          plot.title = element_text(face = "bold", size = 14),
          panel.grid.minor.x = element_blank(), # Removes awkward vertical lines between complexities
          axis.text.x = element_text(size = 12))
  
  # 2. Aggregate the median advantage AND count the experiments per cell
  dt_stress_agg_robust <- dt_paired[, .(
    med_Advantage = median(PreValAdvantage, na.rm = TRUE),
    n_exp = uniqueN(groupId) # Count how many unique experiments formed this median
  ), by = .(Complexity, HistoryBudget, forecastHorizon)]
  
  # 3. Filter to keep ONLY the cells with more than 3 experiments
  dt_stress_filtered <- dt_stress_agg_robust[n_exp > 3]
  
  # 4. The Plot
  P8.1 <- ggplot(dt_stress_filtered, aes(x = HistoryBudget, y = forecastHorizon, fill = med_Advantage)) +
    geom_tile(color = "white", linewidth = 0.2) + 
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, name = "PreVal\nAdvantage") +
    facet_grid(~Complexity) +
    scale_y_continuous(breaks = seq(1, 12, by = 1)) +
    scale_x_continuous(breaks = seq(2, max(dt_stress_filtered$HistoryBudget, na.rm=TRUE), by = 2)) +
    theme_minimal() +
    labs(title = "The Robust Profitability Map (n > 3 experiments)",
         subtitle = "Edges removed to isolate the stable, high-confidence signal.",
         x = "Years of Historical Data (History Budget)",
         y = "Years into the Future (Forecast Horizon)") +
    theme(strip.text = element_text(face = "bold", size = 12),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "grey95", color = NA)) # Grey background highlights the trimmed shape
  
  print(P8.1)

  
    # 1. We want the Median Absolute Loss across all Horizons
  dt_abs_decay <- dt_paired[, .(
    med_Loss_PreVal = median(FutureUnseen, na.rm = TRUE),
    med_Loss_StatusQuo = median(FutureTainted, na.rm = TRUE)
  ), by = .(Complexity, forecastHorizon)]
  
  # 2. Melt the data for plotting two lines
  dt_abs_decay_melt <- melt(dt_abs_decay, id.vars = c("Complexity", "forecastHorizon"), 
                            measure.vars = c("med_Loss_PreVal", "med_Loss_StatusQuo"),
                            variable.name = "Method", value.name = "Absolute_Loss")
  
  # 3. Plot the 'Decline to Random' Trajectory
  P7.1 <- ggplot(dt_abs_decay_melt, aes(x = forecastHorizon, y = Absolute_Loss, color = Method)) +
    # Add a smooth curve to show the general trend of decay
    geom_smooth(method = "loess", span = 0.6, se = FALSE, linewidth = 1.5) +
    geom_point(size = 2, alpha = 0.5) +
    
    facet_grid(~Complexity, scales = "free_y") +
    scale_color_manual(values = c("med_Loss_PreVal" = "#e41a1c", "med_Loss_StatusQuo" = "#377eb8"),
                       labels = c("PreVal (Red)", "Status Quo (Blue)")) +
    scale_x_continuous(breaks = 1:12) +
    theme_minimal() +
    labs(title = "The 'Decline to Random' Hypothesis",
         subtitle = "Do both models eventually hit a ceiling of maximum error at extreme horizons?",
         x = "Forecast Horizon (Years into the Future)",
         y = "Median Absolute Prediction Loss (Higher = Worse)") +
    theme(legend.position = "bottom",
          strip.text = element_text(face = "bold", size = 12),
          plot.title = element_text(face = "bold", size = 14))
  
  # 1. THE STATIC MODEL (Anchored in 2011)
  # It predicts 2012 (Horizon 1), 2013 (Horizon 2) ... 2022 (Horizon 11)
  dt_static <- dt_paired[HistoryEnd == 2011, .(
    med_Loss = median(FutureUnseen, na.rm = TRUE),
    Strategy = "Static Model (Anchored in 2011)"
  ), by = .(TargetYear, Complexity)]
  
  # 2. THE ITERATIVE MODEL (Moving Window)
  # It predicts every year using a recently updated model (e.g., Horizon 1 or 2)
  # Let's use Horizon 2 to give it a realistic "short-term forecasting" gap
  dt_iterative <- dt_paired[forecastHorizon == 2, .(
    med_Loss = median(FutureUnseen, na.rm = TRUE),
    Strategy = "Iterative Model (Rolling 2-Year Horizon)"
  ), by = .(TargetYear, Complexity)]
  
  # 3. Combine and filter for overlapping years
  dt_update_test <- rbind(dt_static, dt_iterative)
  dt_update_test <- dt_update_test[TargetYear >= 2014] # Start where both have data
  
  # 4. Plot the "Power of Updating"
  P9.1 <- ggplot(dt_update_test, aes(x = TargetYear, y = med_Loss, color = Strategy)) +
    geom_line(linewidth = 1.5, alpha = 0.8) +
    geom_point(size = 3) +
    
    facet_grid(~Complexity, scales = "free_y") +
    scale_color_manual(values = c("Static Model (Anchored in 2011)" = "#e41a1c", # Red
                                  "Iterative Model (Rolling 2-Year Horizon)" = "#4daf4a")) + # Green
    scale_x_continuous(breaks = 2013:2022) +
    theme_minimal() +
    labs(title = "The Power of Iterative Forecasting: Avoiding the Crash",
         subtitle = "Does a continuously updated model (Green) avoid the catastrophic drift of a static model (Red)?",
         x = "Target Year Being Predicted",
         y = "Absolute Prediction Loss (Lower is Better)") +
    theme(legend.position = "bottom",
          strip.text = element_text(face = "bold", size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1))

  ################### TESTING OF INDIVIDUAL LEAKAGE
  if (is.null(extractedFeatures)) {
      warning("extractedFeatures not provided. Testing of individual leakage will not be performed", immediate. = TRUE)
  } else {
    DT_master2 <- copy(DT_Master)
    # one row per group x scenario, with the year windows actually used
    design <- unique(DT_master2[, .(groupId, scenario, Complexity, trainStartYear, trainEndYear, testStartYear)])
    
    # which animals show up as a real (used) step in each calendar year
    yearlyIDs <- extractedFeatures[case_ == TRUE, .(ids = list(unique(id))), by = year]
    setkey(yearlyIDs, year)
    
    getIDSet <- function(startY, endY) {
      yrs <- yearlyIDs[year >= startY & year <= endY]
      unique(unlist(yrs$ids))
    }
    
    idOverlap <- design[, {
      trainIDs <- getIDSet(trainStartYear, trainEndYear)
      testIDs  <- getIDSet(testStartYear, testStartYear)
      .(nTrainIDs = length(trainIDs),
        nTestIDs  = length(testIDs),
        pctTestIDsSeenInTrain = if (length(testIDs) > 0)
          length(intersect(trainIDs, testIDs)) / length(testIDs) * 100
        else NA_real_)
    }, by = .(groupId, scenario, Complexity)]
    
    overID <- idOverlap[, .(medianPctOverlap = median(pctTestIDsSeenInTrain, na.rm = TRUE),
                  n = .N), by = scenario]
    
    # 1. Rebuild dt_paired from DT_master.csv (same logic your plotModels.R already uses)
    dt_exp <- DT_master2[, .(med_loss = median(loss)),
                         by = .(groupId, Complexity, scenario, forecastHorizon)]
    dt_exp[, c("Prefix","Complexity","StartYear","HistoryEnd","TargetYear") :=
             tstrsplit(groupId, "_", type.convert = TRUE)]
    dt_exp[Complexity == Inf, Complexity := 30]
    dt_exp[, Complexity := factor(Complexity, levels = c(2,5,10,30))]
    
    dt_paired <- dcast(dt_exp,
                       groupId + Complexity + StartYear + forecastHorizon + HistoryEnd + TargetYear ~ scenario,
                       value.var = "med_loss")
    dt_paired[, PreValAdvantage := FutureTainted - FutureUnseen]
    
    # 2. Pivot your idOverlap table wide: one row per group, with each scenario's overlap %
    overlap_wide <- dcast(idOverlap, groupId ~ scenario, value.var = "pctTestIDsSeenInTrain")
    setnames(overlap_wide, c("FutureTainted","FutureUnseen"), c("overlap_StatusQuo","overlap_PreVal"))
    overlap_wide[, overlapGap := overlap_StatusQuo - overlap_PreVal]
    
    # 3. Join, then check whether the identity-overlap gap explains PreVal's advantage
    #    beyond what Complexity alone would predict (Complexity as a covariate, so we
    #    don't repeat the shared-term mistake from the earlier correlation plot)
    dt_check <- merge(dt_paired,
                      overlap_wide[, .(groupId, overlap_StatusQuo, overlap_PreVal, overlapGap)],
                      by = "groupId")
    
    corr <- cor(dt_check$overlapGap, dt_check$PreValAdvantage, use = "complete.obs")
    fit <- lm(PreValAdvantage ~ overlapGap + Complexity, data = dt_check)

    # 1. Flag influential points via Cook's distance (common rule of thumb: 4/n)
    cooksd <- cooks.distance(fit)
    threshold <- 4 / nrow(dt_check)
    influential <- which(cooksd > threshold)
    length(influential)
    
    # 2. Sanity check: do the flagged points look like the poisoned-anchor cases
    #    (2010 validation year) or the wild complexity-30 outliers we already know about?
    dt_check[influential, .(groupId, Complexity, PreValAdvantage, overlapGap)]
    
    # 3. Refit with those points excluded, compare side by side
    dt_trimmed   <- dt_check[-influential, ]
    fit_trimmed  <- lm(PreValAdvantage ~ overlapGap + Complexity, data = dt_trimmed)
    
    comparison <- data.table(
      term     = names(coef(fit)),
      original = coef(fit),
      trimmed  = coef(fit_trimmed)[names(coef(fit))]
    )
    comparison
    summary(fit_trimmed)
    
    # 4. Alternative that doesn't require picking a cutoff: robust regression,
    #    which downweights outliers automatically rather than deleting them
    Require::Require("MASS")
    fit_robust <- rlm(PreValAdvantage ~ overlapGap + Complexity, data = dt_check)
    summary(fit_robust)
    
    # rlm doesn't give p-values by default — get them like this:
    Require::Require("sfsmisc")
    f.robftest(fit_robust, var = "overlapGap")
    f.robftest(fit_robust, var = "Complexity30")
  }
  
  
  ###########################################

  # Dimensions for a standard 16:9 slide (in inches)
  w <- 12 
  h <- 6.75
  res <- 300 # Dots Per Inch (DPI)
  
  # Save the Faceted Optimism Bias Plot
  ggsave(file.path(outPath, "OptimismBias_Faceted.png"), plot = P1.1, 
         width = w, height = h, dpi = res, bg = "white")
  
  # Save the Synchronization Scatter Plot
  ggsave(file.path(outPath, "Synchronization_Scatter.png"), plot = P4.2, 
         width = 8, height = 6, dpi = res, bg = "white") 
  
  # Save the Global Year Difficulty Plot
  ggsave(file.path(outPath, "GlobalYearDiff.png"), plot = P4.1, 
         width = 8, height = 6, dpi = res, bg = "white") 
  
  # Save the Profitability Heatmap
  ggsave(file.path(outPath, "Profitability_Heatmap.png"), plot = P3.1, 
         width = w, height = h, dpi = res, bg = "white")
  
  # Save the Profitability Heatmap
  ggsave(file.path(outPath, "Impact_Validation_Year.png"), plot = P3.2, 
         width = w, height = h, dpi = res, bg = "white")
  
  # Save the All Years Advantage Plot
  ggsave(file.path(outPath, "PreVal_Advantage_Timeline.png"), plot = P2.1, 
         width = w, height = h, dpi = res, bg = "white")
  
  ggsave(file.path(outPath,"Density_Grid.png"), plot = P1.2, 
         width = 16, height = 10, dpi = res, bg = "white")
  
  ggsave(file.path(outPath, "Check_If_Hard_years.png"), plot = P5.1, 
         width = w, height = h, dpi = res, bg = "white")
  
  ggsave(file.path(outPath, "Check_Ind_Experiments.png"), plot = P6.1, 
         width = w, height = h, dpi = res, bg = "white")
  
  return(list(P11 = P1.1,
              P12 = P1.2,
              P13 = P1.3,
              P21 = P2.1,
              P31 = P3.1,
              P32 = P3.2,
              P41 = P4.1,
              P42 = P4.2,
              P51 = P5.1,
              P61 = P6.1,
              P71 = P7.1,
              P81 = P8.1,
              P91 = P9.1,
              corrLossBias = cor_val,
              percOverlapINDV = overID,
              corrOverlapGapAndPreValAdvtg = corr,
              statisticalAdvantPreVal = summary(fit)))
}
