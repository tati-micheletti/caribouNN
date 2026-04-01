# EXAMPLE CODE FOR COMPARISON...
plotModels <- function(fittedTable, maxClu, useFuture, outPath){
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
  DTmaster <- file.path(outPath, "DT_master.csv")
  if (file.exists(DTmaster)){
    print(paste0("DT master found! Loading from file... "))
    DT_Master <- fread(DTmaster)
  } else {
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
  dt_paired[, OptimismBiasPct := (FutureUnseen - Internal) / Internal * 100]
   dt_paired[, PreValImprovementPct := (FutureTainted - FutureUnseen) / FutureTainted * 100]
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
    labs(title = "Setting 1: H1 (Optimism Bias - % Underestimated Error)",
         subtitle = "Percentage by which traditional CV underestimates the model's actual future prediction loss.",
         x = "Total Years of Data (TimeSpan)", 
         y = "Optimism Bias (% Increase in Error)",
         color = "Model Complexity (No. covariates)",
         fill = "Model Complexity (No. covariates)") +
    theme(legend.position = "bottom",
          strip.text = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold", size = 14))
  P1.1
  
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
  medianDt2 <- DT1[, .(mu = median(loss, na.rm = TRUE)),
                  by = .(scenario, Complexity, forecastHorizon)] #forecastHorizon

  qDT <- DT1[, .(
    q25 = quantile(loss, 0.25, na.rm = TRUE),
    q75 = quantile(loss, 0.75, na.rm = TRUE)
  ), by = .(scenario, HorizonBin, Complexity)]
  
  qDT2 <- DT1[, .(
    q25 = quantile(loss, 0.25, na.rm = TRUE),
    q75 = quantile(loss, 0.75, na.rm = TRUE)
  ), by = .(scenario, forecastHorizon, Complexity)]
  
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
    facet_grid(Complexity ~ HorizonBin, scales = "free_y") +
    # Aesthetics
    scale_fill_manual(values = c("Internal" = "#4daf4a", "FutureTainted" = "#377eb8", "FutureUnseen" = "#e41a1c"),
                      labels = c(
                        "Cross-validation",
                        "Forecast with cross-validated model",
                        "Forecast with predictive validated model"
                      )) +
    scale_color_manual(values = c("Internal" = "#4daf4a", "FutureTainted" = "#377eb8", "FutureUnseen" = "#e41a1c"),
                       labels = c(
                         "Cross-validation",
                         "Forecast with cross-validated model",
                         "Forecast with predictive validated model"
                       )) +
    theme_minimal() +
    coord_cartesian(xlim = c(2.2, 2.63)) +
    
    labs(
      title = "Predictive vs. Cross validation for different forecast horizons and model complexity (median)",
      x = "Prediction Loss (Lower is better)",
      y = "Density (Strata)"
    ) +
    
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold", size = 11),
      panel.spacing = unit(1, "lines"),
      plot.title = element_text(face = "bold", size = 14),
      panel.grid.minor = element_blank()
    )
    P1.2
  
    browser()
  # Setting 2: Does PreVal work better in certain historical periods (e.g., during rapid landscape change)?
  # We look at the "Advantage" vs the actual Year in history
  P2.1 <- ggplot(dt_paired, aes(x = TargetYear, y = PreValAdvantage, color = Complexity)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    # Smooth across the years
    geom_smooth(method = "loess", span = 0.4, se = TRUE, alpha = 0.1) +
    facet_wrap(~Complexity, scales = "free_y") +
    theme_minimal() +
    labs(title = "Setting 2: PreVal Advantage Through History (All Target Years)",
         subtitle = "Shows if the tool's value is stable or context-dependent.",
         x = "Forecast Target Year", y = "Paired Delta Loss (Advantage)")
  P2.1
  
  # Setting 3: If a manager has a 3-year, 6-year, or 9-year budget, is the conclusion the same?
  # Filter for 3 levels of "Data Budgets"
  dt_budgets <- dt_paired[TimeSpan %in% c(4, 7, 10)]
  
  P3.1 <- ggplot(dt_budgets, aes(x = StartYear, y = PreValAdvantage, color = Complexity)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_smooth(method = "loess", span = 0.6, se = FALSE, linewidth = 1.2) +
    # Facet by the budget size
    facet_grid(Complexity ~ TimeSpan, labeller = label_both) +
    theme_minimal() +
    labs(title = "Setting 3: Advantage Stability Across Budgets and Years",
         subtitle = "Columns = Data Budget (TimeSpan). Rows = Model Complexity.",
         x = "Experiment Start Year", y = "PreVal Advantage")
  P3.1 


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # I THINK THE DENSITY PLOT SHOWS THIS MUCH BETTER...This plot is confusing!
  # P1.2 <- ggplot(dt_paired, 
  #        aes(x = TimeSpan, y = PreValAdvantage, color = Complexity, fill = Complexity)) +
  #   # 1. Background raw points
  #   geom_point(alpha = 0.6, position = position_jitter(width = 0.2), size = 1) +
  #   # 2. Colored ribbons (IQR)
  #   stat_summary(fun.data = median_hilow, geom = "ribbon", alpha = 0.15, color = NA) +
  #   # 3. Median Trend Line
  #   stat_summary(fun = median, geom = "line", linewidth = 1.2) +
  #   # 4. Zero Reference
  #   geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.6) +
  #   # 5. Separate into 4 columns
  #   facet_wrap(HorizonBin ~ Complexity, ncol = 4, labeller = label_both) +
  #   # Aesthetics
  #   scale_color_viridis_d() + 
  #   scale_fill_viridis_d() +
  #   scale_y_continuous(labels = unit_format(unit = "%")) + # Format Y-axis as percentages
  #   theme_minimal() +
  #   coord_cartesian(ylim = c(-0.1, 0.5)) +
  #   labs(title = "Setting 1: H2 (The PreVal Advantage)",
  #        subtitle = "Delta = (Blue - Red). Positive = PreVal WON.",
  #        x = "Total Years Spanned by Experiment (TimeSpan)", y = "Paired Delta Loss") +
  #   theme(legend.position = "none",
  #         strip.text = element_blank(),
  #         plot.title = element_text(face = "bold", size = 14))
  #         
  #         
  # # Summarize DT_Master to the Experiment Level (one row per groupId)
  # # We calculate the median loss for each unique experiment run.
  # dt_experiments <- DT_Master[, .(
  #   med_loss = median(loss)
  # ), by = .(groupId, Complexity, scenario, trainingWindow, 
  #           trainEndYear, testStartYear, forecastHorizon)]
  # 
  # # Pivot to Wide Format
  # # This puts our scenarios side-by-side for the SAME groupId
  # dt_paired <- dcast(dt_experiments, 
  #                    groupId + Complexity + forecastHorizon ~ scenario, 
  #                    value.var = "med_loss")
  # 
  # # STEP 3: Calculate the Scientific "Deltas" (The Effect Sizes)
  # # H1 (Overfitting Gap): How much does the model lie to us?
  # dt_paired[, GenGap := FutureUnseen - Internal]
  # 
  # # H2 (PreVal Advantage): How much better is our tool than the status quo?
  # # Note: Status Quo (Tainted/Blue) minus our Tool (Unseen/Red)
  # dt_paired[, PreValAdvantage := FutureTainted - FutureUnseen]
  # 
  # # ----------------------------------------------------------------------------------------- #
  # # SETTING 1: Constant Forecast Horizon (The "Accumulating Data" Story)                      #
  # # "Same starting year (2009), fixed horizon (1 year), increasing training window"           #
  # # ----------------------------------------------------------------------------------------- #
  # 
  # # Plot A: The Generalization Gap (H1)
  # # "How much is the model lying to us across different complexities?"
  # P1.1 <- ggplot(dt_paired, aes(x = forecastHorizon, y = GenGap, color = Complexity)) +
  #   geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  #   geom_smooth(method = "loess", se = TRUE, linewidth = 1.5) + # Loess shows the real shape better
  #   geom_point(alpha = 0.2) +
  #   theme_minimal() +
  #   labs(title = "H1: The Generalization Gap (Strictly Paired)",
  #        x = "Relative Training Depth (Years)", y = "Delta Loss (PV - CV)")
  # P1.1
  # 
  # # Plot B: The PreVal Advantage (H2)
  # # "Did we beat the Status Quo for the same target?"
  # P1.2 <- ggplot(dt_paired[forecastHorizon == 1], aes(x = ref_window, y = PreValAdvantage, color = Complexity)) +
  #   geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  #   geom_smooth(method = "loess", se = TRUE, linewidth = 1.5) +
  #   geom_point(alpha = 0.2) +
  #   theme_minimal() +
  #   labs(title = "H2: The PreVal Advantage (Strictly Paired)",
  #        x = "Relative Training Depth (Years)", y = "Delta Loss (Blue - Red)")
  # P1.2
  # 
  # # ----------------------------------------------------------------------------------------- #
  # # SETTING 2: Constant Forecast Year (The "Predicting 2022" Story)                           #
  # # "Every model is predicting the same target (2022) with different histories"               #
  # # ----------------------------------------------------------------------------------------- #
  # 
  # DT_Setting2 <- DT_Master[testStartYear == 2022] # All years
  # 
  # # 1. Summarize 
  # sum2 <- DT_Setting2[, .(
  #   med = median(loss),
  #   q25 = quantile(loss, 0.25),
  #   q75 = quantile(loss, 0.75)
  # ), by = .(trainingWindow, Complexity, scenario)]
  # 
  # # 2. Plot
  # ggplot(sum2, aes(x = as.factor(trainingWindow), y = med, color = scenario)) +
  #   geom_pointrange(aes(ymin = q25, ymax = q75), position = position_dodge(width = 0.5)) +
  #   facet_grid(. ~ Complexity, labeller = label_both) +
  #   scale_color_manual(values = scen_colors, labels = scen_labels) +
  #   theme_minimal() +
  #   labs(title = "Setting 2: Predicting Year 2022 Performance",
  #        subtitle = "Comparing different training window lengths (Depth) for the same target",
  #        x = "Number of Training Years (Window Depth)", y = "Median Prediction Loss") +
  #   theme(legend.position = "bottom")
  # 
  # # ----------------------------------------------------------------------------------------- #
  # # SETTING 3: Constant Forecast Window (The "Moving Window" Story)                           #
  # # "Training is always exactly 3 years, but the window moves through time"                   #
  # # ----------------------------------------------------------------------------------------- #
  # 
  # # Note: You can change '3' to whatever your standard window was in the experiment
  # DT_Setting3 <- DT_Master[trainingWindow == 3]
  # 
  # # 1. Summarize
  # sum3 <- DT_Setting3[, .(
  #   med = median(loss),
  #   q25 = quantile(loss, 0.25),
  #   q75 = quantile(loss, 0.75)
  # ), by = .(trainStartYear, Complexity, scenario)]
  # 
  # # 2. Plot
  # ggplot(sum3, aes(x = trainStartYear, y = med, color = scenario, group = scenario)) +
  #   geom_line(linewidth = 1) +
  #   geom_point(size = 2) +
  #   facet_wrap(~Complexity, labeller = label_both) +
  #   scale_color_manual(values = scen_colors, labels = scen_labels) +
  #   theme_minimal() +
  #   labs(title = "Setting 3: Stability of Ecological Drivers (3-Year Moving Window)",
  #        subtitle = "Testing if predictive performance changes as the landscape evolves",
  #        x = "Start Year of 3-Year Training Window", y = "Median Prediction Loss") +
  #   theme(legend.position = "bottom")
  # 
  # 
  # print("Settings generated. Ready for visualization.")
  

  ########################################################################
  # PLOT 0: Density of loss for each caribou decision for all scenarios  #
  # This plot focuses on the horizon. However, we have probably a strong #
  # effect of amount of data that needs to be taken into account.        #
  # 1. Prepare Data and Define Horizon Bins                              #
  ########################################################################
  # DT1 <- DT_Master
  # 
  # DT1[forecastHorizon %in% 1:2, HorizonBin := "Horizon: 1-2 Years"]
  # DT1[forecastHorizon %in% 3:4, HorizonBin := "Horizon: 3-4 Years"]
  # DT1[forecastHorizon %in% 5:6, HorizonBin := "Horizon: 5-6 Years"]
  # DT1[forecastHorizon %in% 7:8, HorizonBin := "Horizon: 7-8 Years"]
  # DT1[forecastHorizon %in% 9:10, HorizonBin := "Horizon: 9-10 Years"]
  # DT1[forecastHorizon %in% 11:12, HorizonBin := "Horizon: 11-12 Years"]
  # 
  # # Factor Ordering
  # DT1[, HorizonBin := factor(HorizonBin,
  #                                       levels = c("Horizon: 1-2 Years",
  #                                                  "Horizon: 3-4 Years",
  #                                                  "Horizon: 5-6 Years",
  #                                                  "Horizon: 7-8 Years",
  #                                                  "Horizon: 9-10 Years",
  #                                                  "Horizon: 11-12 Years"))]
  # 
  # names(DT1)[names(DT1) == "numberOfCovariates"] <- "Complexity"
  # 
  # # Calculate Median
  # medianDt <- DT1[, .(mu = median(loss, na.rm = TRUE)), 
  #                by = .(scenario, Complexity, HorizonBin)] #HorizonBin
  # 
  # qDT <- DT1[, .(
  #   q25 = quantile(loss, 0.25, na.rm = TRUE),
  #   q50 = quantile(loss, 0.50, na.rm = TRUE),  # median (you already have this)
  #   q75 = quantile(loss, 0.75, na.rm = TRUE)
  # ), by = .(scenario, HorizonBin, Complexity)]
  # 
  # # 4. Create the Grid Plot (originally without bins)
  # pFinalGridBinned <- ggplot(DT1, aes(x = loss)) +
  #   # Filled Density Forms
  #   geom_density(aes(fill = scenario, color = scenario), alpha = 0.3, linewidth = 0.5) +
  #   # IQR shaded band
  #   geom_rect(
  #     data = qDT,
  #     aes(xmin = q25, xmax = q75, ymin = -Inf, ymax = Inf, fill = scenario),
  #     inherit.aes = FALSE,
  #     alpha = 0.15
  #   ) +
  #   # scenario Median Lines
  #   geom_vline(data = medianDt, aes(xintercept = mu, color = scenario), 
  #              linetype = "dashed", linewidth = 0.7) +
  #   # # Staggered Annotations (to prevent overlap)
  #   # geom_text(data = meansDt,
  #   #           aes(x = mu, y = Inf, label = round(mu, 3), color = scenario, vjust = vPos),
  #   #           size = 3, fontface = "bold") +
  #   # Random Baseline Reference (2.40)
  #   geom_vline(xintercept = 2.3979, linetype = "dotted", color = "black", linewidth = 0.8) +
  #   
  #   # THE GRID
  #   facet_grid(Complexity ~ HorizonBin, scales = "free_y") +
  #   
  #   # Aesthetics
  #   scale_fill_manual(values = c("Internal" = "#4daf4a", "FutureTainted" = "#377eb8", "FutureUnseen" = "#e41a1c"),
  #                     labels = c(
  #                       "Cross-validation",
  #                       "Forecast with cross-validated model",
  #                       "Forecast with predictive validated model"
  #                     )) +
  #   scale_color_manual(values = c("Internal" = "#4daf4a", "FutureTainted" = "#377eb8", "FutureUnseen" = "#e41a1c"),
  #                      labels = c(
  #                        "Cross-validation",
  #                        "Forecast with cross-validated model",
  #                        "Forecast with predictive validated model"
  #                      )) +
  #   
  #   theme_minimal() +
  #   coord_cartesian(xlim = c(2.2, 2.63)) + 
  #   
  #   labs(
  #     title = "Predictive vs. Cross validation for different forecast horizons and model complexity (median)",
  #     x = "Prediction Loss (Lower is better)",
  #     y = "Density (Strata)"
  #   ) +
  #   
  #   theme(
  #     legend.position = "bottom",
  #     strip.text = element_text(face = "bold", size = 11),
  #     panel.spacing = unit(1, "lines"),
  #     plot.title = element_text(face = "bold", size = 14),
  #     panel.grid.minor = element_blank()
  #   )
  # 
  # # Display result
  # pFinalGridBinned
  # }
  # ### HOMOGENEOUS SAMPLES
  # { 
  # # Plot 7. Look at it also from a sample size perspective!
  # DTplot <- unique(DT1[, .(groupId, Complexity, totalSamples, forecastHorizon)])
  # ggplot(DTplot, aes(x = forecastHorizon, y = totalSamples, color = Complexity)) +
  #   geom_point(alpha = 0.6) +
  #   geom_smooth(se = FALSE) +
  #   theme_minimal()
  # 
  # ggplot(DTplot, aes(x = forecastHorizon, y = Complexity, fill = totalSamples)) +
  #   geom_tile() +
  #   scale_fill_viridis_c() +
  #   theme_minimal()
  # 
  # ggplot(DTplot, aes(x = forecastHorizon, y = totalSamples)) +
  #   geom_point(alpha = 0.5) +
  #   facet_wrap(~ Complexity) +
  #   theme_minimal()
  # 
  # homogSamples <- DT1[totalSamples > 38992 & totalSamples < 42209 ,]
  # 
  # # Calculate Means and Staggered Annotation Positions
  # meansDthom <- homogSamples[, .(mu = mean(loss, na.rm = TRUE)), 
  #                by = .(scenario, Complexity, HorizonBin)] #HorizonBin
  # meansDthom[scenario == "Internal", vPos := 1.5]
  # meansDthom[scenario == "FutureTainted", vPos := 3.5]
  # meansDthom[scenario == "FutureUnseen", vPos := 5.5]
  # 
  # # 4. Create the Grid Plot (originally without bins)
  # gridBinnedHom <- ggplot(homogSamples, aes(x = loss)) +
  #   # Filled Density Forms
  #   geom_density(aes(fill = scenario, color = scenario), alpha = 0.3, linewidth = 0.5) +
  #   # scenario Mean Lines
  #   geom_vline(data = meansDthom, aes(xintercept = mu, color = scenario), 
  #              linetype = "dashed", linewidth = 0.7) +
  #    geom_vline(xintercept = 2.3979, linetype = "dotted", color = "black", linewidth = 0.8) +
  #   
  #   # THE GRID
  #   facet_grid(Complexity ~ HorizonBin, scales = "free_y") +
  #   
  #   # Aesthetics
  #   scale_fill_manual(values = c("Internal" = "#4daf4a", "FutureTainted" = "#377eb8", "FutureUnseen" = "#e41a1c"),
  #                     labels = c(
  #                       "Cross-validation",
  #                       "Forecast with cross-validated model",
  #                       "Forecast with predictive validated model"
  #                     )) +
  #   scale_color_manual(values = c("Internal" = "#4daf4a", "FutureTainted" = "#377eb8", "FutureUnseen" = "#e41a1c"),
  #                      labels = c(
  #                        "Cross-validation",
  #                        "Forecast with cross-validated model",
  #                        "Forecast with predictive validated model"
  #                      )) +
  #   
  #   theme_minimal() +
  #   coord_cartesian(xlim = c(1.9, 3.1)) + 
  #   
  #   labs(
  #     title = "Predictive vs. Cross validation for different forecast horizons and model complexity",
  #     x = "Prediction Loss (Lower is better)",
  #     y = "Density (Strata)"
  #   ) +
  #   
  #   theme(
  #     legend.position = "bottom",
  #     strip.text = element_text(face = "bold", size = 11),
  #     panel.spacing = unit(1, "lines"),
  #     plot.title = element_text(face = "bold", size = 14),
  #     panel.grid.minor = element_blank()
  #   )
  # 
  # # Display result
  # gridBinnedHom
  # }
  # 
  # ########################################################################
  # # PLOT 2: Density of loss for each caribou decision for all scenarios  #
  # # This plot focuses on the horizon. However, we have probably a strong #
  # # effect of amount of data that needs to be taken into account.        #
  # # 1. Prepare Data and Define Horizon Bins                              #
  # ########################################################################
  # 
  # DT1path <- file.path(outPath, "DT1.csv")
  # 
  # if (file.exists(DT1path)){
  #   print(paste0("DT1 found! Loading from file... "))
  #   DT1 <- fread(DT1path)
  # } else {
  #   print(paste0("DT1 not found... "))
  #   DT1 <- rbindlist(lapply(1:NROW(fittedTable), function(index){
  #     dt <- data.table(
  #       groupId = fittedTable[index, groupId],
  #       numberOfCovariates = fittedTable[index, numberOfCovariates],
  #       forecastHorizon = fittedTable[index, forecastHorizon],
  #       totalSamples = fittedTable[index, totalSamples],
  #       scenario = fittedTable[index, typeValidation],
  #       loss = readRDS(fittedTable[index, rawLossPath])
  #     )
  #     return(dt)
  #   }), use.names = TRUE)
  #   fwrite(DT1, DT1path)
  # }
  # 
  # DT1[, scenario := factor(scenario, 
  #                          levels = c("Internal", "FutureTainted", "FutureUnseen"))]
  # DT1[, numberOfCovariates := factor(numberOfCovariates, 
  #                                    levels = c("2", "5", 
  #                                               "10", "30"))]
  # 
  # DT1[forecastHorizon %in% 1:2, HorizonBin := "Horizon: 1-2 Years"]
  # DT1[forecastHorizon %in% 3:4, HorizonBin := "Horizon: 3-4 Years"]
  # DT1[forecastHorizon %in% 5:6, HorizonBin := "Horizon: 5-6 Years"]
  # DT1[forecastHorizon %in% 7:8, HorizonBin := "Horizon: 7-8 Years"]
  # DT1[forecastHorizon %in% 9:10, HorizonBin := "Horizon: 9-10 Years"]
  # DT1[forecastHorizon %in% 11:12, HorizonBin := "Horizon: 11-12 Years"]
  # 
  # # Factor Ordering
  # DT1[, HorizonBin := factor(HorizonBin,
  #                            levels = c("Horizon: 1-2 Years",
  #                                       "Horizon: 3-4 Years",
  #                                       "Horizon: 5-6 Years",
  #                                       "Horizon: 7-8 Years",
  #                                       "Horizon: 9-10 Years",
  #                                       "Horizon: 11-12 Years"))]
  # 
  # names(DT1)[names(DT1) == "numberOfCovariates"] <- "Complexity"
  # 
  # # Calculate Median
  # medianDt <- DT1[, .(mu = median(loss, na.rm = TRUE)), 
  #                 by = .(scenario, Complexity, HorizonBin)] #HorizonBin
  # 
  # qDT <- DT1[, .(
  #   q25 = quantile(loss, 0.25, na.rm = TRUE),
  #   q50 = quantile(loss, 0.50, na.rm = TRUE),  # median (you already have this)
  #   q75 = quantile(loss, 0.75, na.rm = TRUE)
  # ), by = .(scenario, HorizonBin, Complexity)]
  # 
  # # 4. Create the Grid Plot (originally without bins)
  # pFinalGridBinned <- ggplot(DT1, aes(x = loss)) +
  #   # Filled Density Forms
  #   geom_density(aes(fill = scenario, color = scenario), alpha = 0.3, linewidth = 0.5) +
  #   # IQR shaded band
  #   geom_rect(
  #     data = qDT,
  #     aes(xmin = q25, xmax = q75, ymin = -Inf, ymax = Inf, fill = scenario),
  #     inherit.aes = FALSE,
  #     alpha = 0.15
  #   ) +
  #   # scenario Median Lines
  #   geom_vline(data = medianDt, aes(xintercept = mu, color = scenario), 
  #              linetype = "dashed", linewidth = 0.7) +
  #   # # Staggered Annotations (to prevent overlap)
  #   # geom_text(data = meansDt,
  #   #           aes(x = mu, y = Inf, label = round(mu, 3), color = scenario, vjust = vPos),
  #   #           size = 3, fontface = "bold") +
  #   # Random Baseline Reference (2.40)
  #   geom_vline(xintercept = 2.3979, linetype = "dotted", color = "black", linewidth = 0.8) +
  #   
  #   # THE GRID
  #   facet_grid(Complexity ~ HorizonBin, scales = "free_y") +
  #   
  #   # Aesthetics
  #   scale_fill_manual(values = c("Internal" = "#4daf4a", "FutureTainted" = "#377eb8", "FutureUnseen" = "#e41a1c"),
  #                     labels = c(
  #                       "Cross-validation",
  #                       "Forecast with cross-validated model",
  #                       "Forecast with predictive validated model"
  #                     )) +
  #   scale_color_manual(values = c("Internal" = "#4daf4a", "FutureTainted" = "#377eb8", "FutureUnseen" = "#e41a1c"),
  #                      labels = c(
  #                        "Cross-validation",
  #                        "Forecast with cross-validated model",
  #                        "Forecast with predictive validated model"
  #                      )) +
  #   
  #   theme_minimal() +
  #   coord_cartesian(xlim = c(2.2, 2.63)) + 
  #   
  #   labs(
  #     title = "Predictive vs. Cross validation for different forecast horizons and model complexity (median)",
  #     x = "Prediction Loss (Lower is better)",
  #     y = "Density (Strata)"
  #   ) +
  #   
  #   theme(
  #     legend.position = "bottom",
  #     strip.text = element_text(face = "bold", size = 11),
  #     panel.spacing = unit(1, "lines"),
  #     plot.title = element_text(face = "bold", size = 14),
  #     panel.grid.minor = element_blank()
  #   )
  # 
  # # Display result
  # pFinalGridBinned
  # 

  return(list(P11 = P1.1,
              P12 = P1.2,
              P21 = P2.1,
              P31 = P3.1))
}


