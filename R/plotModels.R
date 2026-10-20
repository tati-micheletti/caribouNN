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
    
    fittedTable[typeValidation == "FutureUnseen", valStartYear := trainEndYear+1]
    fittedTable[typeValidation == "FutureUnseen", valEndYear := trainEndYear+1]
    fittedTable[typeValidation == "FutureUnseen", testEndYear := testStartYear]
    
    fittedTable[typeValidation == "FutureTainted", valStartYear := trainStartYear]
    fittedTable[typeValidation == "FutureTainted", valEndYear := trainEndYear]
    fittedTable[typeValidation == "FutureTainted", testEndYear := testStartYear]
    
    fittedTable[typeValidation == "Internal", valStartYear := trainStartYear]
    fittedTable[typeValidation == "Internal", valEndYear := trainEndYear]
    fittedTable[typeValidation == "Internal", testEndYear := trainEndYear]
    
    # Adding forecast horizon 
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
    
    DTmaster_path <- file.path(outPath, "DT_master.csv")
    fwrite(DT_Master, DTmaster_path)
  }
  
  # =========================================================================
  # MODIFICATION 1: FILTERING OUT PRE-2013 DATA
  # We drop any group where the training started before 2013 due to bogus 
  # fire and harvest distribution data.
  # =========================================================================
  print("Filtering out pre-2012 data...")
  DT_Master <- DT_Master[trainStartYear >= 2012]
  
  
  # STEP 1: Summarize to the Experiment Level
  dt_exp <- DT_Master[, .(med_loss = median(loss)), 
                      by = .(groupId, Complexity, scenario, forecastHorizon)]
  
  # STEP 2: Parse the groupId string to get the experimental parameters
  dt_exp[, c("Prefix", "Complexity", "StartYear", "HistoryEnd", "TargetYear") := 
           tstrsplit(groupId, "_", type.convert = TRUE)]
  dt_exp[Complexity == Inf, Complexity := 30]
  
  # Remove prefix and set Complexity as a factor
  dt_exp[, Complexity := factor(Complexity, levels = c(2, 5, 10, 30))]
  dt_exp[, TimeSpan := (TargetYear - StartYear + 1)]
  
  # STEP 3: Pivot to Wide (The Paired Table)
  dt_paired <- dcast(dt_exp, 
                     groupId + Complexity + StartYear + forecastHorizon + HistoryEnd + TargetYear + TimeSpan ~ scenario, 
                     value.var = "med_loss")
  
  # STEP 4: Calculate the Paired Deltas (The Evidence)
  dt_paired[, GenGap := FutureUnseen - Internal]
  dt_paired[, PreValAdvantage := FutureTainted - FutureUnseen]
  dt_paired[, OptimismBiasPct := (FutureTainted - Internal) / Internal * 100]
  
  dt_paired[forecastHorizon %in% 1:2, HorizonBin := "Horizon: 1-2 Years"]
  dt_paired[forecastHorizon %in% 3:4, HorizonBin := "Horizon: 3-4 Years"]
  dt_paired[forecastHorizon %in% 5:6, HorizonBin := "Horizon: 5-6 Years"]
  dt_paired[forecastHorizon %in% 7:8, HorizonBin := "Horizon: 7-8 Years"]
  dt_paired[forecastHorizon %in% 9:10, HorizonBin := "Horizon: 9-10 Years"]
  dt_paired[forecastHorizon %in% 11:12, HorizonBin := "Horizon: 11-12 Years"]
  
  dt_paired[, HorizonBin := factor(HorizonBin,
                                   levels = c("Horizon: 1-2 Years",
                                              "Horizon: 3-4 Years",
                                              "Horizon: 5-6 Years",
                                              "Horizon: 7-8 Years",
                                              "Horizon: 9-10 Years",
                                              "Horizon: 11-12 Years"))]
  
  # Setting 1: Plot A: The Overfitting Story (H1)
  P1.1 <- ggplot(dt_paired, 
                 aes(x = TimeSpan, y = OptimismBiasPct, color = Complexity, fill = Complexity)) +
    geom_point(alpha = 0.6, position = position_jitter(width = 0.2), size = 1) +
    stat_summary(fun.data = median_hilow, geom = "ribbon", alpha = 0.15, color = NA) +
    stat_summary(fun = median, geom = "line", linewidth = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.6) +
    facet_grid(~ Complexity, labeller = label_both) +
    scale_color_viridis_d() + 
    scale_fill_viridis_d() +
    theme_minimal() +
    coord_cartesian(ylim = c(0, 12)) +
    labs(title = "Model's Optimism Bias",
         x = "Total Years of Data (TimeSpan)", 
         y = "Optimism Bias (% Increase in Error)",
         color = "Model Complexity (No. covariates)",
         fill = "Model Complexity (No. covariates)") +
    theme(legend.position = "bottom",
          strip.text = element_blank(),
          panel.grid.minor = element_blank(),
          text = element_text(size = 12),
          plot.title = element_text(face = "bold", size = 14))
  
  # Plot B: The PreVal Advantage (H2) Density
  DT1 <- DT_Master
  
  DT1[forecastHorizon %in% 1:2, HorizonBin := "Horizon: 1-2 Years"]
  DT1[forecastHorizon %in% 3:4, HorizonBin := "Horizon: 3-4 Years"]
  DT1[forecastHorizon %in% 5:6, HorizonBin := "Horizon: 5-6 Years"]
  DT1[forecastHorizon %in% 7:8, HorizonBin := "Horizon: 7-8 Years"]
  DT1[forecastHorizon %in% 9:10, HorizonBin := "Horizon: 9-10 Years"]
  DT1[forecastHorizon %in% 11:12, HorizonBin := "Horizon: 11-12 Years"]
  
  DT1[, HorizonBin := factor(HorizonBin,
                             levels = c("Horizon: 1-2 Years",
                                        "Horizon: 3-4 Years",
                                        "Horizon: 5-6 Years",
                                        "Horizon: 7-8 Years",
                                        "Horizon: 9-10 Years",
                                        "Horizon: 11-12 Years"))]
  
  medianDt <- DT1[, .(mu = median(loss, na.rm = TRUE)),
                  by = .(scenario, Complexity, HorizonBin)] 
  
  qDT <- DT1[, .(
    q25 = quantile(loss, 0.25, na.rm = TRUE),
    q75 = quantile(loss, 0.75, na.rm = TRUE)
  ), by = .(scenario, HorizonBin, Complexity)]
  
  P1.2 <- ggplot(DT1, aes(x = loss)) +
    geom_density(aes(fill = scenario, color = scenario), alpha = 0.3, linewidth = 0.5) +
    geom_rect(data = qDT, aes(xmin = q25, xmax = q75, ymin = -Inf, ymax = Inf, fill = scenario),
              inherit.aes = FALSE, alpha = 0.15) +
    geom_vline(data = medianDt, aes(xintercept = mu, color = scenario),
               linetype = "dashed", linewidth = 0.7) +
    geom_vline(xintercept = 2.3979, linetype = "dotted", color = "black", linewidth = 0.8) +
    facet_grid(Complexity ~ HorizonBin) +
    scale_fill_manual(values = c("Internal" = "#4daf4a", "FutureTainted" = "#377eb8", "FutureUnseen" = "#e41a1c"),
                      labels = c("Internal" = "Cross-validation", "FutureTainted" = "Forecast with cross-validated model", "FutureUnseen" = "Forecast with predictive validated model")) +
    scale_color_manual(values = c("Internal" = "#4daf4a", "FutureTainted" = "#377eb8", "FutureUnseen" = "#e41a1c"),
                       labels = c("Internal" = "Cross-validation", "FutureTainted" = "Forecast with cross-validated model", "FutureUnseen" = "Forecast with predictive validated model")) +
    theme_minimal() +
    coord_cartesian(xlim = c(2.2, 2.63)) +
    labs(title = "Predictive vs. Cross validation", 
         x = "Prediction Loss (Lower is better)", y = "Density (Strata)") +
    theme(legend.position = "bottom", text = element_text(size = 12),
          strip.text = element_text(face = "bold", size = 12), panel.spacing = unit(1, "lines"),
          plot.title = element_text(face = "bold", size = 14), panel.grid.minor = element_blank())
  
  # Variance Inflation Plot
  dt_disp <- DT_Master[, .(
    sd_loss  = sd(loss, na.rm = TRUE),
    iqr_loss = IQR(loss, na.rm = TRUE),
    n_obs    = .N
  ), by = .(groupId, Complexity, scenario, forecastHorizon)]
  
  dt_disp_wide <- dcast(dt_disp,
                        groupId + factor(Complexity, levels = c(2,5,10,30)) + forecastHorizon ~ scenario,
                        value.var = c("sd_loss", "iqr_loss"))
  
  dt_disp_wide[, VI_StatusQuo := sd_loss_FutureTainted / sd_loss_Internal]
  dt_disp_wide[, VI_PreVal    := sd_loss_FutureUnseen  / sd_loss_Internal]
  
  dt_vi_long <- melt(dt_disp_wide,
                     id.vars = c("groupId", "Complexity", "forecastHorizon"),
                     measure.vars = c("VI_StatusQuo", "VI_PreVal"),
                     variable.name = "Method", value.name = "VarianceInflation")
  
  P1.3 <- ggplot(dt_vi_long, aes(x = Complexity, y = VarianceInflation, fill = Method)) +
    geom_boxplot(alpha = 0.6, outlier.alpha = 0.3) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    scale_fill_manual(values = c("VI_StatusQuo" = "#377eb8", "VI_PreVal" = "#e41a1c")) +
    theme_minimal() +
    labs(title = "Variance Inflation Relative to the Internal Baseline",
         subtitle = "Values > 1 mean the real forecast is noisier than Internal suggested",
         x = "Model Complexity (Covariates)", y = "SD(loss) / SD(loss)_Internal")
  
  # Setting 2: PreVal Advantage for All Years
  P2.1 <- ggplot(dt_paired, aes(x = TargetYear, y = PreValAdvantage, color = Complexity, fill = Complexity)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.6) +
    stat_summary(fun.data = median_hilow, geom = "ribbon", alpha = 0.15, color = NA) +
    stat_summary(fun = median, geom = "line", linewidth = 1.2) +
    stat_summary(fun = median, geom = "point", size = 2) +
    scale_x_continuous(breaks = seq(min(dt_paired$TargetYear), max(dt_paired$TargetYear), by = 2)) +
    facet_grid(.~Complexity) +
    scale_color_viridis_d() + scale_fill_viridis_d() +
    theme_minimal() +
    labs(title = "PreVal Advantage for All Years)",
         x = "Forecast Target Year", y = "Paired Delta Loss (Advantage)") +
    theme(legend.position = "none", text = element_text(size = 11),
          strip.text = element_text(face = "bold"), panel.grid.minor = element_blank())
  
  # Setting 3: The Profitability Map: The PreVal 'Stress Test'
  dt_paired[, HistoryBudget := (HistoryEnd - StartYear + 1)]
  
  dt_stress_agg <- dt_paired[, .(
    med_Advantage = median(PreValAdvantage, na.rm = TRUE)
  ), by = .(Complexity, HistoryBudget, forecastHorizon)]
  
  P3.1 <- ggplot(dt_stress_agg, aes(x = HistoryBudget, y = forecastHorizon, fill = med_Advantage)) +
    geom_tile(color = "white", linewidth = 0.2) + 
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, name = "PreVal\nAdvantage") +
    facet_grid(~Complexity) +
    scale_y_continuous(breaks = seq(1, 12, by = 1)) +
    scale_x_continuous(breaks = seq(2, max(dt_stress_agg$HistoryBudget), by = 2)) +
    theme_minimal() +
    labs(title = "The Profitability Map: The PreVal 'Stress Test'",
         subtitle = "Blue = PreVal wins despite having one less year of training data than the Status Quo.",
         x = "Years of Historical Data (History Budget)", y = "Years into the Future (Forecast Horizon)") +
    theme(strip.text = element_text(face = "bold", size = 12), panel.grid.minor = element_blank())
  
  # Validation Year Quality Across Time
  dt_paired[, HorizonBlock := fcase(
    forecastHorizon <= 3, "Horizon: 1-3y",
    forecastHorizon > 3 & forecastHorizon <= 6, "Horizon: 4-6y",
    forecastHorizon > 6 & forecastHorizon <= 9, "Horizon: 7-9y",
    forecastHorizon > 9, "Horizon: 10-12y"
  )]
  
  dt_paired[, HorizonBlock := factor(HorizonBlock, levels = c("Horizon: 1-3y", "Horizon: 4-6y", "Horizon: 7-9y", "Horizon: 10-12y"))]
  
  dt_anchor_stress <- dt_paired[, .(
    med_Adv = median(PreValAdvantage, na.rm = TRUE),
    iqr_lower = quantile(PreValAdvantage, 0.25, na.rm = TRUE),
    iqr_upper = quantile(PreValAdvantage, 0.75, na.rm = TRUE),
    n_exp = .N
  ), by = .(HistoryEnd, Complexity, HorizonBlock)]
  
  dt_anchor_filtered <- dt_anchor_stress[n_exp >= 2]
  
  P3.2 <- ggplot(dt_anchor_filtered, aes(x = as.factor(HistoryEnd), y = med_Adv, fill = med_Adv > 0)) +
    geom_bar(stat = "identity", color = "black", alpha = 0.8) +
    geom_errorbar(aes(ymin = iqr_lower, ymax = iqr_upper), width = 0.3, alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
    facet_grid(HorizonBlock ~ Complexity, scales = "free_y") +
    scale_fill_manual(values = c("TRUE" = "#377eb8", "FALSE" = "#e41a1c"), guide = "none") +
    coord_cartesian(ylim = c(-0.1, 0.1)) +
    theme_minimal() +
    labs(title = "Validation Year Quality Across Time",
         x = "Year Used for Validation", y = "Median PreVal Advantage") +
    theme(strip.text = element_text(face = "bold", size = 11),
          axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.minor = element_blank())
  
  # Global Year Difficulty Plot
  year_stats_global <- dt_paired[, .(
    med_loss = median(FutureTainted),
    q25_loss = quantile(FutureTainted, 0.25),
    q75_loss = quantile(FutureTainted, 0.75),
    med_bias = median(OptimismBiasPct)
  ), by = .(TargetYear)]
  
  P4.1 <- ggplot(year_stats_global, aes(x = TargetYear)) +
    geom_ribbon(aes(ymin = q25_loss, ymax = q75_loss), alpha = 0.15, fill = "darkblue") +
    geom_line(aes(y = med_loss), linewidth = 1) +
    geom_point(aes(y = med_loss, size = med_bias, color = med_bias)) +
    scale_color_viridis_c(option = "magma", name = "Optimism Bias %") +
    scale_size_continuous(name = "Optimism Bias %") + 
    scale_x_continuous(breaks = seq(min(year_stats_global$TargetYear), max(year_stats_global$TargetYear), by = 1)) +
    theme_minimal() +
    labs(title = "Global Year Difficulty (Aggregate of All Horizons)",
         y = "Median Prediction Loss", x = "Target Year") +
    theme(legend.position = "right", plot.title = element_text(face = "bold", size = 14),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Synchronization of Risk
  dt_cor <- dt_paired[, .(
    med_loss = median(FutureTainted), 
    med_bias = median(OptimismBiasPct)
  ), by = .(TargetYear)]
  
  cor_val <- round(cor(dt_cor$med_loss, dt_cor$med_bias), 3)
  
  P4.2 <- ggplot(dt_cor, aes(x = med_bias, y = med_loss)) +
    geom_smooth(method = "lm", color = "black", linetype = "dashed", alpha = 0.1) +
    geom_point(size = 4, aes(color = TargetYear)) + 
    geom_text_repel(aes(label = TargetYear), size = 4, box.padding = 1.2, point.padding = 0.5, force = 10, segment.color = "grey50", segment.alpha = 0.6, min.segment.length = 0) +
    annotate("label", x = min(dt_cor$med_bias), y = max(dt_cor$med_loss), label = paste0("Pearson's r = ", cor_val), fill = "white", fontface = "bold", size = 6, hjust = 0) +
    scale_color_viridis_c(option = "magma") + 
    theme_minimal() +
    labs(title = "Loss vs. Deception",
         x = "Optimism Bias (% Underestimation of Error)", y = "Median Prediction Loss Cross-Validated Forecast") +
    theme(legend.position = "none", plot.title = element_text(face = "bold", size = 16), text = element_text(size = 12), axis.title = element_text(face = "bold", size = 12), panel.grid.minor = element_blank())
  
  # Tracking Individual Experiments
  dt_individual <- dt_paired[HistoryBudget == 3]
  dt_individual[, Exp_Label := paste0("Hist: ", StartYear, "-", HistoryEnd, " (Val: ", HistoryEnd, ")")]
  
  P6.1 <- ggplot(dt_individual, aes(x = forecastHorizon, y = PreValAdvantage, color = Exp_Label)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
    geom_line(linewidth = 1, alpha = 0.8) + geom_point(size = 2) +
    facet_grid(~Complexity, scales = "free_y") +
    scale_color_viridis_d(option = "turbo", name = "Specific Experiment") +
    theme_minimal() +
    scale_x_continuous(breaks = 1:12) +
    labs(title = "The Lifespan of Individual Experiments (3-Year History Budget)",
         subtitle = "Tracking single (N=1) experiments over time. Do they all eventually crash?",
         x = "Years into the Future (Forecast Horizon)", y = "PreVal Advantage (Blue/Above 0 = PreVal Wins)") +
    theme(legend.position = "right", strip.text = element_text(face = "bold", size = 12), panel.grid.minor = element_blank())
  
  
  # =========================================================================
  # MODIFICATION 2: ABSOLUTE PREDICTIVE POWER PLOTS (SLIDE 5 "BAMM" TRANSITION)
  # =========================================================================
  
  # 1. Filter for operational forecasting window (1 to 5 years)
  # dt_abs_perf <- dt_paired[forecastHorizon <= 5]
  dt_abs_perf <- dt_paired[]
  
  # 2. Calculate Median and IQR for Status Quo (Tainted)
  dt_sq <- dt_abs_perf[, .(
    Method = "Status Quo (Standard CV)",
    Med_Loss = median(FutureTainted, na.rm = TRUE),
    Q25 = quantile(FutureTainted, 0.25, na.rm = TRUE),
    Q75 = quantile(FutureTainted, 0.75, na.rm = TRUE)
  ), by = .(Complexity)]
  
  # 3. Calculate Median and IQR for PreVal (Unseen)
  dt_pv <- dt_abs_perf[, .(
    Method = "PreVal (Temporal Block)",
    Med_Loss = median(FutureUnseen, na.rm = TRUE),
    Q25 = quantile(FutureUnseen, 0.25, na.rm = TRUE),
    Q75 = quantile(FutureUnseen, 0.75, na.rm = TRUE)
  ), by = .(Complexity)]
  
  # 4. Combine into a single plotting table and make Complexity numeric
  dt_plot_abs <- rbind(dt_sq, dt_pv)
  dt_plot_abs[, Comp_Num := as.numeric(as.character(Complexity))]
  
  # Calculate universal Y limits so the plot doesn't jump during the slide transition
  y_limits <- c(min(dt_plot_abs$Q25, na.rm = TRUE), max(dt_plot_abs$Q75, na.rm = TRUE))
  
  # 5a. PLOT: BLUE ONLY (Status Quo)
  P5.1_BlueOnly <- ggplot(dt_plot_abs[Method == "Status Quo (Standard CV)"], 
                          aes(x = Comp_Num, y = Med_Loss, color = Method, fill = Method)) +
    geom_ribbon(aes(ymin = Q25, ymax = Q75), alpha = 0.15, color = NA) +
    geom_line(linewidth = 1.5) +
    geom_point(size = 4) +
    scale_color_manual(values = c("Status Quo (Standard CV)" = "#377eb8")) +
    scale_fill_manual(values = c("Status Quo (Standard CV)" = "#377eb8")) +
    scale_x_continuous(breaks = c(2, 5, 10, 30)) +
    coord_cartesian(ylim = y_limits) + # Lock the Y-axis to match the Both plot
    theme_minimal() +
    labs(#title = "Absolute Predictive Power: Defeating the Bias-Variance Tradeoff",
         # subtitle = "Status Quo overfits (U-shape).",
         x = "Model Complexity (Number of Covariates)",
         # y = "Median Absolute Prediction Loss (Horizons 1-5)") +
         y = "Median Absolute Prediction Loss") +
    theme(legend.position = "bottom", legend.title = element_blank(),
          legend.text = element_text(size = 12), plot.title = element_text(face = "bold", size = 16),
          axis.title = element_text(face = "bold", size = 12), axis.text = element_text(size = 11),
          panel.grid.minor.x = element_blank())
  
  # 5b. PLOT: BOTH (The BAMM Moment)
  P5.1_Both <- ggplot(dt_plot_abs, aes(x = Comp_Num, y = Med_Loss, color = Method, fill = Method)) +
    geom_ribbon(aes(ymin = Q25, ymax = Q75), alpha = 0.15, color = NA) +
    geom_line(linewidth = 1.5) +
    geom_point(size = 4) +
    scale_color_manual(values = c("Status Quo (Standard CV)" = "#377eb8", "PreVal (Temporal Block)" = "#e41a1c")) +
    scale_fill_manual(values = c("Status Quo (Standard CV)" = "#377eb8", "PreVal (Temporal Block)" = "#e41a1c")) +
    scale_x_continuous(breaks = c(2, 5, 10, 30)) +
    coord_cartesian(ylim = y_limits) + # Locked Y-axis
    theme_minimal() +
    labs(#title = "Absolute Predictive Power: Defeating the Bias-Variance Tradeoff",
         # subtitle = "Status Quo overfits (U-shape), PreVal redeems complexity.",
         x = "Model Complexity (Number of Covariates)",
         # y = "Median Absolute Prediction Loss (Horizons 1-5)") +
         y = "Median Absolute Prediction Loss") +
    theme(legend.position = "bottom", legend.title = element_blank(),
          legend.text = element_text(size = 12), plot.title = element_text(face = "bold", size = 16),
          axis.title = element_text(face = "bold", size = 12), axis.text = element_text(size = 11),
          panel.grid.minor.x = element_blank())
  
  
  # The 'Decline to Random' Trajectory
  dt_abs_decay <- dt_paired[, .(
    med_Loss_PreVal = median(FutureUnseen, na.rm = TRUE),
    med_Loss_StatusQuo = median(FutureTainted, na.rm = TRUE)
  ), by = .(Complexity, forecastHorizon)]
  
  dt_abs_decay_melt <- melt(dt_abs_decay, id.vars = c("Complexity", "forecastHorizon"), 
                            measure.vars = c("med_Loss_PreVal", "med_Loss_StatusQuo"),
                            variable.name = "Method", value.name = "Absolute_Loss")
  
  P7.1 <- ggplot(dt_abs_decay_melt, aes(x = forecastHorizon, y = Absolute_Loss, color = Method)) +
    geom_smooth(method = "loess", span = 0.6, se = FALSE, linewidth = 1.5) +
    geom_point(size = 2, alpha = 0.5) +
    facet_grid(~Complexity, scales = "free_y") +
    scale_color_manual(values = c("med_Loss_PreVal" = "#e41a1c", "med_Loss_StatusQuo" = "#377eb8"),
                       labels = c("PreVal (Red)", "Status Quo (Blue)")) +
    scale_x_continuous(breaks = 1:12) +
    theme_minimal() +
    labs(title = "The 'Decline to Random' Hypothesis",
         subtitle = "Do both models eventually hit a ceiling of maximum error at extreme horizons?",
         x = "Forecast Horizon (Years into the Future)", y = "Median Absolute Prediction Loss (Higher = Worse)") +
    theme(legend.position = "bottom", strip.text = element_text(face = "bold", size = 12), plot.title = element_text(face = "bold", size = 14))
  
  # Robust Profitability Map (n > 3 experiments)
  dt_stress_agg_robust <- dt_paired[, .(
    med_Advantage = median(PreValAdvantage, na.rm = TRUE),
    n_exp = uniqueN(groupId) 
  ), by = .(Complexity, HistoryBudget, forecastHorizon)]
  
  dt_stress_filtered <- dt_stress_agg_robust[n_exp > 3]
  
  P8.1 <- ggplot(dt_stress_filtered, aes(x = HistoryBudget, y = forecastHorizon, fill = med_Advantage)) +
    geom_tile(color = "white", linewidth = 0.2) + 
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, name = "PreVal\nAdvantage") +
    facet_grid(~Complexity) +
    scale_y_continuous(breaks = seq(1, 12, by = 1)) +
    scale_x_continuous(breaks = seq(2, max(dt_stress_filtered$HistoryBudget, na.rm=TRUE), by = 2)) +
    theme_minimal() +
    labs(title = "The Robust Profitability Map (n > 3 experiments)",
         subtitle = "Edges removed to isolate the stable, high-confidence signal.",
         x = "Years of Historical Data (History Budget)", y = "Years into the Future (Forecast Horizon)") +
    theme(strip.text = element_text(face = "bold", size = 12), panel.grid.minor = element_blank(), panel.background = element_rect(fill = "grey95", color = NA))
  
  # Power of Iterative Forecasting
  dt_static <- dt_paired[HistoryEnd == 2011, .(
    med_Loss = median(FutureUnseen, na.rm = TRUE),
    Strategy = "Static Model (Anchored in 2011)"
  ), by = .(TargetYear, Complexity)]
  
  dt_iterative <- dt_paired[forecastHorizon == 2, .(
    med_Loss = median(FutureUnseen, na.rm = TRUE),
    Strategy = "Iterative Model (Rolling 2-Year Horizon)"
  ), by = .(TargetYear, Complexity)]
  
  dt_update_test <- rbind(dt_static, dt_iterative)
  dt_update_test <- dt_update_test[TargetYear >= 2014] 
  
  P9.1 <- ggplot(dt_update_test, aes(x = TargetYear, y = med_Loss, color = Strategy)) +
    geom_line(linewidth = 1.5, alpha = 0.8) + geom_point(size = 3) +
    facet_grid(~Complexity, scales = "free_y") +
    scale_color_manual(values = c("Static Model (Anchored in 2011)" = "#e41a1c", "Iterative Model (Rolling 2-Year Horizon)" = "#4daf4a")) +
    scale_x_continuous(breaks = 2013:2022) +
    theme_minimal() +
    labs(title = "The Power of Iterative Forecasting: Avoiding the Crash",
         subtitle = "Does a continuously updated model (Green) avoid the catastrophic drift of a static model (Red)?",
         x = "Target Year Being Predicted", y = "Absolute Prediction Loss (Lower is Better)") +
    theme(legend.position = "bottom", strip.text = element_text(face = "bold", size = 12), axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Testing of individual leakage
  if (is.null(extractedFeatures)) {
    warning("extractedFeatures not provided. Testing of individual leakage will not be performed", immediate. = TRUE)
    corr <- NA
    fit <- NULL
    overID <- NULL
  } else {
    DT_master2 <- copy(DT_Master)
    design <- unique(DT_master2[, .(groupId, scenario, Complexity, trainStartYear, trainEndYear, testStartYear)])
    
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
    
    overID <- idOverlap[, .(medianPctOverlap = median(pctTestIDsSeenInTrain, na.rm = TRUE), n = .N), by = scenario]
    
    overlap_wide <- dcast(idOverlap, groupId ~ scenario, value.var = "pctTestIDsSeenInTrain")
    setnames(overlap_wide, c("FutureTainted","FutureUnseen"), c("overlap_StatusQuo","overlap_PreVal"))
    overlap_wide[, overlapGap := overlap_StatusQuo - overlap_PreVal]
    
    dt_check <- merge(dt_paired, overlap_wide[, .(groupId, overlap_StatusQuo, overlap_PreVal, overlapGap)], by = "groupId")
    corr <- cor(dt_check$overlapGap, dt_check$PreValAdvantage, use = "complete.obs")
    fit <- lm(PreValAdvantage ~ overlapGap + Complexity, data = dt_check)
  }
  
  # =========================================================================
  # MODIFICATION 3: UPDATED GGSAVE COMMANDS FOR NEW PLOTS
  # =========================================================================
  w <- 12 
  h <- 6.75
  res <- 300 
  
  ggsave(file.path(outPath, "OptimismBias_Faceted.png"), plot = P1.1, width = w, height = h, dpi = res, bg = "white")
  ggsave(file.path(outPath, "Synchronization_Scatter.png"), plot = P4.2, width = 8, height = 6, dpi = res, bg = "white") 
  ggsave(file.path(outPath, "GlobalYearDiff.png"), plot = P4.1, width = 8, height = 6, dpi = res, bg = "white") 
  ggsave(file.path(outPath, "Profitability_Heatmap.png"), plot = P3.1, width = w, height = h, dpi = res, bg = "white")
  ggsave(file.path(outPath, "Impact_Validation_Year.png"), plot = P3.2, width = w, height = h, dpi = res, bg = "white")
  ggsave(file.path(outPath, "PreVal_Advantage_Timeline.png"), plot = P2.1, width = w, height = h, dpi = res, bg = "white")
  ggsave(file.path(outPath, "Density_Grid.png"), plot = P1.2, width = 16, height = 10, dpi = res, bg = "white")
  
  # Saving the two new split absolute power plots
  ggsave(file.path(outPath, "Absolute_Predictive_Power_BlueOnly.png"), plot = P5.1_BlueOnly, width = 10, height = 6, dpi = res, bg = "white")
  ggsave(file.path(outPath, "Absolute_Predictive_Power_BAMM.png"), plot = P5.1_Both, width = 10, height = 6, dpi = res, bg = "white")
  
  ggsave(file.path(outPath, "Check_Ind_Experiments.png"), plot = P6.1, width = w, height = h, dpi = res, bg = "white")
  
  return(list(P11 = P1.1,
              P12 = P1.2,
              P13 = P1.3,
              P21 = P2.1,
              P31 = P3.1,
              P32 = P3.2,
              P41 = P4.1,
              P42 = P4.2,
              P51_BlueOnly = P5.1_BlueOnly, # Added to return list
              P51_Both = P5.1_Both,         # Added to return list
              P61 = P6.1,
              P71 = P7.1,
              P81 = P8.1,
              P91 = P9.1,
              corrLossBias = cor_val,
              percOverlapINDV = overID,
              corrOverlapGapAndPreValAdvtg = corr,
              statisticalAdvantPreVal = summary(fit)))
}
