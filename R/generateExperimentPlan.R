#' @title Generate a Definitive Experimental Design Plan for Forecasting
#'
#' @description
#' This function creates a comprehensive, machine-readable data.table that outlines
#' all combinations for a forecasting experiment. It generates scenarios for both
#' predictive validation (PreVal) and traditional cross-validation (CrossVal),
#' including tests of CV-developed models on future data.
#'
#' @param startYear An integer specifying the first year of available data.
#'   Defaults to `2008`.
#' @param endYear An integer specifying the last year of available data.
#'   Defaults to `2022`.
#' @param numberOfCovariatesList A numeric vector listing the different numbers
#'   of covariates to be tested in the models. Defaults to `c(2, 5, 10, Inf)`.
#' @param outputPath A character string specifying the file path to save the
#'   resulting CSV file. Defaults to `"experimentalDesign.csv"`.
#'
#' @return
#' A data.table containing the full experimental plan. Each row represents a
#' unique model run to be performed. The function also saves this table as a CSV
#' file to the specified `outputPath`.
#'
#' @examples
#' # Run with default settings and save to "experimentalDesign.csv"
#' # myPlan <- generateExperimentPlan()
#'
#' # Run with a shorter time period and fewer covariate options
#' # customPlan <- generateExperimentPlan(startYear = 2010, endYear = 2018,
#' #                                      numberOfCovariatesList = c(5, 10, 15))
#'
#' # Save the output to a different file
#' # generateExperimentPlan(outputPath = "my_custom_plan.csv")
#'
generateExperimentPlan <- function(startYear = 2008,
                                   endYear = 2022,
                                   numberOfCovariatesList = c(2, 5, 10, Inf),
                                   outputPath = "data/experimentalDesign.csv") {
  
  Require::Require("data.table")
  allYears <- startYear:endYear
  resultsList <- list()
  
  for (numCov in numberOfCovariatesList) {
    # windowSize is the size of the 'Past' history (e.g., 2008-2009 = size 2)
    # We need windowSize >= 2 to allow the "Unseen" scenario (Year 1=Train, Year 2=Val)
    for (windowSize in 2:(length(allYears) - 1)) { 
      for (startYearTrain in allYears) {
        endYearTrain <- startYearTrain + windowSize - 1
        if (endYearTrain >= endYear) break
        
        # predictionYear is the 'Test' year
        for (predictionYear in (endYearTrain + 1):endYear) {
          
          # This ID links the 3 levels of honesty for this specific forecasting goal
          groupId <- paste("Grp", numCov, startYearTrain, endYearTrain, predictionYear, sep = "_")
          
          # --- 1. FUTURE UNSEEN (The "Honest" Gold Standard) ---
          # Train: Start to (End - 1) | Val: End | Test: PredictionYear
          resultsList[[length(resultsList) + 1]] <- data.table(
            groupId = groupId,
            typeValidation = "FutureUnseen",
            numberOfCovariates = numCov,
            trainStartYear = startYearTrain,
            trainEndYear   = endYearTrain - 1,
            valStartYear   = endYearTrain,
            valEndYear     = endYearTrain,
            testStartYear  = predictionYear,
            testEndYear    = predictionYear
          )
          
          # --- 2. FUTURE TAINTED (The "Standard Predictive") ---
          # Train/Val: Mixed across Start to End | Test: PredictionYear
          resultsList[[length(resultsList) + 1]] <- data.table(
            groupId = groupId,
            typeValidation = "FutureTainted",
            numberOfCovariates = numCov,
            trainStartYear = startYearTrain,
            trainEndYear   = endYearTrain,
            valStartYear   = startYearTrain, # fitting function will do random 20%
            valEndYear     = endYearTrain,
            testStartYear  = predictionYear,
            testEndYear    = predictionYear
          )
          
          # --- 3. INTERNAL (The "Cheating" Baseline) ---
          # Train/Val/Test: Randomly mixed from the ENTIRE pool (Start to PredictionYear)
          resultsList[[length(resultsList) + 1]] <- data.table(
            groupId = groupId,
            typeValidation = "Internal",
            numberOfCovariates = numCov,
            trainStartYear = startYearTrain,
            trainEndYear   = predictionYear,
            valStartYear   = startYearTrain,
            valEndYear     = predictionYear,
            testStartYear  = startYearTrain,
            testEndYear    = predictionYear
          )
        }
      }
    }
  }
  
  experimentPlan <- rbindlist(resultsList)
  
  # Add metadata columns for your analysis
  experimentPlan[, forecastHorizon := testStartYear - trainEndYear]
  # For Internal, the horizon is 0 because the test data is part of the training window
  experimentPlan[typeValidation == "Internal", forecastHorizon := 0]
  
  cat(sprintf("Generated %d rows across %d complete triplet groups.\n", 
              nrow(experimentPlan), uniqueN(experimentPlan$groupId)))
  
  fwrite(experimentPlan, outputPath)
  return(experimentPlan)
}
