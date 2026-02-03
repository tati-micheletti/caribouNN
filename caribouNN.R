defineModule(sim, list(
  name = "caribouNN",
  description = "Performs an experiment on models with different forecasting settings",
  keywords = "",
  authors = structure(list(list(given = "Tati", family = "Micheletti", role = c("aut", "cre"), email = "tati.micheletti@gmail.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(caribouNN = "0.0.1"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "caribouNN.Rmd"),
  reqdPkgs = list("SpaDES.core (>= 3.0.4)", "ggplot2","data.table", "torch", "luz"),
  parameters = bindrows(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used - e.g., a hash of the study",
                          "area obtained using `reproducible::studyAreaName()`"),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?"),
    defineParameter("epoch", "numeric", 100, 10, 200, 
                    "Epochs for the ranking model (keep low for speed)"),
    defineParameter("batchSize", "numeric", 512, 32, 4096, 
                    "Batch size"),
    defineParameter("learningRate", "numeric", 0.01, 0.001, 0.1, 
                    paste0("Learning rate. The smaller it is, the longer it takes, but the more",
                           " precise to find the best parameters."))#,
    # defineParameter("rerunPrepData", "logical", FALSE, NA, NA, 
    #                 "Should the dataPrep be re-run?"),
    defineParameter("rerunTraining", "logical", FALSE, NA, NA,
                    "Should the training be re-run?")
  ),
  inputObjects = bindrows(
    expectsInput("featurePriority", "character", 
                  "Ordered list of variable names based on importance"),
    expectsInput("preparedDataFinal", "data.table", 
                  paste0("Data table containing Dataset of raw ",
                         "features after preparation (interactions added, etc.) ",
                         "This is generally done by another module."))
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "experimentPlan", objectClass = "data.table", 
                  desc = paste0("Data.table containing the experiment plan, including",
                                " a column with the link to the folder with each model's",
                                " results")),
    createsOutput(objectName = "experimentDatasets", objectClass = "list", 
                  desc = paste0("List of data.tables containing the specific datasets ",
                                " for each experiment planned"))
  )
))

doEvent.caribouNN = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim), "caribouNN", "prepareExperiment")
      sim <- scheduleEvent(sim, time(sim), "caribouNN", "trainExperiment")
      sim <- scheduleEvent(sim, time(sim), "caribouNN", "predictExperiment")
      sim <- scheduleEvent(sim, time(sim), "caribouNN", "compareExperiment")
      sim <- scheduleEvent(sim, time(sim), "caribouNN", "analyseExperiment")
    },
    prepareExperiment = {
      
      sim$experimentPlan <- generateExperimentPlan(startYear = 2008,
                                                   endYear = 2022,
                                                   numberOfCovariatesList = c(2, 5, 10, Inf),
                                                   outputPath = file.path(outputPath(sim), 
                                                                          paste0("experimentalDesign",
                                                                                 format(Sys.Date(),
                                                                                        "%d%b%y"),
                                                                                 ".csv"))
      # NOTE: in experimentPlan we need to swap 2008 for 2007 because we do not have data for 2008
      print("Swap 2008 to 2007")
      browser()
      sim$experimentDatasets <- lapply(unique(experimentPlan$groupId), function(ID){
        browser()
      }) # Create a list with all datasets
      # Train it, but check first if the given model exists --> save models using the 
      # experiment_typeName
    },
    trainExperiment = {
      
    },
    predictExperiment = {
      
    },
    compareExperiment = {
      
    },
    analyseExperiment = {
      
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  if (!suppliedElsewhere("preparedDataFinal", sim = sim)){
    stop("No defaults have been implemented yet...Please run caribouNN_Gobal")
  }
  if (!suppliedElsewhere("featurePriority", sim = sim)){
    sim$featurePriority <- fread("inputs/featureTable.csv")
  }
  return(invisible(sim))
}

ggplotFn <- function(data, ...) {
  ggplot2::ggplot(data, ggplot2::aes(TheSample)) +
    ggplot2::geom_histogram(...)
}

