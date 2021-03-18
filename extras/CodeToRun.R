library(EmcWaltersDementiaModel)
# USER INPUTS
#=======================
# The folder where the study intermediate and result files will be written:
outputFolder <- "./EmcWaltersDementiaModelResults"

# Details for connecting to the server:
dbms <- "you dbms"
user <- 'your username'
pw <- 'your password'
server <- 'your server'
port <- 'your port'

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)

# Add the database containing the OMOP CDM data
cdmDatabaseSchema <- 'cdm database schema'
# Add a database with read/write access as this is where the cohorts will be generated
cohortDatabaseSchema <- 'work database schema'
# Add the database name
cdmDatabaseName <- 'friendly database name'

oracleTempSchema <- NULL

# table name where the cohorts will be generated
cohortTable <- 'EmcWaltersDementiaModelCohort'

#=== Do not edit the following settings of the CodeToRun.R file 

# TAR settings (will be ignored, refer to the study protocol for final settings)
sampleSize <- NULL
riskWindowStart <- 1
startAnchor <- 'cohort start'
riskWindowEnd <- 365
endAnchor <- 'cohort start'
firstExposureOnly <- F
removeSubjectsWithPriorOutcome <- F
priorOutcomeLookback <- 99999
requireTimeAtRisk <- F
minTimeAtRisk <- 1
includeAllOutcomes <- T


#=======================

EmcWaltersDementiaModel::execute(connectionDetails = connectionDetails,
                              cdmDatabaseSchema = cdmDatabaseSchema,
                              cdmDatabaseName = cdmDatabaseName,
                              cohortDatabaseSchema = cohortDatabaseSchema,
                              cohortTable = cohortTable,
                              sampleSize = sampleSize,
                              riskWindowStart = riskWindowStart,
                              startAnchor = startAnchor,
                              riskWindowEnd = riskWindowEnd,
                              endAnchor = endAnchor,
                              firstExposureOnly = firstExposureOnly,
                              removeSubjectsWithPriorOutcome = removeSubjectsWithPriorOutcome,
                              priorOutcomeLookback = priorOutcomeLookback,
                              requireTimeAtRisk = requireTimeAtRisk,
                              minTimeAtRisk = minTimeAtRisk,
                              includeAllOutcomes = includeAllOutcomes,
                              outputFolder = outputFolder,
                              createCohorts = T,
                              runAnalyses = T,
                              recalibrate = F,
                              viewShiny = T,
                              packageResults = T,
                              minCellCount= 5,
                              verbosity = "INFO",
                              cdmVersion = 5)

cdmDatabaseNameRecalibrated <- paste0(cdmDatabaseName, "_recalibrated")

EmcWaltersDementiaModel::execute(connectionDetails = connectionDetails,
                              cdmDatabaseSchema = cdmDatabaseSchema,
                              cdmDatabaseName = cdmDatabaseNameRecalibrated,
                              cohortDatabaseSchema = cohortDatabaseSchema,
                              cohortTable = cohortTable,
                              sampleSize = sampleSize,
                              riskWindowStart = riskWindowStart,
                              startAnchor = startAnchor,
                              riskWindowEnd = riskWindowEnd,
                              endAnchor = endAnchor,
                              firstExposureOnly = firstExposureOnly,
                              removeSubjectsWithPriorOutcome = removeSubjectsWithPriorOutcome,
                              priorOutcomeLookback = priorOutcomeLookback,
                              requireTimeAtRisk = requireTimeAtRisk,
                              minTimeAtRisk = minTimeAtRisk,
                              includeAllOutcomes = includeAllOutcomes,
                              outputFolder = outputFolder,
                              createCohorts = T,
                              runAnalyses = T,
                              recalibrate = T,
                              viewShiny = T,
                              packageResults = T,
                              minCellCount= 5,
                              verbosity = "INFO",
                              cdmVersion = 5)