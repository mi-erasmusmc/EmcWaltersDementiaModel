library(EmcWaltersDementiaModel)
# USER INPUTS
#=======================
# The folder where the study intermediate and result files will be written:
outputFolder <- "./EmcWaltersDementiaModelResults"
options(andromedaTempFolder = "S:/temp/tempandromeda")

# Details for connecting to the server:
dbms <- "pdw"
user <- NULL
pw <- NULL
server <- 'JRDUSAPSCTL01'
port <- 17001

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)

# Add the database containing the OMOP CDM data
cdmDatabaseSchema <- c("CDM_Optum_Panther_v1355.dbo",
                       "CDM_CPRD_v1299.dbo",
                       "CDM_IBM_MDCR_v1352.dbo",
                       "CDM_IQVIA_Germany_DA_v1376.dbo",
                       "CDM_Optum_Extended_SES_v1387.dbo")

cdmDatabaseName <- c('OPPANv1355',
                     "CPRDv1299",
                     "MDCRv1352",
                     "IQGERv1376",
                     "OPSESv1387")

# Add a database with read/write access as this is where the cohorts will be generated
cohortDatabaseSchema <- 'Scratch.dbo'

oracleTempSchema <- NULL

# table name where the cohorts will be generated
cohortTable <- 'EmcWaltersDementiaModelCohort'

# # TAR settings
sampleSize <- 100000 #1000000
# riskWindowStart <- 1
# startAnchor <- 'cohort start'
# riskWindowEnd <- 365*5
# endAnchor <- 'cohort start'
# firstExposureOnly <- F
# removeSubjectsWithPriorOutcome <- F
# priorOutcomeLookback <- 99999
# requireTimeAtRisk <- T
# minTimeAtRisk <- 365
# includeAllOutcomes <- T

#=======================
for (i in 4:length(cdmDatabaseName)) {
  
  EmcWaltersDementiaModel::execute(connectionDetails = connectionDetails,
                                       cdmDatabaseSchema = cdmDatabaseSchema[i],
                                       cdmDatabaseName = cdmDatabaseName[i],
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
                                       viewShiny = F,
                                       packageResults = F,
                                       minCellCount= 5,
                                       verbosity = "INFO",
                                       cdmVersion = 5)
}
