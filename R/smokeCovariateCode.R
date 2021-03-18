# Copyright 2021 Observational Health Data Sciences and Informatics
#
# This file is part of SkeletonPredictionValidationStudy
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Extracts smoker or ex-smoker status covariates
#'
#' @details
#' This extracts smoker or ex-smoker from defined concept set ids
#'
#' @param connection  The database connection
#' @param oracleTempSchema  The temp schema if using oracle
#' @param cdmDatabaseSchema  The schema of the OMOP CDM data
#' @param cdmVersion  version of the OMOP CDM data
#' @param cohortTable  the table name that contains the target population cohort
#' @param rowIdField  string representing the unique identifier in the target population cohort
#' @param aggregated  whether the covariate should be aggregated
#' @param cohortId  cohort id for the target population cohort
#' @param covariateSettings  settings for the covariate cohorts and time periods
#'
#' @return
#' The models will now be in the package
#'
#' @export
getSmokingCovariateData <- function(connection,
                                    oracleTempSchema = NULL,
                                    cdmDatabaseSchema,
                                    cdmVersion = "5",
                                    cohortTable = "#cohort_person",
                                    rowIdField = "row_id",
                                    aggregated,
                                    cohortId,
                                    covariateSettings) {
  
  # Some SQL to construct the covariate:
  sql <- paste("select lastdates.row_id, ",
               "case when alldates.concept_id in (@smokercodes) then 1000+@analysisId",
               " when alldates.concept_id in (@exsmokercodes) then 2000+@analysisId else 0 end as covariate_id, ",
               "1 as covariate_value from ",
               
               "(select c.@row_id_field AS row_id, d.person_id, max(d.date) as date from ",
               "(select person_id, condition_start_date as date from @cdm_database_schema.condition_occurrence where condition_concept_id in (@allCodes) union ",
               "select person_id, procedure_date as date from @cdm_database_schema.procedure_occurrence where procedure_concept_id in (@allCodes) union ",
               "select person_id, observation_date as date from @cdm_database_schema.observation where observation_concept_id in (@allCodes)) d",
               "inner join @cohort_temp_table c on d.person_id = c.subject_id and d.date <= dateadd(day, @endDay, c.cohort_start_date) and d.date >= dateadd(day, @startDay, c.cohort_start_date) ",
               "group by c.@row_id_field, d.person_id) lastdates ",
               
               "inner join ",
               
               "(select person_id, condition_concept_id as concept_id, condition_start_date as date from @cdm_database_schema.condition_occurrence where condition_concept_id in (@allCodes) union ",
               "select person_id, procedure_concept_id as concept_id, procedure_date as date from @cdm_database_schema.procedure_occurrence where procedure_concept_id in (@allCodes) union ",
               "select person_id, observation_concept_id as concept_id, observation_date as date from @cdm_database_schema.observation where observation_concept_id in (@allCodes)) alldates",
               
               " on lastdates.person_id = alldates.person_id and lastdates.date = alldates.date"
  )
  
  sql <- SqlRender::render(sql,
                           cohort_temp_table = cohortTable,
                           row_id_field = rowIdField,
                           startDay=covariateSettings$startDay,
                           endDay=covariateSettings$endDay,
                           smokercodes = paste(covariateSettings$smokerConceptSet, collapse = ','),
                           exsmokercodes = paste(covariateSettings$exsmokerConceptSet, collapse = ','),
                           allCodes = paste(c(covariateSettings$exsmokerConceptSet, covariateSettings$smokerConceptSet), collapse = ','),
                           cdm_database_schema = cdmDatabaseSchema,
                           analysisId = covariateSettings$analysisId
  )
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"),
                              oracleTempSchema = oracleTempSchema)
  # Retrieve the covariate:
  covariates <- DatabaseConnector::querySql(connection, sql)
  # Convert colum names to camelCase:
  colnames(covariates) <- SqlRender::snakeCaseToCamelCase(colnames(covariates))
  
  
  # Construct covariate reference:
  covariateRef <- data.frame(covariateId = c(1000,2000)+covariateSettings$analysisId,
                             covariateName = paste(c('Smoker during day','ex-smoker during day'),
                                                   covariateSettings$startDay,
                                                   'through',
                                                   covariateSettings$endDay,
                                                   'days relative to index:',
                                                   covariateSettings$covariateName
                             ),
                             analysisId = rep(covariateSettings$analysisId,2),
                             conceptId = rep(0,2))
  
  analysisRef <- data.frame(analysisId = covariateSettings$analysisId,
                            analysisName = "smoker status covariate",
                            domainId = "smoker status covariate",
                            startDay = covariateSettings$startDay,
                            endDay = covariateSettings$endDay,
                            isBinary = "Y",
                            missingMeansZero = "Y")
  
  metaData <- list(sql = sql, call = match.call())
  result <- Andromeda::andromeda(covariates = covariates,
                                 covariateRef = covariateRef,
                                 analysisRef = analysisRef)
  attr(result, "metaData") <- metaData
  class(result) <- "CovariateData"	
  return(result)
}


createSmokingCovariateSettings <- function(covariateName = 'Smoking status',
                                           startDay=-1825, endDay=0, 
                                           smokerConceptSet = c(762498,762499,764103,764104,
                                                                4041511,4042037,4044775,4044776,
                                                                4044777,4044778,4052029,4052030,
                                                                4052947,4058136,4058138,4141787,4144273,
                                                                4204653,4209006,4209585,4218917,4246415,
                                                                4276526,4298794,37395605),
                                           exsmokerConceptSet = c(762500,762501,4052465,4052949,4092281,
                                                                  4141782,4141783,4141784,4145798,4148415,
                                                                  4148416,4207221,4232375,4237385,4310250,
                                                                  35610339,35610340,35610343,35610345,35610347,
                                                                  35610349,42536346,44802113,46270534),
                                           analysisId = 477
) {
  
  covariateSettings <- list(covariateName=covariateName, 
                            startDay=startDay,
                            endDay=endDay,
                            smokerConceptSet = smokerConceptSet,
                            exsmokerConceptSet = exsmokerConceptSet,
                            analysisId = analysisId
  )
  
  attr(covariateSettings, "fun") <- "getSmokingCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}
