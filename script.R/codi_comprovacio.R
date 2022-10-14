###################################################################################################
# Auxiliar package
###################################################################################################
devtools::install_github("ohdsi/CirceR")
devtools::install_github("ohdsi/Capr")
devtools::install_github("ohdsi/CohortGenerator")
devtools::install_github("ohdsi/CohortDiagnostics")
# devtools::install(pkg = '~idiap/projects/SOPHIA')
devtools::install_github('Blanch-Font/SOPHIA')

###################################################################################################
# Package
###################################################################################################
library(SOPHIA)
library(DatabaseConnector)
library(magrittr)
library(dplyr)
library(tidyr)
library(purrr)
library(uwot)

###################################################################################################
# Server Configuration
###################################################################################################
Sys.setenv("DATABASECONNECTOR_JAR_FOLDER" = "data/jdbcDrivers/")

dbms = Sys.getenv("DBMS")
user <- if (Sys.getenv("DB_USER") == "") NULL else Sys.getenv("DB_USER")
password <- if (Sys.getenv("DB_PASSWORD") == "") NULL else Sys.getenv("DB_PASSWORD")
server = Sys.getenv("DB_SERVER")
port = Sys.getenv("DB_PORT")
connectionDetails <- createConnectionDetails(dbms = dbms,
                                             server = server,
                                             user = user,
                                             password = password,
                                             port = port)
cdm_bbdd <- connect(connectionDetails = connectionDetails)

# Name in the server. Better in Renviron?
cdm_schema <- 'omop21t2_test'
results_sc <- 'sophia_test'
cohortTable <- 'cohortTable'

# SOPHIA package's directory
SOPHIAroot <- 'renv/library/R-4.1/x86_64-pc-linux-gnu/SOPHIA/'

###################################################################################################
# Cohort and Outcome creation
###################################################################################################
# Cohort T2DM
cohort_T2DM <- CreateSQL_T2DM(cdm_bbdd,
                              cdm_schema,
                              results_sc,
                              cohortTable)
# Cohort T1DM
cohort_T1DM <- CreateSQL_T1DM(cdm_bbdd,
                              cdm_schema,
                              results_sc,
                              cohortTable)
# Outcome AMI
outcome_AMI <- CreateSQL_AMI(cdm_bbdd,
                             cdm_schema,
                             results_sc,
                             cohortTable)
# Outcome Angor
outcome_angor <- CreateSQL_angor(cdm_bbdd,
                                 cdm_schema,
                                 results_sc,
                                 cohortTable)
# Outcome Stroke Ischemic
outcome_stroke_i <- CreateSQL_stroke_i(cdm_bbdd,
                                       cdm_schema,
                                       results_sc,
                                       cohortTable)
# Outcome TIA
outcome_TIA <- CreateSQL_TIA(cdm_bbdd,
                             cdm_schema,
                             results_sc,
                             cohortTable)

# Outcome Nephropathy
outcome_nephro <- CreateSQL_nephro(cdm_bbdd,
                                   cdm_schema,
                                   results_sc,
                                   cohortTable)

# Outcome Retinopathy
outcome_retino <- CreateSQL_retino(cdm_bbdd,
                                   cdm_schema,
                                   results_sc,
                                   cohortTable)

# Outcome Neuropathy
outcome_neuro <- CreateSQL_neuro(cdm_bbdd,
                                 cdm_schema,
                                 results_sc,
                                 cohortTable)

# Outcome PAD
outcome_PAD <- CreateSQL_PAD(cdm_bbdd,
                             cdm_schema,
                             results_sc,
                             cohortTable)

# Definition Set for cohort and outcome
cohortDefinitionSet <- data.frame(atlasId = rep(NA, 10),
                                  cohortId = 1:10,
                                  cohortName = c("SIDIAP T2DM-WP5",
                                                 "SIDIAP T1DM-WP4",
                                                 "Outcome: AMI",
                                                 "Outcome: Angor",
                                                 "Outcome: Stroke I",
                                                 "Outcome: TIA",
                                                 "Outcome: Nephropathy",
                                                 "Outcome: Retinopathy",
                                                 "Outcome: Neuropathy",
                                                 "Outcome: PAD"),
                                  sql = c(cohort_T2DM$ohdiSQL,
                                          cohort_T1DM$ohdiSQL,
                                          outcome_AMI$ohdiSQL,
                                          outcome_angor$ohdiSQL,
                                          outcome_stroke_i$ohdiSQL,
                                          outcome_TIA$ohdiSQL,
                                          outcome_nephro$ohdiSQL,
                                          outcome_retino$ohdiSQL,
                                          outcome_neuro$ohdiSQL,
                                          outcome_PAD$ohdiSQL),
                                  json = c(cohort_T2DM$circeJson,
                                           cohort_T1DM$circeJson,
                                           outcome_AMI$circeJson,
                                           outcome_angor$circeJson,
                                           outcome_stroke_i$circeJson,
                                           outcome_TIA$circeJson,
                                           outcome_nephro$circeJson,
                                           outcome_retino$circeJson,
                                           outcome_neuro$circeJson,
                                           outcome_PAD$circeJson),
                                  logicDescription = rep(as.character(NA), 10),
                                  generateStats = rep(T, 10))

# Creation and saving in the server
n_cohort <- createCohort(cdm_bbdd,
                         cdm_schema,
                         results_sc,
                         cohortTable,
                         cohortDefinitionSet)

###################################################################################################
# Cohort diagnosis
###################################################################################################
fet_diag <- runDiagnostic(cdm_bbdd,
                          cdm_schema,
                          results_sc,
                          cohortTable,
                          cohortDefinitionSet)
# CohortDiagnostics::launchDiagnosticsExplorer(sqliteDbPath = 'MergedCohortDiagnosticsData.sqlite')
disconnect(cdm_bbdd)
