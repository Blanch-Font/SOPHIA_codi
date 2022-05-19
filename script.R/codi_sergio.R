# Paquets necessaris
# devtools::install_github("ohdsi/CirceR")
# devtools::install_github("ohdsi/Capr")
# devtools::install_github("ohdsi/CohortGenerator")
# devtools::install_github("ohdsi/CohortDiagnostics")
# devtools::install(pkg = '~idiap/projects/SOPHIA')

library(SOPHIA)
library(DatabaseConnector)
library(magrittr)
# library(Andromeda)

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
cdm_schema <- 'omop21t2_test'
results_sc <- 'sophia_test'
cohortTable <- 'prova_Capr'

SOPHIAroot <- 'renv/library/R-4.1/x86_64-pc-linux-gnu/SOPHIA/'

# cohortInfo <- CreateSQL_T2DM(cdm_bbdd,
#                              cdm_schema,
#                              results_sc,
#                              cohortTable)
# outcomeInfo <- CreateSQL_T2DM_outcome(cdm_bbdd,
#                                       cdm_schema,
#                                       results_sc,
#                                       cohortTable)
#
# cohortDefinitionSet <- data.frame(atlasId = rep(NA, 2),
#                                   cohortId = 1:2,
#                                   cohortName = c("SIDIAP T2DM-WP5: Entrada",
#                                                  "SIDIAP T2DM-WP5: Outcome"),
#                                   sql = c(cohortInfo$ohdiSQL,
#                                           outcomeInfo$ohdiSQL),
#                                   json = c(cohortInfo$circeJson,
#                                            outcomeInfo$circeJson),
#                                   logicDescription = rep(as.character(NA), 2),
#                                   generateStats = rep(T, 2))

cohortDefinitionSet <- data.frame(
  atlasId = rep(NA, 2),
  CohortGenerator::getCohortDefinitionSet(
    settingsFileName = 'Cohorts.csv',
    jsonFolder = file.path(paste0(SOPHIAroot,'extdata')),
    sqlFolder = file.path(paste0(SOPHIAroot,'sql/sql_server'))),
  logicDescription = rep(as.character(NA), 2),
  generateStats = rep(T, 2))

n_cohort <- createCohort(cdm_bbdd,
                         cdm_schema,
                         results_sc,
                         cohortTable,
                         cohortDefinitionSet)

fet_diag <- runDiagnostic(cdm_bbdd,
                          cdm_schema,
                          results_sc,
                          cohortTable,
                          cohortDefinitionSet)
# CohortDiagnostics::launchDiagnosticsExplorer(dataFolder = paste0('~idiap/projects/SOPHIA_codi/',exportFolder),
#                                              dataFile = "Premerged.RData")

###################################################################################################
covariateData <- buildData(cdm_bbdd,
                           cdm_schema,
                           results_sc,
                           cohortTable)
covariateData2 <- FeatureExtraction::aggregateCovariates(covariateData)

sel_med_conceptId <- c(21600712, #DRUGS USED IN DIABETES
                       #Aquestes insulines no les troba
                       21076306, 44058584, 21086042, 21036596,
                       21601238, #C01
                       21600381, #C02
                       21601461, #C03
                       21601664, #C07
                       21601744, #C08
                       21601782, #C09
                       21601853, #C10
                       21603933 #M01A
)
cov_cate_resum <- covariateData2$covariateRef %>%
  dplyr::filter(analysisId %in% c(411, 413) & conceptId %in% sel_med_conceptId |
                  !(analysisId %in% c(411, 413))) %>%
  dplyr::inner_join(covariateData2$covariates) %>%
  dplyr::mutate(covariateId = as.character(floor(covariateId)),
                analysisId = as.integer(analysisId),
                conceptId = as.integer(conceptId),
                sumValue = as.integer(sumValue),
                averageValue = averageValue*100) %>%
  dplyr::collect()

cov_num_resum <- covariateData2$covariateRef %>%
  dplyr::inner_join(covariateData2$covariatesContinuous) %>%
  dplyr::mutate(covariateId = as.character(floor(covariateId)),
                analysisId = as.integer(analysisId),
                conceptId = as.integer(conceptId)) %>%
  dplyr::select(-covariateId, -analysisId, -conceptId) %>%
  dplyr::collect()

save(cov_cate_resum,
     cov_num_resum,
     file = 'taules_desc.RData')

disconnect(cdm_bbdd)
