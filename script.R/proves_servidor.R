Sys.setenv("DATABASECONNECTOR_JAR_FOLDER" = "data/jdbcDrivers/")

dbms = Sys.getenv("DBMS")
user <- if (Sys.getenv("DB_USER") == "") NULL else Sys.getenv("DB_USER")
password <- if (Sys.getenv("DB_PASSWORD") == "") NULL else Sys.getenv("DB_PASSWORD")
server = Sys.getenv("DB_SERVER")
port = Sys.getenv("DB_PORT")
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = password,
                                                                port = port)
cdm_bbdd <- DatabaseConnector::connect(connectionDetails = connectionDetails)
cdm_schema <- 'omop21t2_test'
results_sc <- 'sophia_test'
cohortTable <- 'SOPHIA_SIDIAP_T2DM'

DatabaseConnector::executeSql(connection = cdm_bbdd,
                              sql = "CREATE TABLE sophia_test.sophia_sidiap_t2dm_2 (
                              cohort_definition_id integer,
                              subject_id           integer,
                              cohort_start_date    date,
                              cohort_end_date      date
                              );")

SqlRender::renderSqlFile(sourceFile = 'inst/sql/sql_server/cohort_def.sql',
                         targetFile = 'prova.sql',
                         vocabulary_database_schema = cdm_schema,
                         cdm_database_schema = cdm_schema,
                         target_database_schema = results_sc,
                         target_cohort_table = cohortTable,
                         target_cohort_id = 1778445)
sql <- SqlRender::readSql(sourceFile = 'prova.sql')
cohortDefinitionSet <- tibble::tibble(atlasId = 1778445,
                                  cohortId = 1778445,
                                  cohortName = 'SOPHIA_SIDIAP_T2DM',
                                  # cohortFullName = "Cohort de Diabetes T2DM pel projecte SOPHIA",
                                  # sql = sql,
                                  sql = readChar(con = 'inst/sql/sql_server/100.sql',
                                                 nchars = file.info('inst/sql/sql_server/100.sql')$size),
                                  json = readChar(con = 'inst/extdata/100.json',
                                                  nchars = file.info('inst/extdata/100.json')$size),
                                  logicDescription = as.character(NA),
                                  generateStats = TRUE)

cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = cohortTable)
CohortGenerator::createCohortTables(#connectionDetails = connectionDetails,
                                    connection = cdm_bbdd,
                                    cohortTableNames = cohortTableNames,
                                    cohortDatabaseSchema = results_sc,
                                    incremental = FALSE)
CohortGenerator::generateCohortSet(#connectionDetails = connectionDetails,
                                   connection = cdm_bbdd,
                                   cdmDatabaseSchema = cdm_schema,
                                   cohortDatabaseSchema = results_sc,
                                   cohortTableNames = cohortTableNames,
                                   cohortDefinitionSet = cohortDefinitionSet,
                                   incremental = FALSE)
cohortCounts <- CohortGenerator::getCohortCounts(#connectionDetails = connectionDetails,
                                                 connection = cdm_bbdd,
                                                 cohortDatabaseSchema = results_sc,
                                                 cohortTable = cohortTableNames$cohortTable)
CohortGenerator::insertInclusionRuleNames(#connectionDetails = connectionDetails,
                                          connection = cdm_bbdd,
                                          cohortDefinitionSet = cohortDefinitionSet,
                                          cohortDatabaseSchema = results_sc,
                                          cohortInclusionTable = cohortTableNames$cohortInclusionTable)
CohortGenerator::exportCohortStatsTables(#connectionDetails = connectionDetails,
                                         connection = cdm_bbdd,
                                         cohortDatabaseSchema = results_sc,
                                         cohortTableNames = cohortTableNames,
                                         cohortStatisticsFolder = file.path("InclusionStats"))
exportFolder <- "export"
CohortDiagnostics::executeDiagnostics(cohortDefinitionSet,
                                      # connectionDetails = connectionDetails,
                                      connection = cdm_bbdd,
                                      cohortTable = cohortTable,
                                      cohortDatabaseSchema = results_sc,
                                      cdmDatabaseSchema = cdm_schema,
                                      vocabularyDatabaseSchema = cdm_schema,
                                      exportFolder = exportFolder,
                                      databaseId = "MyCdm",
                                      minCellCount = 5)
CohortGenerator::dropCohortStatsTables(#connectionDetails = connectionDetails,
                                       connection = cdm_bbdd,
                                       cohortDatabaseSchema = results_sc,
                                       cohortTableNames = cohortTableNames)
CohortDiagnostics::preMergeDiagnosticsFiles(exportFolder)
CohortDiagnostics::launchDiagnosticsExplorer(dataFolder = paste0('~idiap/projects/SOPHIA/',exportFolder),
                                             dataFile = "Premerged.RData")


cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(baseUrl = 'http://api.ohdsi.org:80/WebAPI',
                                                               cohortIds = 1778445,
                                                               generateStats = TRUE)
# cohortDefinitionSet$sql <- sql
CohortGenerator::generateCohortSet(connection = cdm_bbdd,
                                   cdmDatabaseSchema = cdm_schema,
                                   cohortDatabaseSchema = results_sc,
                                   cohortTableNames = cohortTableNames,
                                   cohortDefinitionSet = cohortDefinitionSet,
                                   incremental = FALSE)
CohortGenerator::insertInclusionRuleNames(connection = cdm_bbdd,
                                          cohortDefinitionSet = cohortDefinitionSet,
                                          cohortDatabaseSchema = results_sc,
                                          cohortInclusionTable = cohortTableNames$cohortInclusionTable)
CohortGenerator::exportCohortStatsTables(connection = cdm_bbdd,
                                         cohortDatabaseSchema = results_sc,
                                         cohortTableNames = cohortTableNames,
                                         cohortStatisticsFolder = file.path("InclusionStats"))

# sql <- SqlRender::readSql(sourceFile = 'prova2.sql')
# DatabaseConnector::executeSql(connection = cdm_bbdd,
#                               sql = sql)
DatabaseConnector::disconnect(cdm_bbdd)
