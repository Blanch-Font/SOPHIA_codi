Sys.setenv("DATABASECONNECTOR_JAR_FOLDER" = "data/jdbcDrivers/")

cdm_connect <- DatabaseConnector::createConnectionDetails(dbms = 'postgresql',
                                                          user = 'jordi',
                                                          password = 'jordi',
                                                          server = 'localhost/postgres')
cdm_bbdd <- DatabaseConnector::connect(connectionDetails = cdm_connect)
cdm_schema <- 'synpuf'
results <- 'results'

SqlRender::renderSqlFile(sourceFile = 'inst/sql/sql_server/cohort_def.sql',
                         targetFile = 'prova.sql',
                         vocabulary_database_schema = 'synpuf',
                         cdm_database_schema = 'synpuf',
                         target_database_schema = 'results',
                         target_cohort_table = 'cohort2',
                         target_cohort_id = 1778445)
sql <- SqlRender::readSql(sourceFile = 'prova.sql')
DatabaseConnector::executeSql(connection = cdm_bbdd,
                              sql = sql)
  # sql <- SqlRender::loadRenderTranslateSql(sqlFilename = 'cohort_def.sql',
#                                          packageName = 'SOPHIA',
#                                          vocabulary_database_schema = 'synpuf',
#                                          cdm_database_schema = 'synpuf',
#                                          target_database_schema = 'results',
#                                          target_cohort_table = 'cohort2',
#                                          target_cohort_id = 13)
