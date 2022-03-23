Sys.setenv("DATABASECONNECTOR_JAR_FOLDER" = "data/jdbcDrivers/")

cdm_connect <- DatabaseConnector::createConnectionDetails(dbms = 'postgresql',
                                                          user = 'jordi',
                                                          password = 'jordi',
                                                          server = 'localhost/postgres')
cdm_bbdd <- DatabaseConnector::connect(connectionDetails = cdm_connect)
cdm_schema <- 'synpuf'
results <- 'results'

aux_sql <- SqlRender::render(sql = "SELECT COUNT(*) FROM @schema_cdm.PERSON;",
                             schema_cdm = cdm_schema)
aux <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                   sql = aux_sql)

# Busquem els descendents
diab_sql <- SqlRender::render(sql = "SELECT * FROM @schema_cdm.CONCEPT_ANCESTOR where ancestor_concept_id = 201826",
                              schema_cdm = cdm_schema)
diab_def <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                        sql = diab_sql)

ckd_sql <- SqlRender::render(sql = "SELECT * FROM @schema_cdm.CONCEPT_ANCESTOR where ancestor_concept_id = 46271022",
                             schema_cdm = cdm_schema)
ckd_def <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                       sql = ckd_sql)

esquizo_sql <- SqlRender::render(sql = "SELECT * FROM @schema_cdm.CONCEPT_ANCESTOR where ancestor_concept_id = 435783",
                                 schema_cdm = cdm_schema)
esquizo_def <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                           sql = esquizo_sql)

epilepsia_sql <- SqlRender::render(sql = "SELECT * FROM @schema_cdm.CONCEPT_ANCESTOR where ancestor_concept_id = 4029498",
                                   schema_cdm = cdm_schema)
epilepsia_def <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                             sql = epilepsia_sql)

tumor_sql <- SqlRender::render(sql = "SELECT * FROM @schema_cdm.CONCEPT_ANCESTOR where ancestor_concept_id = 443392",
                               schema_cdm = cdm_schema)
tumor_def <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                         sql = tumor_sql)

drug_sql <- SqlRender::render(sql = "SELECT * FROM @schema_cdm.CONCEPT_ANCESTOR where ancestor_concept_id = 21602722",
                              schema_cdm = cdm_schema)
drug_def <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                        sql = drug_sql)

# Agafem tots els diagnòstics de DM-T2
diab_cond_sql <- SqlRender::render(sql = "SELECT * FROM @schema_cdm.CONDITION_OCCURRENCE where condition_concept_id IN (@diab_def)",
                                   schema_cdm = cdm_schema,
                                   diab_def = c(201826, diab_def$DESCENDANT_CONCEPT_ID))
diab_cond <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                         sql = diab_cond_sql)
diab_cond <- data.table::as.data.table(diab_cond)
if (is(diab_cond$CONDITION_START_DATE)[1] != "Date"){
  diab_cond[, CONDITION_START_DATE := as.Date(CONDITION_START_DATE, origin = '1970-01-01')]
}

## Ens quedem el primer diagnòstic
diab_cond_1r <- diab_cond[, .(diab_1r  = min(CONDITION_START_DATE)),
                          keyby = .(PERSON_ID)]

# Baixem la informació demogràfica
diab_pers_sql <- SqlRender::render(sql = "SELECT * FROM @schema_cdm.PERSON where person_id IN (@p)",
                                   schema_cdm = cdm_schema,
                                   p = unique(diab_cond_1r$PERSON_ID))
diab_pers <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                         sql = diab_pers_sql)
diab_pers <- data.table::as.data.table(diab_pers)

flow_chart <- data.table::data.table(pas = c('Origen', 'T2DM'),
                                     N = c(aux, diab_pers[, .N]))

# Ens quedem amb els diagnosticats posteriors als 35 anys.
diab_cond_1r <- data.table::merge.data.table(x = diab_cond_1r,
                                             y = diab_pers[, .(PERSON_ID, YEAR_OF_BIRTH, MONTH_OF_BIRTH, DAY_OF_BIRTH)],
                                             by = "PERSON_ID",
                                             all.x = TRUE)
diab_cond_1r[, `:=`(diab_1r = as.Date(diab_1r, format = '%Y-%m-%d'),
                    dbirth = as.Date(sprintf('%i-%i-%i', YEAR_OF_BIRTH, MONTH_OF_BIRTH,
                                             DAY_OF_BIRTH), format = '%Y-%m-%d'))]
diab_cond_1r[, age := as.numeric(diab_1r - dbirth)/365.25]
diab_cond_1r <- diab_cond_1r[35 <= age]

flow_chart <- data.table::rbindlist(list(flow_chart,
                                         data.table::data.table(pas = 'Diagnostic posterior 35 anys',
                                                                N = diab_cond_1r[, .N])))

# Baixem el periode d'observació
diab_obs_sql <- SqlRender::render("SELECT * FROM @schema_cdm.OBSERVATION_PERIOD where person_id IN (@p)",
                                  schema_cdm = cdm_schema,
                                  p = unique(diab_cond_1r$PERSON_ID))
diab_obs <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                        sql = diab_obs_sql)
# Baixem la informació de la mort
diab_death_sql <- SqlRender::render("SELECT * FROM @schema_cdm.DEATH where person_id IN (@p)",
                                    schema_cdm = cdm_schema,
                                    p = unique(diab_cond_1r$PERSON_ID))
diab_death <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                          sql = diab_death_sql)

# Definim el periode d'observació
diab_cond_1r <- merge(diab_cond_1r,
                      diab_obs,
                      by = "PERSON_ID")
diab_cond_1r <- merge(diab_cond_1r,
                      diab_death,
                      by = "PERSON_ID")
if(is(diab_cond_1r$OBSERVATION_PERIOD_START_DATE)[1] != 'Date'){
  diab_cond_1r[, OBSERVATION_PERIOD_START_DATE := as.Date(OBSERVATION_PERIOD_START_DATE,
                                                          origin = '1970-01-01')]
}
if(is(diab_cond_1r$OBSERVATION_PERIOD_END_DATE)[1] != 'Date'){
  diab_cond_1r[, OBSERVATION_PERIOD_END_DATE := as.Date(OBSERVATION_PERIOD_END_DATE,
                                                        origin = '1970-01-01')]
}
if(is(diab_cond_1r$DEATH_DATE)[1] != 'Date'){
  diab_cond_1r[, DEATH_DATE := as.Date(DEATH_DATE,
                                       origin = '1970-01-01')]
}

diab_cond_1r[, `:=`(min_date = pmax(diab_1r, OBSERVATION_PERIOD_START_DATE,
                                     as.Date('2010-01-01')),
                    max_date = pmin(DEATH_DATE, OBSERVATION_PERIOD_END_DATE, as.Date('2020-12-31')))]
# Ens quedem als que tenen un periode vàlid
diab_cond_1r <- diab_cond_1r[min_date <= max_date]
flow_chart <- data.table::rbindlist(list(flow_chart,
                                         data.table::data.table(pas = 'Dins el periode estudi',
                                                                N = diab_cond_1r[, .N])))

# Criteris d'exclusió condicions
exc_cond_sql <- SqlRender::render("SELECT *
                                   FROM @schema_cdm.CONDITION_OCCURRENCE
                                   WHERE condition_concept_id IN (@excl_def) AND person_id IN (@p)",
                                  schema_cdm = cdm_schema,
                                  excl_def = c(46271022, ckd_def$DESCENDANT_CONCEPT_ID,
                                               435783, esquizo_def$DESCENDANT_CONCEPT_ID,
                                               4029498, epilepsia_def$DESCENDANT_CONCEPT_ID,
                                               443392, tumor_def$DESCENDANT_CONCEPT_ID),
                                  p = unique(diab_cond_1r$PERSON_ID))
exc_cond <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                        sql = exc_cond_sql)
exc_cond <- data.table::as.data.table(exc_cond)
## Ens quedem el primer diagnòstic
if(is(exc_cond$CONDITION_START_DATE)[1] != 'Date'){
  exc_cond[, CONDITION_START_DATE := as.Date(CONDITION_START_DATE,
                                             origin = '1970-01-01')]
}
exc_cond_1r <- exc_cond[, .(exc_1r  = min(CONDITION_START_DATE)),
                        keyby = .(PERSON_ID)]
diab_cond_1r <- merge(diab_cond_1r,
                      exc_cond_1r,
                      by = "PERSON_ID",
                      all.x = TRUE)
diab_cond_1r <- diab_cond_1r[is.na(exc_1r) | diab_1r < exc_1r]
flow_chart <- data.table::rbindlist(list(flow_chart,
                                         data.table::data.table(pas = "Fora exclusions (condicions) a l'entrada",
                                                                N = diab_cond_1r[, .N])))

exc_drug_sql <- SqlRender::render("SELECT *
                                   FROM @schema_cdm.DRUG_EXPOSURE
                                   WHERE drug_concept_id IN (@excl_def) AND person_id IN (@p)",
                                  schema_cdm = cdm_schema,
                                  excl_def = c(21602722, drug_def$DESCENDANT_CONCEPT_ID),
                                  p = unique(diab_cond_1r$PERSON_ID))
exc_drug <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                        sql = exc_drug_sql)
## Ens quedem el primer diagnòstic
exc_drug <- data.table::as.data.table(exc_drug)
if(is(exc_drug$DRUG_EXPOSURE_START_DATE)[1] != 'Date'){
  exc_drug[, DRUG_EXPOSURE_START_DATE := as.Date(DRUG_EXPOSURE_START_DATE,
                                                 origin = '1970-01-01')]
}
exc_drug_1r <- exc_drug[, .(exc_drug_1r  = min(DRUG_EXPOSURE_START_DATE)),
                           keyby = .(PERSON_ID)]
diab_cond_1r <- merge(diab_cond_1r,
                      exc_drug_1r,
                      by = "PERSON_ID",
                      all.x = TRUE)
diab_cond_1r <- diab_cond_1r[is.na(exc_drug_1r) | diab_1r < exc_drug_1r]
flow_chart <- data.table::rbindlist(list(flow_chart,
                                         data.table::data.table(pas = "Fora exclusions (drug) a l'entrada",
                                                                N = diab_cond_1r[, .N])))

# Creem una nova bbdd amb la taula cohort
diab_cond_1r <- diab_cond_1r[order(PERSON_ID)]
diab_cond_1r[min_date == diab_1r,
             min_date := min_date + 1]
cohort <- diab_cond_1r[, .(cohort_definition_id = 1,
                           cohort_start_date = min_date, #as.POSIXct(min_date),
                           cohort_end_date = max_date, #as.POSIXct(max_date),
                           subject_id = PERSON_ID)]
# cohort[, cohort_definition_id := cumsum(cohort_definition_id)]

DatabaseConnector::insertTable(connection = cdm_bbdd,
                               databaseSchema = results,
                               tableName = "COHORT",
                               data = cohort,
                               createTable = TRUE,
                               dropTableIfExists = TRUE)

covariateSettings <- FeatureExtraction::createDefaultCovariateSettings()
settings2 <- FeatureExtraction::convertPrespecSettingsToDetailedSettings(covariateSettings)
covariateData <- FeatureExtraction::getDbCovariateData(connection = cdm_bbdd,
                                                       cdmDatabaseSchema = cdm_schema,
                                                       cohortTable = "cohort",
                                                       cohortDatabaseSchema = results,
                                                       cohortId = -1,
                                                       rowIdField = "subject_id",
                                                       covariateSettings = covariateSettings)
# FeatureExtraction::saveCovariateData(covariateData, "covariates")
# covariateData <- FeatureExtraction::loadCovariateData(file = "covariates")
covariateData2 <- FeatureExtraction::aggregateCovariates(covariateData)
result <- FeatureExtraction::createTable1(covariateData2,
                                          output = "one column")
View(result)

covariateSettings_m <- FeatureExtraction::createCovariateSettings(
  useDemographicsGender = TRUE,
  useDemographicsAge = TRUE,
  useDemographicsAgeGroup = TRUE,
  useConditionOccurrenceAnyTimePrior = TRUE,
  useConditionGroupEraAnyTimePrior = TRUE,
  useConditionOccurrencePrimaryInpatientAnyTimePrior = TRUE,
  useDrugExposureAnyTimePrior = TRUE,
  useMeasurementAnyTimePrior = TRUE,
  # useMeasurementValueAnyTimePrior = TRUE,
  useMeasurementRangeGroupAnyTimePrior = TRUE,
  useObservationAnyTimePrior = TRUE,
  includedCovariateConceptIds = 201826,
  addDescendantsToInclude = TRUE)#,



settings_m <- FeatureExtraction::convertPrespecSettingsToDetailedSettings(covariateSettings_m)
covariateData_m <- FeatureExtraction::getDbCovariateData(connection = cdm_bbdd,
                                                         cdmDatabaseSchema = cdm_schema,
                                                         cohortTable = "cohort",
                                                         cohortDatabaseSchema = results,
                                                         cohortId = -1,
                                                         # rowIdField = "subject_id",
                                                         covariateSettings = covariateSettings_m,
                                                         aggregated = TRUE)
# covariateData2_m <- FeatureExtraction::aggregateCovariates(covariateData_m)
result_m <- FeatureExtraction::createTable1(covariateData2_m,
                                            output = "one column")
View(result_m)

cdm_schema <- ''
results <- ''

library(CohortDiagnostics)
cdmDatabaseSchema <- "synpuf"
tempEmulationSchema <- NULL
cohortDatabaseSchema <- "results"
cohortTable <- "COHORT"
preMergeDiagnosticsFiles('www/allZipFiles')
