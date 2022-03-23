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

# Busquem els codis CONCEPT_ID
sql_concept <- SqlRender::render(sql = "SELECT *
                                        FROM @schema_cdm.CONCEPT
                                        WHERE CONCEPT_ID in (@diab_def)",
                                 schema_cdm = cdm_schema,
                                 diab_def = setdiff(unique(diab_def$DESCENDANT_CONCEPT_ID),
                                                    unique(diab_exc_def$DESCENDANT_CONCEPT_ID)))
concept <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                       sql = sql_concept)
# concept <- data.table::as.data.table(concept)
# t2dm_code <- concept[grepl(pattern = '[T|t]ype 2 diabetes mellitus',
#                            x = CONCEPT_NAME) &
#                        DOMAIN_ID == 'Condition']
# t2dm_code <- t2dm_code[!grepl(pattern = 'existing',
#                               x = CONCEPT_NAME)]

# Definició DMII
vec_dm_hist <- c(40769338, 43021173, 42539022, 46270562)
vec_dm_cur <- c(201820, 442793, 443238)
vec_dm <- c(vec_dm_hist, vec_dm_cur)
vec_dm_exc <- c(201254, 4058243, 435216, 40484648)
vec_dm_exc2 <- c(192279, 195771, 200687, 318712, 321822, 376114, 376683, 376979, 377552, 377821,
                 378743, 380096, 380097, 442793, 443727, 443730, 443735, 443767)
diab_sql <- SqlRender::render(sql = "SELECT *
                                     FROM @schema_cdm.CONCEPT_ANCESTOR
                                     WHERE ancestor_concept_id IN (@dm)",
                              schema_cdm = cdm_schema,
                              dm = vec_dm)
diab_def <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                        sql = diab_sql)

diab_exc_sql <- SqlRender::render(sql = "SELECT *
                                         FROM @schema_cdm.CONCEPT_ANCESTOR
                                         WHERE ancestor_concept_id IN (@dm)",
                              schema_cdm = cdm_schema,
                              dm = vec_dm_exc)
diab_exc_def <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                            sql = diab_exc_sql)

diab_exc2_sql <- SqlRender::render(sql = "SELECT *
                                          FROM @schema_cdm.CONCEPT_ANCESTOR
                                          WHERE ancestor_concept_id IN (@dm)",
                                   schema_cdm = cdm_schema,
                                   dm = vec_dm_exc2)
diab_exc2_def <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                             sql = diab_exc2_sql)

# End-stage kidney disease
## End stage renal disease
vec_eskd_cod <- c(443611, 193782, 443919, 45887996, 2617395, 2617396, 44786436, 2617401, 2617405,
                  2617397, 2617404, 2617403, 2617400, 2617402, 2617399, 2617398)
## Dialysis
vec_dial <- c(4090651, 4032243, 45889365, 4027133, 38003431)
## Transplatation
vec_trans <- c(199991, 42539502, 4324887) #c(1576284, 42539502)#4300185, 44797545, 1576284
## eGFR < 15 (més endavant)
vec_eskd <- c(vec_eskd_cod, vec_dial, vec_trans)
eskd_sql <- SqlRender::render(sql = "SELECT *
                                     FROM @schema_cdm.CONCEPT_ANCESTOR
                                     WHERE ancestor_concept_id IN (@eskd)",
                              schema_cdm = cdm_schema,
                              eskd = vec_eskd)
eskd_def <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                        sql = eskd_sql)

depress_sql <- SqlRender::render(sql = "SELECT *
                                        FROM @schema_cdm.CONCEPT_ANCESTOR
                                        WHERE ancestor_concept_id = 440383",
                                 schema_cdm = cdm_schema)
depress_def <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                           sql = depress_sql)

esquizo_sql <- SqlRender::render(sql = "SELECT *
                                        FROM @schema_cdm.CONCEPT_ANCESTOR
                                        WHERE ancestor_concept_id = 435783",
                                 schema_cdm = cdm_schema)
esquizo_def <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                           sql = esquizo_sql)

epilepsia_sql <- SqlRender::render(sql = "SELECT *
                                          FROM @schema_cdm.CONCEPT_ANCESTOR
                                          WHERE ancestor_concept_id = 380378",
                                   schema_cdm = cdm_schema)
epilepsia_def <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                             sql = epilepsia_sql)

tumor_sql <- SqlRender::render(sql = "SELECT *
                                      FROM @schema_cdm.CONCEPT_ANCESTOR
                                      where ancestor_concept_id IN (443392, 4144289)",
                               schema_cdm = cdm_schema)
tumor_def <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                         sql = tumor_sql)

drug_sql <- SqlRender::render(sql = "SELECT *
                                     FROM @schema_cdm.CONCEPT_ANCESTOR
                                     WHERE ancestor_concept_id = 21602722",
                              schema_cdm = cdm_schema)
drug_def <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                        sql = drug_sql)

A10A_sql <- SqlRender::render(sql = "SELECT *
                                     FROM @schema_cdm.CONCEPT_ANCESTOR
                                     WHERE ancestor_concept_id = 21600713",
                             schema_cdm = cdm_schema)
A10A_def <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                        sql = A10A_sql)

C10_sql <- SqlRender::render(sql = "SELECT *
                                    FROM @schema_cdm.CONCEPT_ANCESTOR
                                    WHERE ancestor_concept_id = 21601853",
                              schema_cdm = cdm_schema)
C10_def <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                       sql = C10_sql)

antihtn_sql <- SqlRender::render(sql = "SELECT *
                                        FROM @schema_cdm.CONCEPT_ANCESTOR
                                        WHERE ancestor_concept_id IN (@antihtn)",
                                 schema_cdm = cdm_schema,
                                 antihtn = c(21600381, 21601664, 21601744, 21601782))
antihtn_def <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                           sql = antihtn_sql)

# Agafem tots els diagnòstics de DM-T2
diab_cond_sql <- SqlRender::render(sql = "SELECT *
                                          FROM @schema_cdm.CONDITION_OCCURRENCE
                                          WHERE condition_concept_id IN (@diab_def)",
                                   schema_cdm = cdm_schema,
                                   diab_def = setdiff(unique(diab_def$DESCENDANT_CONCEPT_ID),
                                                      unique(diab_exc_def$DESCENDANT_CONCEPT_ID)))
                                   # diab_def = unique(diab_def$DESCENDANT_CONCEPT_ID))
diab_cond <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                         sql = diab_cond_sql)
diab_cond <- data.table::as.data.table(diab_cond)
if (is(diab_cond$CONDITION_START_DATE)[1] != "Date"){
  diab_cond[, CONDITION_START_DATE := as.Date(CONDITION_START_DATE, origin = '1970-01-01')]
}
## Ens quedem el primer diagnòstic
diab_cond <- diab_cond[order(PERSON_ID, CONDITION_START_DATE)]
diab_cond_1r <- diab_cond[, lapply(.SD, first),
                          keyby = .(PERSON_ID)]
diab_cond_1r[, diab_1r := CONDITION_START_DATE]
# diab_cond_1r <- diab_cond[, .(diab_1r  = min(CONDITION_START_DATE)),
#                           keyby = .(PERSON_ID)]
diab_cond_1r <- diab_cond_1r[, .(PERSON_ID, diab_1r,
                                 SOURCE_VALUE = CONDITION_SOURCE_VALUE)]
sel_id <- diab_cond_1r$PERSON_ID

diab_obs_sql <- SqlRender::render(sql = "SELECT *
                                         FROM @schema_cdm.OBSERVATION
                                         WHERE observation_concept_id IN (@diab_def)",
                                  schema_cdm = cdm_schema,
                                  diab_def = unique(diab_def$DESCENDANT_CONCEPT_ID))
diab_obs <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                        sql = diab_obs_sql)
diab_obs <- data.table::as.data.table(diab_obs)
if(0 < diab_obs[,.N]){
  diab_obs <- diab_obs[order(PERSON_ID, OBSERVATION_DATE)]
  diab_obs_1r <- diab_obs[, lapply(.SD, first),
                          keyby = .(PERSON_ID)]
  diab_obs_1r[, diab_1r := CONDITION_START_DATE]
  sel_id <- c(sel_id, diab_obs_1r$PERSON_ID)
  diab_cond_1r <- data.table::rbindlist(list(diab_cond_1r,
                                             diab_obs_1r[, .(PERSON_ID, diab_1r,
                                                             SOURCE_VALUE = OBSERVATION_SOURCE_VALUE)]))
}

diab_cond_1r <- unique(diab_cond_1r)
sel_id <- unique(sel_id)

# Baixem la informació demogràfica
diab_pers_sql <- SqlRender::render(sql = "SELECT * FROM @schema_cdm.PERSON where person_id IN (@p)",
                                   schema_cdm = cdm_schema,
                                   p = sel_id)
diab_pers <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                         sql = diab_pers_sql)
diab_pers <- data.table::as.data.table(diab_pers)

flow_chart <- data.table::data.table(pas = c('Origen', 'DM'),
                                     N = c(aux, diab_pers[, .N]))

diabT1_cond_sql <- SqlRender::render(sql = "SELECT *
                                            FROM @schema_cdm.CONDITION_OCCURRENCE
                                            WHERE condition_concept_id IN (@diab_def)",
                                     schema_cdm = cdm_schema,
                                     diab_def = unique(diab_exc_def$DESCENDANT_CONCEPT_ID))
diabT1_cond <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                           sql = diabT1_cond_sql)
diabT1_cond <- data.table::as.data.table(diabT1_cond)

diab_pers <- diab_pers[!(PERSON_ID %in% diabT1_cond$PERSON_ID)]
flow_chart <- data.table::rbindlist(list(flow_chart,
                                         data.table::data.table(pas = 'Excloem els DMI',
                                                                N = diab_pers[, .N])))

# Ens quedem amb els diagnosticats posteriors als 35 anys.
diab_cond_1r <- data.table::merge.data.table(x = diab_cond_1r,
                                             y = diab_pers[, .(PERSON_ID, YEAR_OF_BIRTH, MONTH_OF_BIRTH, DAY_OF_BIRTH)],
                                             by = "PERSON_ID")
diab_cond_1r[, `:=`(diab_1r = as.Date(diab_1r, format = '%Y-%m-%d'),
                    dbirth = as.Date(sprintf('%i-%i-%i', YEAR_OF_BIRTH, MONTH_OF_BIRTH,
                                             DAY_OF_BIRTH), format = '%Y-%m-%d'))]
diab_cond_1r[, age := as.numeric(diab_1r - dbirth)/365.25]
diab_cond_1r <- diab_cond_1r[35 <= age]

flow_chart <- data.table::rbindlist(list(flow_chart,
                                         data.table::data.table(pas = 'Diagnostic posterior 35 anys',
                                                                N = diab_cond_1r[, .N])))

# Criteris d'exclusió condicions
exc_cond_sql <- SqlRender::render("SELECT *
                                   FROM @schema_cdm.CONDITION_OCCURRENCE
                                   WHERE condition_concept_id IN (@excl_def) AND
                                         person_id IN (@p)",
                                  schema_cdm = cdm_schema,
                                  excl_def = c(eskd_def$DESCENDANT_CONCEPT_ID,
                                               depress_def$DESCENDANT_CONCEPT_ID,
                                               esquizo_def$DESCENDANT_CONCEPT_ID,
                                               epilepsia_def$DESCENDANT_CONCEPT_ID,
                                               tumor_def$DESCENDANT_CONCEPT_ID),
                                  p = unique(diab_cond_1r$PERSON_ID))
exc_cond <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                        sql = exc_cond_sql)
exc_cond <- data.table::as.data.table(exc_cond)
# ## Ens quedem el primer diagnòstic
# if(is(exc_cond$CONDITION_START_DATE)[1] != 'Date'){
#   exc_cond[, CONDITION_START_DATE := as.Date(CONDITION_START_DATE,
#                                              origin = '1970-01-01')]
# }
exc_cond_1r <- exc_cond[, .(exc_1r  = min(CONDITION_START_DATE)),
                        keyby = .(PERSON_ID)]
diab_cond_1r <- merge(diab_cond_1r,
                      exc_cond_1r,
                      by = "PERSON_ID",
                      all.x = TRUE)
# actualitzem la data màxima d'entrada
# diab_cond_1r[, max_date := pmin(max_date, exc_1r, na.rm = T)]
# diab_cond_1r <- diab_cond_1r[min_date < max_date]
diab_cond_1r <- diab_cond_1r[is.na(exc_1r) | diab_1r < exc_1r]

# diab_cond_1r <- diab_cond_1r[is.na(exc_1r) | diab_1r < exc_1r]
flow_chart <- data.table::rbindlist(list(flow_chart,
                                         data.table::data.table(pas = "Fora exclusions (condicions) a l'entrada",
                                                                N = diab_cond_1r[, .N])))

exc_drug_sql <- SqlRender::render("SELECT *
                                   FROM @schema_cdm.DRUG_EXPOSURE
                                   WHERE drug_concept_id IN (@excl_def) AND person_id IN (@p)",
                                  schema_cdm = cdm_schema,
                                  excl_def = drug_def$DESCENDANT_CONCEPT_ID,
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
# actualitzem la data màxima d'entrada
# diab_cond_1r[, max_date := pmin(max_date, exc_drug_1r, na.rm = T)]
# diab_cond_1r <- diab_cond_1r[min_date < max_date]
diab_cond_1r <- diab_cond_1r[is.na(exc_drug_1r) | diab_1r < exc_drug_1r]
flow_chart <- data.table::rbindlist(list(flow_chart,
                                         data.table::data.table(pas = "Fora exclusions (drug) a l'entrada",
                                                                N = diab_cond_1r[, .N])))

exc_A10A_sql <- SqlRender::render("SELECT *
                                   FROM @schema_cdm.DRUG_EXPOSURE
                                   WHERE drug_concept_id IN (@excl_def) AND person_id IN (@p)",
                                  schema_cdm = cdm_schema,
                                  excl_def = A10A_def$DESCENDANT_CONCEPT_ID,
                                  p = unique(diab_cond_1r$PERSON_ID))
exc_A10A <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                        sql = exc_A10A_sql)
## Ens quedem el primer diagnòstic
exc_A10A <- data.table::as.data.table(exc_A10A)
exc_A10A <- merge(exc_A10A,
                  diab_cond_1r[, .(PERSON_ID, diab_1r)],
                  by = 'PERSON_ID')
exc_A10A <- exc_A10A[diab_1r <= DRUG_EXPOSURE_START_DATE &
                       DRUG_EXPOSURE_START_DATE <= (diab_1r + 365/2)]
diab_cond_1r <- diab_cond_1r[!(PERSON_ID %in% unique(exc_A10A$PERSON_ID))]
# diab_cond_1r <- diab_cond_1r[is.na(exc_drug_1r) | diab_1r < exc_drug_1r]
flow_chart <- data.table::rbindlist(list(flow_chart,
                                         data.table::data.table(pas = "Fora insulina primers 6m",
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
                      by = "PERSON_ID",
                      all.x = TRUE)
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
                    max_date = pmin(DEATH_DATE, OBSERVATION_PERIOD_END_DATE,
                                    as.Date('2020-12-31'), na.rm = TRUE))]
# Ens quedem als que tenen un periode vàlid
diab_cond_1r <- diab_cond_1r[min_date < max_date]
flow_chart <- data.table::rbindlist(list(flow_chart,
                                         data.table::data.table(pas = 'Dins el periode estudi',
                                                                N = diab_cond_1r[, .N])))
# Mesuras
meas_sql <- SqlRender::render(sql = "SELECT *
                                     FROM @schema_cdm.MEASUREMENT
                                     WHERE measurement_concept_id IN (@dm) AND
                                           person_id IN (@p)",
                              schema_cdm = cdm_schema,
                              dm = c(3020460, #PCR
                                     3001122, #ferritin
                                     3010813, #LEUC_N
                                     3024561, #ALBUM_ser
                                     3016723, #CREATININA
                                     40764999,#CKDEPI
                                     3034639, #HBA1C
                                     3004501, #GLICEMIA
                                     4245997, #BMI
                                     3036277, #Body height
                                     4099154),#Body weight
                              p = unique(diab_cond_1r$PERSON_ID))
meas <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                    sql = meas_sql)
meas <- data.table::as.data.table(meas)
meas <- merge(meas,
              diab_cond_1r[, .(PERSON_ID, min_date)],
              by = 'PERSON_ID')
meas <- meas[(min_date - 365.25*1.5) <= MEASUREMENT_DATE & MEASUREMENT_DATE <= min_date]
meas_short <- data.table::dcast(data = meas,
                                formula = PERSON_ID ~ MEASUREMENT_CONCEPT_ID,
                                fun.aggregate = length)

# drug_sql <- SqlRender::render("SELECT *
#                                FROM @schema_cdm.DRUG_EXPOSURE
#                                WHERE drug_concept_id IN (@drug_def) AND person_id IN (@p)",
#                               schema_cdm = cdm_schema,
#                               drug_def = c(unique(C10_def$DESCENDANT_CONCEPT_ID),
#                                            unique(antihtn_def$DESCENDANT_CONCEPT_ID)),
#                               p = unique(diab_cond_1r$PERSON_ID))
# drug <- DatabaseConnector::querySql(connection = cdm_bbdd,
#                                     sql = drug_sql)
# ## Ens quedem el primer diagnòstic
# drug <- data.table::as.data.table(drug)
# drug <- merge(drug,
#               diab_cond_1r[, .(PERSON_ID, min_date)],
#               by = 'PERSON_ID')
# drug <- drug[DRUG_EXPOSURE_START_DATE <= min_date]
# drug_short <- data.table::dcast(data = drug,
#                                 formula = PERSON_ID ~ DRUG_CONCEPT_ID,
#                                 fun.aggregate = length)

# Creem una nova bbdd amb la taula cohort
diab_cond_1r <- diab_cond_1r[order(PERSON_ID)]
# diab_cond_1r[min_date == diab_1r,
#              min_date := min_date + 1]
cohort <- diab_cond_1r[, .(cohort_definition_id = 1,
                           cohort_start_date = min_date, #as.POSIXct(min_date),
                           cohort_end_date = max_date, #as.POSIXct(max_date),
                           subject_id = PERSON_ID)]
# cohort[, cohort_definition_id := cumsum(cohort_definition_id)]

source_value <- diab_cond_1r[PERSON_ID %in% cohort$SUBJECT_ID,
                             .N,
                             keyby = .(SOURCE_VALUE)]

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
  includedCovariateConceptIds = c(vec_dm, vec_dm_exc),
  addDescendantsToInclude = TRUE)#,

settings_m <- FeatureExtraction::convertPrespecSettingsToDetailedSettings(covariateSettings_m)
covariateData_m <- FeatureExtraction::getDbCovariateData(connection = cdm_bbdd,
                                                         cdmDatabaseSchema = cdm_schema,
                                                         cohortTable = "cohort",
                                                         cohortDatabaseSchema = results,
                                                         cohortId = -1,
                                                         # rowIdField = "subject_id",
                                                         covariateSettings = covariateSettings_m)
# FeatureExtraction::saveCovariateData(covariateData_m, "covariates_m")
covariateData_m2 <- FeatureExtraction::aggregateCovariates(covariateData_m)

save(flow_chart, source_value, meas_short, covariateData2, covariateData_m2, #drug_short,
     "results_prov.RData")

# DMII_sql <- SqlRender::render(sql = "SELECT *
#                                      FROM @schema_cdm.CONCEPT_ANCESTOR
#                                      WHERE ancestor_concept_id = 201826",
#                               schema_cdm = cdm_schema)
# DMII_def <- DatabaseConnector::querySql(connection = cdm_bbdd,
#                                         sql = DMII_sql)
# DMII_cond_sql <- SqlRender::render(sql = "SELECT COUNT(DISTINCT PERSON_ID)
#                                           FROM @schema_cdm.CONDITION_OCCURRENCE
#                                           WHERE condition_concept_id IN (@diab_def) AND person_id IN (@p)",
#                                    schema_cdm = cdm_schema,
#                                    diab_def = unique(DMII_def$DESCENDANT_CONCEPT_ID),
#                                    p = unique(cohort$SUBJECT_ID))
# DatabaseConnector::querySql(connection = cdm_bbdd,
#                             sql = DMII_cond_sql)
#
# DM_sql <- SqlRender::render(sql = "SELECT *
#                                    FROM @schema_cdm.CONCEPT_ANCESTOR
#                                    WHERE ancestor_concept_id = 201820",
#                               schema_cdm = cdm_schema)
# DM_def <- DatabaseConnector::querySql(connection = cdm_bbdd,
#                                       sql = DM_sql)
# DM_cond_sql <- SqlRender::render(sql = "SELECT COUNT(DISTINCT PERSON_ID)
#                                         FROM @schema_cdm.CONDITION_OCCURRENCE
#                                         WHERE condition_concept_id IN (@diab_def) AND person_id IN (@p)",
#                                    schema_cdm = cdm_schema,
#                                    diab_def = unique(DM_def$DESCENDANT_CONCEPT_ID),
#                                    p = unique(cohort$SUBJECT_ID))
# DatabaseConnector::querySql(connection = cdm_bbdd,
#                             sql = DM_cond_sql)
