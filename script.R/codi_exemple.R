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

cohortInfo <- CreateSQL_T2DM(cdm_bbdd,
                             cdm_schema,
                             results_sc,
                             cohortTable)
outcomeInfo <- CreateSQL_T2DM_outcome(cdm_bbdd,
                                      cdm_schema,
                                      results_sc,
                                      cohortTable)

cohortDefinitionSet <- data.frame(atlasId = rep(NA, 2),
                                  cohortId = 1:2,
                                  cohortName = c("SIDIAP T2DM-WP5: Entrada",
                                                 "SIDIAP T2DM-WP5: Outcome"),
                                  sql = c(cohortInfo$ohdiSQL,
                                          outcomeInfo$ohdiSQL),
                                  json = c(cohortInfo$circeJson,
                                           outcomeInfo$circeJson),
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
CohortDiagnostics::launchDiagnosticsExplorer(dataFolder = paste0('~idiap/projects/SOPHIA_codi/',exportFolder),
                                             dataFile = "Premerged.RData")

###################################################################################################
# Descriptiva i creació de les covariables
# covariateSettings <- createDefaultCovariateSettings()
# covariateSettings <- createDefaultTemporalCovariateSettings()
covDemo <- FeatureExtraction::createCovariateSettings(
  useDemographicsGender = TRUE,
  useDemographicsAge = TRUE,
  # useDemographicsAgeGroup = TRUE,
  # useDemographicsIndexYear = TRUE,
  useDemographicsPriorObservationTime = TRUE,
  useDemographicsPostObservationTime = TRUE,
  useDemographicsTimeInCohort = TRUE)
  # useConditionOccurrenceAnyTimePrior = TRUE)
  # useDrugExposureAnyTimePrior = TRUE,
  # useProcedureOccurrenceAnyTimePrior = TRUE,
  # useMeasurementAnyTimePrior = TRUE)#,
  # useMeasurementLongTerm = TRUE,
  # useMeasurementValueAnyTimePrior = TRUE)#,
  # useMeasurementValueLongTerm = TRUE,
  # longTermStartDays = 2*(-365.25),
  # endDays = 0)
covariateSettings2 <- FeatureExtraction::convertPrespecSettingsToDetailedSettings(covDemo)

BMI_conceptId <- c(3038553, 40762638) #LOINC
height_conceptId <- c(3036277, 3015514) #LOINC
weight_conceptId <- c(3025315) #LOINC
covMeasValueAny <- FeatureExtraction::createCovariateSettings(
  useMeasurementValueAnyTimePrior = TRUE,
  includedCovariateConceptIds = c(BMI_conceptId,
                                  height_conceptId,
                                  weight_conceptId),
  addDescendantsToInclude = TRUE)

SBP_conceptId <- c(3004249, #LOINC
                   4152194) #SNOMED
DBP_conceptId <- c(3012888, #LOINC
                   4154790) #SNOMED
cT_conceptId <- c(3019900, 3027114) #LOINC
cHDL_conceptId <- c(3011884, 3007070, 3023602) #LOINC
cLDL_conceptId <- c(3028437, 3001308) #LOINC
cVLDL_conceptId <- c(3022487) #LOINC
Tg_conceptId <- c(3022192, 42868692)  #LOINC
glu_conceptId <- c(3004501,
                   46235168, 40757523, 3005834, 40757527, 3016567, 40757528, 40757529,
                   40757627, 40757628,
                   3018251, 46236948,
                   3015024, 3036895, 3001022, 3016701, 3018582, 3008799, 3045700,
                   3020491) #LOINC
alt_conceptId <- c(3006923, 46235106)
CRP_conceptId <- c(3020460) #LOINC
ferritin_conceptId <- c(3001122) #LOINC
WBC_conceptId <- c(3010813) #LOINC
neutrophils_conceptId <- c(3017732, 3046321, 43055364) #LOINC
basophils_conceptId <- c(3006315, 43055368) #LOINC
eosinophils_conceptId <- c(3013115, 43055367) #LOINC
monocytes_conceptId <- c(3001604, 43055365) #LOINC
lymphocytes_conceptId <- c(3019198, 43055366) #LOINC
HbA1c_conceptId <- c(3034639, 3004410) #LOINC
covMeasValueLong <- FeatureExtraction::createCovariateSettings(
  useMeasurementValueLongTerm = TRUE,
  longTermStartDays = 2*(-365.25),
  endDays = 0,
  includedCovariateConceptIds = c(BMI_conceptId,
                                  height_conceptId,
                                  weight_conceptId,
                                  SBP_conceptId,
                                  DBP_conceptId,
                                  cT_conceptId,
                                  cHDL_conceptId,
                                  cLDL_conceptId,
                                  cVLDL_conceptId,
                                  Tg_conceptId,
                                  glu_conceptId,
                                  alt_conceptId,
                                  CRP_conceptId,
                                  ferritin_conceptId,
                                  WBC_conceptId,
                                  neutrophils_conceptId,
                                  basophils_conceptId,
                                  eosinophils_conceptId,
                                  monocytes_conceptId,
                                  lymphocytes_conceptId,
                                  HbA1c_conceptId),
  addDescendantsToInclude = TRUE)

T2DM_vars <- FeatureExtraction::createAnalysisDetails(
  analysisId = 111,
  sqlFileName = "DomainConcept.sql",
  parameters = list(analysisId = 111,
                    analysisName = "T2DM",
                    startDay = "anyTimePrior",
                    endDay = 0,
                    subType = "all",
                    domainId = "Condition",
                    domainTable = "condition_occurrence",
                    domainConceptId = "condition_concept_id",
                    domainStartDate = "condition_start_date",
                    domainEndDate = "condition_start_date"),
  includedCovariateConceptIds = c(201820, 442793, 443238),
  addDescendantsToInclude = TRUE,
  excludedCovariateConceptIds = c(201254, 435216, 4058243, 40484648,195771, 761051),
  addDescendantsToExclude = TRUE)

obesity_vars <- FeatureExtraction::createAnalysisDetails(
  analysisId = 112,
  sqlFileName = "DomainConcept.sql",
  parameters = list(analysisId = 112,
                    analysisName = "Angina pectoris",
                    startDay = "anyTimePrior",
                    endDay = 0,
                    subType = "all",
                    domainId = "Condition",
                    domainTable = "condition_occurrence",
                    domainConceptId = "condition_concept_id",
                    domainStartDate = "condition_start_date",
                    domainEndDate = "condition_start_date"),
  includedCovariateConceptIds = c(433736),
  addDescendantsToInclude = TRUE)

angor_vars <- FeatureExtraction::createAnalysisDetails(
  analysisId = 113,
  sqlFileName = "DomainConcept.sql",
  parameters = list(analysisId = 113,
                    analysisName = "Angina pectoris",
                    startDay = "anyTimePrior",
                    endDay = 0,
                    subType = "all",
                    domainId = "Condition",
                    domainTable = "condition_occurrence",
                    domainConceptId = "condition_concept_id",
                    domainStartDate = "condition_start_date",
                    domainEndDate = "condition_start_date"),
  includedCovariateConceptIds = c(321318),
  addDescendantsToInclude = TRUE)

ami_vars <- FeatureExtraction::createAnalysisDetails(
  analysisId = 114,
  sqlFileName = "DomainConcept.sql",
  parameters = list(analysisId = 114,
                    analysisName = "AMI",
                    startDay = "anyTimePrior",
                    endDay = 0,
                    subType = "all",
                    domainId = "Condition",
                    domainTable = "condition_occurrence",
                    domainConceptId = "condition_concept_id",
                    domainStartDate = "condition_start_date",
                    domainEndDate = "condition_start_date"),
  includedCovariateConceptIds = c(312327),
  addDescendantsToInclude = TRUE)

stroke_vars <- FeatureExtraction::createAnalysisDetails(
  analysisId = 115,
  sqlFileName = "DomainConcept.sql",
  parameters = list(analysisId = 115,
                    analysisName = "Stroke",
                    startDay = "anyTimePrior",
                    endDay = 0,
                    subType = "all",
                    domainId = "Condition",
                    domainTable = "condition_occurrence",
                    domainConceptId = "condition_concept_id",
                    domainStartDate = "condition_start_date",
                    domainEndDate = "condition_start_date"),
  includedCovariateConceptIds = c(43530727, 443454),
  addDescendantsToInclude = TRUE)

TIA_vars <- FeatureExtraction::createAnalysisDetails(
  analysisId = 116,
  sqlFileName = "DomainConcept.sql",
  parameters = list(analysisId = 116,
                    analysisName = "TIA",
                    startDay = "anyTimePrior",
                    endDay = 0,
                    subType = "all",
                    domainId = "Condition",
                    domainTable = "condition_occurrence",
                    domainConceptId = "condition_concept_id",
                    domainStartDate = "condition_start_date",
                    domainEndDate = "condition_start_date"),
  includedCovariateConceptIds = c(373503, 381591),
  addDescendantsToInclude = TRUE)

covariateSettings <- list(covDemo,
                          covMeasValueAny,
                          covMeasValueLong,
                          FeatureExtraction::createDetailedCovariateSettings(
                            list(T2DM_vars,
                                 obesity_vars,
                                 angor_vars,
                                 ami_vars,
                                 stroke_vars,
                                 TIA_vars)))

covariateData <- FeatureExtraction::getDbCovariateData(
  connection = cdm_bbdd,
  cdmDatabaseSchema = cdm_schema,
  cohortDatabaseSchema = results_sc,
  cohortTable = cohortTable,
  cohortId = 1,
  rowIdField = "subject_id",
  covariateSettings = covariateSettings)
FeatureExtraction::summary(covariateData)
bbdd_covar <- covariateData$covariates %>%
  dplyr::collect() %>%
  dplyr::mutate(variable = as.character(NA),
                variable = dplyr::if_else(covariateId == 1002, 'age', variable),
                variable = dplyr::if_else(covariateId == 1008, 'time_prior', variable),
                variable = dplyr::if_else(covariateId == 1009, 'time_post', variable),
                variable = dplyr::if_else(covariateId == 1010, 'time_between', variable),
                variable = dplyr::if_else(covariateId == 8507001, 'sex_male', variable),
                variable = dplyr::if_else(covariateId == 8532001, 'sex_female', variable),
                variable = dplyr::if_else(stringr::str_sub(covariateId,
                                                    start = -3L) == 111,
                                   'T2DM', variable),
                variable = dplyr::if_else(stringr::str_sub(covariateId,
                                                           start = -3L) == 112,
                                          'obesity', variable),
                variable = dplyr::if_else(stringr::str_sub(covariateId,
                                                           start = -3L) == 113,
                                          'angor', variable),
                variable = dplyr::if_else(stringr::str_sub(covariateId,
                                                           start = -3L) == 114,
                                          'ami', variable),
                variable = dplyr::if_else(stringr::str_sub(covariateId,
                                                           start = -3L) == 115,
                                          'stroke', variable),
                variable = dplyr::if_else(stringr::str_sub(covariateId,
                                                           start = -3L) == 116,
                                          'tia', variable),
                variable = dplyr::if_else(covariateId == 3038553531705, 'BMI', variable),
                variable = dplyr::if_else(covariateId == 3036277582705, 'height', variable),
                variable = dplyr::if_else(covariateId == 3025315529705, 'weight', variable),
                variable = dplyr::if_else(covariateId == 3038553531706, 'BMI_long', variable),
                variable = dplyr::if_else(covariateId == 3036277582706, 'height_long', variable),
                variable = dplyr::if_else(covariateId == 3025315529706, 'weight_long', variable),
                variable = dplyr::if_else(covariateId %in% c(3004249323706, 4152194876706),
                                          'SBP', variable),
                variable = dplyr::if_else(covariateId %in% c(3012888323706, 4154790876706),
                                          'DBP', variable),
                variable = dplyr::if_else(covariateId == 3027114840706, 'cT', variable),
                variable = dplyr::if_else(covariateId == 3011884840706, 'cHDL', variable),
                variable = dplyr::if_else(covariateId == 3028437840706, 'cLDL', variable),
                variable = dplyr::if_else(covariateId == 3022192840706, 'Tg', variable),
                variable = dplyr::if_else(covariateId == 3004501840706,'Glucose', variable),
                variable = dplyr::if_else(covariateId == 3006923645706, 'ALT', variable),
                variable = dplyr::if_else(covariateId == 3020460751706, 'CRP', variable),
                variable = dplyr::if_else(covariateId == 3001122748706, 'Ferritin', variable)) %>%
  dplyr::group_by(rowId, variable) %>%
  dplyr::summarise(covariateValue = mean(covariateValue),
                   .groups = 'keep') %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(id_cols = rowId,
                     names_from = variable,
                     values_from = covariateValue)
tidyCovariates <- tidyCovariateData(covariateData,
                                    minFraction = 0.001,
                                    normalize = FALSE,
                                    removeRedundancy = TRUE)
covariateData2 <- FeatureExtraction::aggregateCovariates(covariateData)
FeatureExtraction::summary(covariateData2)
covariateData2$covariates
covariateData2$covariatesContinuous

# sel_id <- c(3020460751706, #C reactive protein [Mass/volume] in Serum or Plasma (milligram per liter)
#             3001122748706, #Ferritin [Mass/volume] in Serum or Plasma (microgram per liter)
#             3010813848706, #Leukocytes [#/volume] in Blood (thousand per microliter)
#             3024561713706, #Albumin [Mass/volume] in Serum or Plasma (gram per deciliter)
#             3016723840706, #Creatinine [Mass/volume] in Serum or Plasma (milligram per deciliter)
#             40764999117706, #Glomerular filtration rate/1.73 sq M.predicted [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (CKD-EPI) (milliliter per minute per 1.73 square meter)
#             46236952117706, #Glomerular filtration rate/1.73 sq M.predicted [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (MDRD) (milliliter per minute per 1.73 square meter)
#             3034639554706, #Hemoglobin A1c [Mass/volume] in Blood (percent)
#             3004501840706, #Glucose [Mass/volume] in Serum or Plasma (milligram per deciliter)
#             3038553531706, #Body mass index (BMI) [Ratio] (kilogram per square meter)
#             3036277582706, #Body height (centimeter)
#             3025315529706, #Body weight (kilogram)
#             4152194876706, #Systolic blood pressure (millimeter mercury column)
#             4154790876706, #Diastolic blood pressure (millimeter mercury column)
#             3028437840706, #Cholesterol in LDL [Mass/volume] in Serum or Plasma (milligram per deciliter)
#             3011884840706, #Cholesterol in HDL [Presence] in Serum or Plasma (milligram per deciliter)
#             3027114840706, #Cholesterol [Mass/volume] in Serum or Plasma (milligram per deciliter)
#             3022192840706, #Triglyceride [Mass/volume] in Serum or Plasma (milligram per deciliter)
#             3006923645706, #Alanine aminotransferase [Enzymatic activity/volume] in Serum or Plasma (unit per liter)
#             3038553531705, #Body mass index (BMI) [Ratio] (kilogram per square meter)
#             3036277582705, #Body height (centimeter)
#             3025315529705, #Body weight (kilogram)
#             1002 # Age
# )
# sel_id <- as.character(sel_id)
flextable::flextable(covariateData2$covariateRef %>%
                       dplyr::inner_join(covariateData2$covariatesContinuous) %>%
                       dplyr::mutate(covariateId = as.character(floor(covariateId))) %>%
                       # filter(covariateId %in% sel_id) %>%
                       dplyr::select(-covariateId, -analysisId, -conceptId) %>%
                       dplyr::collect())

sel_obes <- bbdd_covar %>%
  dplyr::filter((obesity & !is.na(obesity)) | (30 <= BMI & !is.na(BMI))) %>%
  dplyr::pull(rowId)
covariateData_obes <- covariateData
covariateData_obes$covariates <- covariateData_obes$covariates %>%
  dplyr::filter(rowId %in% sel_obes)
covariateData2_obes <- FeatureExtraction::aggregateCovariates(covariateData_obes)
flextable::flextable(covariateData2_obes$covariateRef %>%
                       dplyr::inner_join(covariateData2_obes$covariatesContinuous) %>%
                       dplyr::mutate(covariateId = as.character(floor(covariateId))) %>%
                       # filter(covariateId %in% sel_id) %>%
                       dplyr::select(-covariateId, -analysisId, -conceptId) %>%
                       dplyr::collect())


result <- createTable1(covariateData2, output = 'one column')
View(result)
print(result, row.names = FALSE, right = FALSE)

disconnect(cdm_bbdd)
