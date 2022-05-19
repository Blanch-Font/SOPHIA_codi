# Descriptiva i creació de les covariables
# covariateSettings <- createDefaultCovariateSettings()
# covariateSettings <- createDefaultTemporalCovariateSettings()
covDemo <- FeatureExtraction::createCovariateSettings(
  useDemographicsGender = TRUE,
  useDemographicsAge = TRUE,
  # useDemographicsAgeGroup = TRUE,
  # useDemographicsIndexYear = TRUE,
  # useDemographicsPriorObservationTime = TRUE,
  # useDemographicsPostObservationTime = TRUE,
  # useDemographicsTimeInCohort = TRUE#,
  # useObservationAnyTimePrior = TRUE,
  # useObservationLongTerm = TRUE,
  # useObservationMediumTerm = TRUE,
  # useObservationShortTerm = TRUE
)
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
  includedCovariateConceptIds = c(#BMI_conceptId,
    #height_conceptId,
    #weight_conceptId,
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
                    analysisName = "Obesity",
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

A10_conceptId <- c(21600712,
                   782681, 793321, 1502829, 1502830, 1503327, 1525221, 1529352,
                   1547554, 1596977, 1597761, 1597772, 1597773, 1597781, 1597792,
                   19006931, 19021312, 19023424, 19023425, 19023426, 19029030, 19029061,
                   19058398, 19059800, 19077638, 19077682, 19078552, 19078559, 19079293,
                   19079465, 19095211, 19095212, 19099055, 19101729, 19112791, 19125041,
                   19125045, 19125049, 19129179, 19133793, 19135264, 21022404, 21036596,
                   21061594, 21061613, 21076306, 21081251, 21086042, 21100924, 21114195,
                   21133671, 21169719, 35408233, 35410536, 35412102, 35412890, 35412958,
                   36403507, 36403509, 36884964, 40044221, 40051377, 40052768, 40054707,
                   40139098, 40164885, 40164888, 40164891, 40164897, 40164913, 40164916,
                   40164942, 40164943, 40164946, 40166037, 40166041, 40239218, 42479624,
                   42479783, 42481504, 42481541, 42482012, 42482588, 42656231, 42656236,
                   42656240, 42708086, 42708090, 42899447, 42902356, 42902587, 42902742,
                   42902821, 42902945, 42902992, 42903059, 42903341, 43013885, 43013905,
                   43013911, 43013918, 43013924, 43013928, 43526467, 43526471, 44032735,
                   44058584, 44123708, 44785831, 45774709, 45774754, 45774893, 46233969,
                   46234047, 46234234, 46234237, 46287408, 46287689)

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
covDrug <- FeatureExtraction::createCovariateSettings(
  # useDrugExposureLongTerm = TRUE,
  # useDrugExposureMediumTerm = TRUE,
  # useDrugEraAnyTimePrior = TRUE,
  # useDrugEraLongTerm = TRUE,
  # useDrugEraMediumTerm = TRUE,
  # useDrugEraShortTerm = TRUE,
  # useDrugEraOverlapping = TRUE,
  # useDrugGroupEraAnyTimePrior = TRUE,
  # useDrugGroupEraLongTerm = TRUE,
  useDrugGroupEraMediumTerm = TRUE,
  # useDrugGroupEraShortTerm = TRUE,
  # useDrugGroupEraOverlapping = TRUE,
  # longTermStartDays = 2*(-365.25),
  mediumTermStartDays = -365.25,
  endDays = 0,
  includedCovariateConceptIds = sel_med_conceptId,
  addDescendantsToInclude = TRUE)

smoking_vars <- FeatureExtraction::createAnalysisDetails(
  analysisId = 810,
  sqlFileName = "DomainConcept.sql",
  parameters = list(analysisId = 810,
                    analysisName = "Tobacco",
                    startDay = "anyTimePrior",
                    endDay = 1,
                    subType = "last",
                    domainId = "Observation",
                    domainTable = "observation",
                    domainConceptId = "VALUE_AS_CONCEPT_ID",
                    domainStartDate = "observation_date",
                    domainEndDate = ""),
  includedCovariateConceptIds = c(45879404, 45884037, 45883458),
  addDescendantsToInclude = TRUE)

createSmokingCovariateSettings <- function(useSmoking = TRUE){
  covariateSettings <- list(useSmoking = useSmoking)
  attr(covariateSettings, "fun") <- "getDbSmokingCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}
getDbSmokingCovariateData <- function(connection,
                                      oracleTempSchema = NULL,
                                      cdmDatabaseSchema,
                                      cohortTable = "#cohort_person",
                                      cohortId = -1,
                                      cdmVersion = "5",
                                      rowIdField = "subject_id",
                                      covariateSettings,
                                      aggregated = FALSE){
  writeLines("Constructing Smoking covariates")
  if (covariateSettings$useSmoking == FALSE) {
    return(NULL)
  }

  # Some SQL to construct the covariate:
  sql <- "SELECT DISTINCT ON (obs2.person_id)
                 obs2.person_id AS row_id,
                 obs2.value_as_concept_id AS covariate_id,
                 1 AS covariate_value
          FROM (SELECT *
                FROM @cdm_database_schema.OBSERVATION obs
                INNER JOIN @cohort_table cohort
                      ON cohort.subject_id = obs.person_id
                WHERE obs.observation_concept_id = 43054909 AND
                      obs.observation_date <= cohort.cohort_start_date AND
                      cohort.cohort_definition_id IN (@cohort_definition_id)) obs2
          ORDER BY obs2.person_id, obs2.observation_date desc"
  sql <- SqlRender::render(sql,
                           cdm_database_schema = cdmDatabaseSchema,
                           cohort_table = cohortTable, # ha de ser results_sc.cohortTable
                           cohort_definition_id = cohortId)
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"))
  # Retrieve the covariate:
  covariates <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
  # Construct covariate reference:
  covariateRef <- data.frame(covariateId = c(45879404, 45884037, 45883458),
                             covariateName = c('Never smoker', 'Current some day smoker',
                                               'Former smoker'),
                             analysisId = 500,
                             conceptId = rep(43054909, 3))
  # Construct analysis reference:
  analysisRef <- data.frame(analysisId = 500,
                            analysisName = "Smoking status",
                            domainId = "Observation",
                            startDay = NA,
                            endDay = 0,
                            isBinary = "Y",
                            missingMeansZero = "Y")
  # Construct analysis reference:
  metaData <- list(sql = sql, call = match.call())
  result <- Andromeda::andromeda(covariates = covariates,
                                 covariateRef = covariateRef,
                                 analysisRef = analysisRef)
  attr(result, "metaData") <- metaData
  class(result) <- "CovariateData"
  return(result)
}
SmokingCovSet <- createSmokingCovariateSettings(useSmoking = TRUE)
# covariates <- FeatureExtraction::getDbCovariateData(
#   connection = cdm_bbdd,
#   cdmDatabaseSchema = cdm_schema,
#   cohortDatabaseSchema = results_sc,
#   cohortTable = cohortTable,
#   cohortId = 1,
#   rowIdField = "subject_id",
#   covariateSettings = SmokingCovSet)

covariateSettings <- list(covDemo,
                          covMeasValueAny,
                          covMeasValueLong,
                          FeatureExtraction::createDetailedCovariateSettings(
                            list(T2DM_vars,
                                 obesity_vars,
                                 angor_vars,
                                 ami_vars,
                                 stroke_vars,
                                 TIA_vars)),
                          covDrug,
                          SmokingCovSet)

covariateData <- FeatureExtraction::getDbCovariateData(
  connection = cdm_bbdd,
  cdmDatabaseSchema = cdm_schema,
  cohortDatabaseSchema = results_sc,
  cohortTable = cohortTable,
  cohortId = 1,
  rowIdField = "subject_id",
  covariateSettings = covariateSettings)
FeatureExtraction::summary(covariateData)

T2DM_conceptId <- c(201530111, 201826111, 376065111, 443729111, 443731111, 443733111,
                    4193704111, 4196141111, 4221495111, 36714116111, 37016349111,
                    37017432111, 43530685111, 43530690111, 43531563111, 43531578111,
                    45770830111)
DM_conceptId <- c(442793111, 321822111, 443730111, 192279111, 4048028111, 4226798111,
                  201820111, 4008576111, 443767111)
obesity_conceptId <- covariateData$covariateRef %>%
  dplyr::filter(analysisId == 112) %>%
  dplyr::pull(covariateId)
angina_conceptId <- covariateData$covariateRef %>%
  dplyr::filter(analysisId == 113) %>%
  dplyr::pull(covariateId)
ami_conceptId <- covariateData$covariateRef %>%
  dplyr::filter(analysisId == 114) %>%
  dplyr::pull(covariateId)
stroke_conceptId <- covariateData$covariateRef %>%
  dplyr::filter(analysisId == 115) %>%
  dplyr::pull(covariateId)
tia_conceptId <- covariateData$covariateRef %>%
  dplyr::filter(analysisId == 116) %>%
  dplyr::pull(covariateId)
#
covariateData$covariates <- covariateData$covariates %>%
  dplyr::mutate(covariateId = if_else(covariateId %in% T2DM_conceptId,
                                      201826111, covariateId),
                covariateId = if_else(covariateId %in% DM_conceptId,
                                      201820111, covariateId),
                covariateId = if_else(covariateId %in% obesity_conceptId,
                                      433736112, covariateId),
                covariateId = if_else(covariateId %in% angina_conceptId,
                                      321318113, covariateId),
                covariateId = if_else(covariateId %in% ami_conceptId,
                                      312327114, covariateId),
                covariateId = if_else(covariateId %in% stroke_conceptId,
                                      443454115, covariateId),
                covariateId = if_else(covariateId %in% tia_conceptId,
                                      373503116, covariateId)) %>%
  dplyr::distinct()


tidyCovariates <- FeatureExtraction::tidyCovariateData(covariateData,
                                                       minFraction = 0.001,
                                                       normalize = FALSE,
                                                       removeRedundancy = TRUE)
covariateData2 <- FeatureExtraction::aggregateCovariates(tidyCovariates)
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

cov_cate_resum <- covariateData2$covariateRef %>%
  dplyr::filter(analysisId %in% c(411, 413) & conceptId %in% sel_med_conceptId |
                  !(analysisId %in% c(411, 413))) %>%
  dplyr::inner_join(covariateData2$covariates) %>%
  dplyr::mutate(covariateId = as.character(floor(covariateId)),
                analysisId = as.integer(analysisId),
                conceptId = as.integer(conceptId),
                sumValue = as.integer(sumValue),
                averageValue = averageValue*100) %>%
  # dplyr::arrange(analysisId, desc(averageValue)) %>%
  # filter(covariateId %in% sel_id) %>%
  # dplyr::select(-covariateId, -analysisId, -conceptId) %>%
  dplyr::collect()
flextable::flextable(cov_cate_resum) %>%
  flextable::colformat_double(x = .,
                              digits = 1)
cov_num_resum <- covariateData2$covariateRef %>%
  dplyr::inner_join(covariateData2$covariatesContinuous) %>%
  dplyr::mutate(covariateId = as.character(floor(covariateId)),
                analysisId = as.integer(analysisId),
                conceptId = as.integer(conceptId)) %>%
  # filter(covariateId %in% sel_id) %>%
  dplyr::select(-covariateId, -analysisId, -conceptId) %>%
  dplyr::collect()
flextable::flextable(cov_num_resum) %>%
  flextable::colformat_double(x = .,
                              digits = 1)

bbdd_covar <- covariateData$covariates %>%
  dplyr::collect() %>%
  dplyr::mutate(variable = as.character(NA),
                variable = dplyr::if_else(covariateId == 1002, 'age', variable),
                # variable = dplyr::if_else(covariateId == 1008, 'time_prior', variable),
                # variable = dplyr::if_else(covariateId == 1009, 'time_post', variable),
                # variable = dplyr::if_else(covariateId == 1010, 'time_between', variable),
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
                variable = dplyr::if_else(covariateId == 21600712411, 'A10', variable),
                variable = dplyr::if_else(covariateId == 21601238411, 'C01', variable),
                variable = dplyr::if_else(covariateId == 21600381411, 'C02', variable),
                variable = dplyr::if_else(covariateId == 21601461411, 'C03', variable),
                variable = dplyr::if_else(covariateId == 21601664411, 'C07', variable),
                variable = dplyr::if_else(covariateId == 21601744411, 'C08', variable),
                variable = dplyr::if_else(covariateId == 21601782411, 'C09', variable),
                variable = dplyr::if_else(covariateId == 21601853411, 'C10', variable),
                variable = dplyr::if_else(covariateId == 21603933411, 'M01A', variable),
                variable = dplyr::if_else(covariateId == 45879404, 'Never', variable),
                variable = dplyr::if_else(covariateId == 45884037, 'Current', variable),
                variable = dplyr::if_else(covariateId == 45883458, 'Former', variable),
                variable = dplyr::if_else(covariateId == 3038553531705, 'BMI', variable),
                variable = dplyr::if_else(covariateId == 3036277582705, 'height', variable),
                variable = dplyr::if_else(covariateId == 3025315529705, 'weight', variable),
                # variable = dplyr::if_else(covariateId == 3038553531706, 'BMI_long', variable),
                # variable = dplyr::if_else(covariateId == 3036277582706, 'height_long', variable),
                # variable = dplyr::if_else(covariateId == 3025315529706, 'weight_long', variable),
                variable = dplyr::if_else(covariateId %in% c(3004249323706, 4152194876706),
                                          'SBP', variable),
                variable = dplyr::if_else(covariateId %in% c(3012888323706, 4154790876706),
                                          'DBP', variable),
                variable = dplyr::if_else(covariateId == 3010813848706, 'Leukocytes', variable),
                variable = dplyr::if_else(covariateId == 3034639554706, 'HbA1c', variable),
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
                     values_from = covariateValue) %>%
  dplyr::mutate(sex_female = dplyr::if_else(is.na(sex_female), 0, sex_female),
                sex_male = dplyr::if_else(is.na(sex_male), 0, sex_male),
                T2DM = dplyr::if_else(is.na(T2DM), 0, T2DM),
                obesity = dplyr::if_else(is.na(obesity), 0, obesity),
                angor = dplyr::if_else(is.na(angor), 0, angor),
                tia = dplyr::if_else(is.na(tia), 0, tia),
                stroke = dplyr::if_else(is.na(stroke), 0, stroke),
                ami = dplyr::if_else(is.na(ami), 0, ami),
                Current = dplyr::if_else(is.na(Current), 0, Current),
                Former = dplyr::if_else(is.na(Former), 0, Former))
