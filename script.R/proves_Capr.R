library(tibble)
library(magrittr)
library(DatabaseConnector)
library(Capr)
library(SqlRender)

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

###################################################################################################
# Definicions del Capr (https://ohdsi.github.io/Capr/articles/complex-cohort-example.html)
# Type 2 Diabetes segons Covid19CharacterizationCharybdis
# <-- S'ha de posar en l'ordre que et dóna el getConceptIdDetails
conceptMapping <- createConceptMapping(n = 9,
                                       includeDescendants = rep(T, 9),      # <--
                                       isExcluded = c(T, T, F, T, F, F, T, T, T)) # <--
DMDx <- getConceptIdDetails(conceptIds = c(201820, 442793, 443238,
                                           201254, 435216, 4058243, 40484648,
                                           #Afegit mirant atlas-phenotype
                                           195771, 761051), #diabetis secondaria
                            connection = cdm_bbdd,
                            vocabularyDatabaseSchema = cdm_schema) %>%
  createConceptSetExpressionCustom(Name = "Diabetes Diagnosis",
                                   conceptMapping = conceptMapping)
# arreglo errors del paquet
DMDx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()
DMDx_hist <- getConceptIdDetails(conceptIds = c(40769338, 43021173, 42539022, 46270562),
                            connection = cdm_bbdd,
                            vocabularyDatabaseSchema = cdm_schema) %>%
  createConceptSetExpression(Name = "History of Diabetes Diagnosis",
                             includeDescendants = TRUE)
DMDx_hist@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()
#Type 1 Diabetes Diagnosis
T1Dx <- getConceptIdDetails(conceptIds = c(201254, 435216, 4058243, 40484648),
                            connection = cdm_bbdd,
                            vocabularyDatabaseSchema = cdm_schema) %>%
  createConceptSetExpression(Name = "Type 1 Diabetes Diagnosis",
                             includeDescendants = TRUE)
T1Dx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()

#Secondary Diabetes Diagnosis
SecondDMDx <- getConceptIdDetails(conceptIds = c(195771, 761051),
                                  connection = cdm_bbdd,
                                  vocabularyDatabaseSchema = cdm_schema) %>%
  createConceptSetExpression(Name = "Secondary Diabetes Diagnosis",
                             includeDescendants = TRUE)
SecondDMDx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()

# 139953
# T1DRxNormCodes <- paste(c(139825, 274783, 314684, 352385, 400008, 51428, 5856, 86009))
# T1Rx <- getConceptCodeDetails(conceptCode = T1DRxNormCodes,
#                               vocabulary = "RxNorm",
#                               connection = cdm_bbdd,
#                               vocabularyDatabaseSchema = cdm_schema,
#                               mapToStandard = TRUE) %>%
T1DRxNormCodes <- paste(c(1596977, 19058398, 19078552, 19078559, 19095211, 19095212, 19112791,
                          19133793, 19135264, 21076306, 21086042, 35410536, 35412958, 40051377,
                          40052768, 42479783, 42481504, 42481541, 42899447, 42902356, 42902587,
                          42902742, 42902821, 42902945, 42903059, 44058584, 46233969, 46234047,
                          46234234, 46234237))
T1Rx <- getConceptIdDetails(conceptIds = T1DRxNormCodes,
                            connection = cdm_bbdd,
                            vocabularyDatabaseSchema = cdm_schema) %>%
  createConceptSetExpression(Name = "Type 1 Diabetes Medications",
                             includeDescendants = TRUE)
T1Rx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()

# End-stage kidney disease
## End stage renal disease
vec_eskd_cod <- c(443611, 193782, 443919, 45887996, 2617395, 2617396, 44786436, 2617401, 2617405,
                  2617397, 2617404, 2617403, 2617400, 2617402, 2617399, 2617398,
                  #afegit mirant la descriptiva
                  192359)
## Dialysis
vec_dial <- c(4090651, 4032243, 45889365, 4027133, 38003431)
## Transplatation
vec_trans <- c(199991, 42539502, 4324887, 4309006)
## eGFR < 15 (més endavant)
vec_eskd <- c(vec_eskd_cod, vec_dial, vec_trans)
RenalDx <- getConceptIdDetails(conceptIds = vec_eskd,
                                    connection = cdm_bbdd,
                                    vocabularyDatabaseSchema = cdm_schema) %>%
  createConceptSetExpression(Name = "End-stage kidney disease",
                             includeDescendants = TRUE)
RenalDx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()
#Abnormal Lab
eGFR <- getConceptIdDetails(conceptIds = c("40764999", "1617023", "1619025", "46236952"),
                            connection = cdm_bbdd,
                            vocabularyDatabaseSchema = cdm_schema) %>%
  createConceptSetExpression(Name = "Abnormal eGFR",
                             includeDescendants = TRUE)

# Depress
DepressDx <- getConceptIdDetails(conceptIds = c(4098302, 433440, 440383,
                                                #afegit mirant la descriptiva
                                                4282096),
                                 connection = cdm_bbdd,
                                 vocabularyDatabaseSchema = cdm_schema) %>%
  createConceptSetExpression(Name = "Depress",
                             includeDescendants = TRUE)
DepressDx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()

# schizophrenia
SchizophreniaDx <- getConceptIdDetails(conceptIds = c(435783,
                                                      #afegit mirant la descriptiva
                                                      433450),
                                 connection = cdm_bbdd,
                                 vocabularyDatabaseSchema = cdm_schema) %>%
  createConceptSetExpression(Name = "Schizophrenia",
                             includeDescendants = TRUE)
SchizophreniaDx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()

# Epilèpsia
SeizureDx <- getConceptIdDetails(conceptIds = c(#afegit mirant la descriptiva
  380378),
                                       connection = cdm_bbdd,
                                       vocabularyDatabaseSchema = cdm_schema) %>%
  createConceptSetExpression(Name = "Seizure",
                             includeDescendants = TRUE)
SeizureDx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()

# Any malignant tumour
MaligNeoDx <- getConceptIdDetails(conceptIds = c(443392, 4144289,
                                                 #afegit mirant atlas-demo
                                                 439392,
                                                 #afegit mirant la descriptiva
                                                 200962, 139750, 4311499, 137809, 197500),
  connection = cdm_bbdd,
  vocabularyDatabaseSchema = cdm_schema) %>%
  createConceptSetExpression(Name = "Malignant Neoplasm",
                             includeDescendants = TRUE)
MaligNeoDx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()

# Systemic steroids: H02
# CortiRxNormCodes <- paste(c(42903427, 1555120, 19017895,
#                             920458, 1518254, 19055344, 1506270, 19027186, 1550557, 1551099, 903963,
#                             975125, 1507705, 19011127, 977421, 19086888, 19050907, 19009116,
#                             19061907, 19055156,
#                             19042801, 37499303, 985708))
CortiRxNormCodes <- paste(c(975169, 1506426, 1506430, 1506479, 1518259, 1518292, 1551101, 1551122,
                            1551123, 1551171, 1551192, 1555142, 1592257, 19016866, 19018083,
                            19063670, 19070310, 19084229, 19101595, 19104623, 19106649, 19106650,
                            19111643, 19121383, 35606531, 35606542, 36884768, 36893086, 37497612,
                            40234819, 40241504, 40897491, 40930518, 41052849, 42629020))
CortiRx <- getConceptIdDetails(conceptIds = CortiRxNormCodes,
                               connection = cdm_bbdd,
                               vocabularyDatabaseSchema = cdm_schema) %>%
  createConceptSetExpression(Name = "Systemic steroids: H02 Medications",
                             includeDescendants = TRUE)
CortiRx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()
###################################################################################################
# Building Queries
#DMDx Condition Occurrence Query
DMDxQuery <- createConditionOccurrence(conceptSetExpression = DMDx)
#DMDx_hist Condition Occurrence Query
DMDx_histQuery <- createConditionOccurrence(conceptSetExpression = DMDx_hist)
#T1Dx Condition Occurrence Query
T1DxQuery <- createConditionOccurrence(conceptSetExpression = T1Dx)
#T1Rx Drug Exposure Query
T1RxQuery <- createDrugExposure(conceptSetExpression = T1Rx)
#SecondDMDx Condition Occurrence Query
SecondDMDxQuery <- createConditionOccurrence(conceptSetExpression = SecondDMDx)

#RenalDx Condition Occurrence Query
RenalDxQuery <- createConditionOccurrence(conceptSetExpression = RenalDx)
#eGFR Query with value attribute
AbeGFRQuery <- createMeasurement(conceptSetExpression = eGFR,
                                 #add attribute of eGFR < 15
                                 attributeList = list(createValueAsNumberAttribute(Op = "lt",
                                                                                   Value = 15)))

#DepressDx Condition Occurrence Query
DepressDxQuery <- createConditionOccurrence(conceptSetExpression = DepressDx)
#SchizophreniaDx Condition Occurrence Query
SchizophreniaDxQuery <- createConditionOccurrence(conceptSetExpression = SchizophreniaDx)
#SeizureDx Condition Occurrence Query
SeizureDxQuery <- createConditionOccurrence(conceptSetExpression = SeizureDx)
#MaligNeoDx Condition Occurrence Query
MaligNeoDxQuery <- createConditionOccurrence(conceptSetExpression = MaligNeoDx)
#CortiRx Drug Exposure Query
CortiRxQuery <- createDrugExposure(conceptSetExpression = CortiRx)

###################################################################################################
# Creating the Initial Cohort Entry
## We defined initial entry as observed occurrence of all of the following events:
## + T2DM diagnosis,
## + prescription of a T2DM medication and
## + the presence of an abnormal lab
PrimaryCriteria <- createPrimaryCriteria(
  Name = "T2DM as Covid19CharacterizationCharybdis",
  ComponentList = list(DMDxQuery,
                       DMDx_histQuery),
  ObservationWindow = createObservationWindow(PriorDays = 0L,
                                              PostDays = 0L),
  Limit = "All")

###################################################################################################
# Inclusion Rules

# Inclusion with age
AgeAtt <- createAgeAttribute(Op = "gte", Value = 35)
Age35AndOlderGroup <- createGroup(Name = ">=35 years old",
                                  type="ALL",
                                  criteriaList = NULL,
                                  demographicCriteriaList = list(AgeAtt),
                                  Groups = NULL)

# No T1Dx at any point in patient history
tl1 <- createTimeline(StartWindow = createWindow(StartDays = "All",
                                                 StartCoeff = "Before",
                                                 EndDays = "All",
                                                 EndCoeff = "After"))
noT1DxCount <- createCount(Query = T1DxQuery,
                           Logic = "exactly",
                           Count = 0L,
                           Timeline = tl1)
NoT1DxGroup <- createGroup(Name = "No Diagnosis of Type 1 Diabetes",
                           type = "ALL",
                           criteriaList = list(noT1DxCount))

#no exposure to T1DM medication
tl2 <- createTimeline(StartWindow = createWindow(StartDays = "All",
                                                 StartCoeff = "Before",
                                                 EndDays = 183L,
                                                 EndCoeff = "After"))
noT1RxCount <- createCount(Query = T1RxQuery,
                           Logic = "exactly",
                           Count = 0L,
                           Timeline = tl2)
NoT1RxGroup <- createGroup(Name = "Without Insulin [-inf, T2DM + 6m)",
                           type = "ALL",
                           criteriaList = list(noT1RxCount))

tlprev <- createTimeline(StartWindow = createWindow(StartDays = "All",
                                                    StartCoeff = "Before",
                                                    EndDays = 0L,
                                                    EndCoeff = "After"))
# No SecondDMDx at any point previous to DM in patient history
noSecondDMDxCount <- createCount(Query = SecondDMDxQuery,
                                 Logic = "exactly",
                                 Count = 0L,
                                 Timeline = tlprev)
noSecondDMDxGroup <- createGroup(Name = "No previous Secondary diabetes",
                                 type = "ALL",
                                 criteriaList = list(noSecondDMDxCount))

# No RenalDx at any point previous to DM in patient history
noRenalDxCount <- createCount(Query = RenalDxQuery,
                              Logic = "exactly",
                              Count = 0L,
                              Timeline = tlprev)
exactly0AbeGFRCount <- createCount(Query = AbeGFRQuery,
                                   Logic = "exactly",
                                   Count = 0L,
                                   Timeline = tlprev)
noRenalDxGroup <- createGroup(Name = "No previous Renal problems",
                              type = "ALL",
                              criteriaList = list(noRenalDxCount,
                                                  exactly0AbeGFRCount))

# No DepressDx at any point previous to DM in patient history
noDepressDxCount <- createCount(Query = DepressDxQuery,
                                Logic = "exactly",
                                Count = 0L,
                                Timeline = tlprev)
noDepressDxGroup <- createGroup(Name = "No previous Depression",
                                type = "ALL",
                                criteriaList = list(noDepressDxCount))

# No SchizophreniaDx at any point previous to DM in patient history
noSchizophreniaDxCount <- createCount(Query = SchizophreniaDxQuery,
                                      Logic = "exactly",
                                      Count = 0L,
                                      Timeline = tlprev)
noSchizophreniaDxGroup <- createGroup(Name = "No previous Schizophrenia",
                                      type = "ALL",
                                      criteriaList = list(noSchizophreniaDxCount))

# No SeizureDx at any point previous to DM in patient history
noSeizureDxCount <- createCount(Query = SeizureDxQuery,
                                Logic = "exactly",
                                Count = 0L,
                                Timeline = tlprev)
noSeizureDxGroup <- createGroup(Name = "No previous Seizure",
                                type = "ALL",
                                criteriaList = list(noSeizureDxCount))

# No MaligNeoDx at any point previous to DM in patient history
noMaligNeoDxCount <- createCount(Query = MaligNeoDxQuery,
                                Logic = "exactly",
                                Count = 0L,
                                Timeline = tlprev)
noMaligNeoDxGroup <- createGroup(Name = "No previous Malignant Neoplasm",
                                type = "ALL",
                                criteriaList = list(noMaligNeoDxCount))

# No CortiRx at any point previous to DM in patient history
noCortiRxCount <- createCount(Query = CortiRxQuery,
                                 Logic = "exactly",
                                 Count = 0L,
                                 Timeline = tlprev)
noCortiRxGroup <- createGroup(Name = "No previous Corticoides",
                              type = "ALL",
                              criteriaList = list(noCortiRxCount))

InclusionRules <- createInclusionRules(Name = "Inclusion Rules",
                                       Contents = list(Age35AndOlderGroup,
                                                       NoT1DxGroup,
                                                       NoT1RxGroup,
                                                       noSecondDMDxGroup,
                                                       noRenalDxGroup,
                                                       noDepressDxGroup,
                                                       noSchizophreniaDxGroup,
                                                       noSeizureDxGroup,
                                                       noMaligNeoDxGroup,
                                                       noCortiRxGroup),
                                       Limit = "First")

###################################################################################################
# Finalizing the Cohort Definition
#person exits cohort if there is a diagnosis of T1DM
CensoringCriteria <- createCensoringCriteria(Name = "Censor of Renal, Depress cases",
                                             ComponentList = list(RenalDxQuery,
                                                                  DepressDxQuery,
                                                                  SchizophreniaDxQuery,
                                                                  SeizureDxQuery,
                                                                  MaligNeoDxQuery))

# EsCovidDiag <- createDateOffsetEndStrategy(offset = 0, eventDateOffset = "EndDate")
# La data d'entrada mínima és 2010-01-01, els anteriors són prevalents.
# Assegurem que tenim almenys 5 anys de seguiment
cohortEra <- createCohortEra(LeftCensorDate = "2009-12-31",
                             RightCensorDate = "2014-12-31")

T2DMPhenotype <- createCohortDefinition(Name = "T2DM as Covid19CharacterizationCharybdis",
                                        PrimaryCriteria = PrimaryCriteria,
                                        # AdditionalCriteria = AdditionalCriteria)#,
                                        InclusionRules = InclusionRules,
                                        CensoringCriteria = CensoringCriteria,
                                        # EndStrategy = EsCovidDiag,
                                        CohortEra = cohortEra)
# JSON
T2DMPhenotypeJson <- compileCohortDefinition(T2DMPhenotype)

###################################################################################################
# https://ohdsi.github.io/Capr/articles/CAPR_tutorial.html

genOp <- CirceR::createGenerateOptions(cohortIdFieldName = "cohort_definition_id",
                                       cohortId = 1,
                                       cdmSchema = cdm_schema,
                                       targetTable = paste(results_sc, cohortTable, sep='.'),
                                       resultSchema = results_sc,
                                       vocabularySchema = cdm_schema,
                                       generateStats = T)
cohortInfo <- compileCohortDefinition(T2DMPhenotype, genOp)
# Modifiquem el codi per tenir els casos anteriors a l'entrada al SIDIAP.
cohortInfo$ohdiSQL <- gsub(pattern = 'E.start_date >=  OP.observation_period_start_date and ',
                           replacement = '',
                           x =  cohortInfo$ohdiSQL, fixed = T)
cohortInfo$ohdiSQL <- gsub(pattern = 'WHERE DATEADD(day,0,OP.OBSERVATION_PERIOD_START_DATE) <= E.START_DATE AND DATEADD(day,0,E.START_DATE) <= OP.OBSERVATION_PERIOD_END_DATE',
                           replacement = '',
                           x =  cohortInfo$ohdiSQL, fixed = T)
cohortInfo$ohdiSQL <- gsub(pattern = 'AND A.START_DATE >= P.OP_START_DATE',
                           replacement = '',
                           x =  cohortInfo$ohdiSQL, fixed = T)
cohortInfo$ohdiSQL <- gsub(pattern = 'cohort_censor_stats',
                           replacement = paste0(cohortTable, '_censor_stats'),
                           x =  cohortInfo$ohdiSQL)
cohortInfo$ohdiSQL <- gsub(pattern = 'cohort_inclusion_result',
                           replacement = paste0(cohortTable, '_inclusion_result'),
                           x =  cohortInfo$ohdiSQL)
cohortInfo$ohdiSQL <- gsub(pattern = 'cohort_inclusion_stats',
                           replacement = paste0(cohortTable, '_inclusion_stats'),
                           x =  cohortInfo$ohdiSQL)
cohortInfo$ohdiSQL <- gsub(pattern = 'cohort_summary_stats',
                           replacement = paste0(cohortTable, '_summary_stats'),
                           x =  cohortInfo$ohdiSQL)
sink('prova.sql')
cat(cohortInfo$ohdiSQL)
sink()
# sink('prova.json')
# cat(cohortInfo$circeJson)
# sink()

###################################################################################################
# Cohort OUTCOME
#Angina
AngorDx <- getConceptIdDetails(conceptIds = c(321318),
                               connection = cdm_bbdd,
                               vocabularyDatabaseSchema = cdm_schema) %>%
  createConceptSetExpression(Name = "Angor Diagnosis",
                             includeDescendants = TRUE)
# arreglo errors del paquet
AngorDx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()

#AMI
AMIDx <- getConceptIdDetails(conceptIds = c(312327),# 4108217, 433128, 4329847),
                             connection = cdm_bbdd,
                             vocabularyDatabaseSchema = cdm_schema) %>%
  createConceptSetExpression(Name = "AMI Diagnosis",
                             includeDescendants = TRUE)
AMIDx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()

#Ictus
StrokeDx <- getConceptIdDetails(conceptIds = c(43530727, 443454),# 255919, 43022059),
                                connection = cdm_bbdd,
                                vocabularyDatabaseSchema = cdm_schema) %>%
  createConceptSetExpression(Name = "Stroke Diagnosis",
                             includeDescendants = TRUE)
StrokeDx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()

#TIA
TIADx <- getConceptIdDetails(conceptIds = c(373503, 381591),# 4353709, 43022059),
                             connection = cdm_bbdd,
                             vocabularyDatabaseSchema = cdm_schema) %>%
  createConceptSetExpression(Name = "TIA Diagnosis",
                             includeDescendants = TRUE)
TIADx@ConceptSetExpression[[1]]@id <- uuid::UUIDgenerate()

###################################################################################################
# Building Queries
AngorDxQuery <- createConditionOccurrence(conceptSetExpression = AngorDx)
AMIDxQuery <- createConditionOccurrence(conceptSetExpression = AMIDx)
StrokeDxQuery <- createConditionOccurrence(conceptSetExpression = StrokeDx)
TIADxQuery <- createConditionOccurrence(conceptSetExpression = TIADx)

###################################################################################################
# Creating the Initial Cohort Entry
OutcomePrimaryCriteria <- createPrimaryCriteria(
  Name = "Outcome: Angor, AMI, Stroke and TIA",
  ComponentList = list(AngorDxQuery,
                       AMIDxQuery,
                       StrokeDxQuery,
                       TIADxQuery),
  ObservationWindow = createObservationWindow(PriorDays = 0L,
                                              PostDays = 0L),
  Limit = "All")

OUTCOME <- createCohortDefinition(Name = "OUTCOME",
                                  PrimaryCriteria = OutcomePrimaryCriteria)
# JSON
OUTCOMEJson <- compileCohortDefinition(OUTCOME)

genOp <- CirceR::createGenerateOptions(cohortIdFieldName = "cohort_definition_id",
                                       cohortId = 2,
                                       cdmSchema = cdm_schema,
                                       targetTable = paste(results_sc, cohortTable, sep='.'),
                                       resultSchema = results_sc,
                                       vocabularySchema = cdm_schema,
                                       generateStats = T)
outcomeInfo <- compileCohortDefinition(OUTCOME, genOp)
# Modifiquem el codi per tenir els casos anteriors a l'entrada al SIDIAP.
outcomeInfo$ohdiSQL <- gsub(pattern = 'E.start_date >=  OP.observation_period_start_date and ',
                            replacement = '',
                            x =  outcomeInfo$ohdiSQL, fixed = T)
outcomeInfo$ohdiSQL <- gsub(pattern = 'WHERE DATEADD(day,0,OP.OBSERVATION_PERIOD_START_DATE) <= E.START_DATE AND DATEADD(day,0,E.START_DATE) <= OP.OBSERVATION_PERIOD_END_DATE',
                            replacement = '',
                            x =  outcomeInfo$ohdiSQL, fixed = T)
outcomeInfo$ohdiSQL <- gsub(pattern = 'AND A.START_DATE >= P.OP_START_DATE',
                            replacement = '',
                            x =  outcomeInfo$ohdiSQL, fixed = T)
outcomeInfo$ohdiSQL <- gsub(pattern = 'cohort_censor_stats',
                            replacement = paste0(cohortTable, '_censor_stats'),
                            x =  outcomeInfo$ohdiSQL)
outcomeInfo$ohdiSQL <- gsub(pattern = 'cohort_inclusion_result',
                            replacement = paste0(cohortTable, '_inclusion_result'),
                            x =  outcomeInfo$ohdiSQL)
outcomeInfo$ohdiSQL <- gsub(pattern = 'cohort_inclusion_stats',
                            replacement = paste0(cohortTable, '_inclusion_stats'),
                            x =  outcomeInfo$ohdiSQL)
outcomeInfo$ohdiSQL <- gsub(pattern = 'cohort_summary_stats',
                            replacement = paste0(cohortTable, '_summary_stats'),
                            x =  outcomeInfo$ohdiSQL)

###################################################################################################
# Generem les cohorts d'entrada i sortida
library(CohortGenerator)

cohortDefinitionSet <- tibble(atlasId = 9999999999,
                              cohortId = 1,
                              cohortName = "SIDIAP T2DM-WP5: Entrada",
                              sql = cohortInfo$ohdiSQL,
                              json = cohortInfo$circeJson,
                              logicDescription = as.character(NA),
                              generateStats = T) %>%
  bind_rows(tibble(atlasId = 9999999998,
                   cohortId = 2,
                   cohortName = "SIDIAP T2DM-WP5: Outcome",
                   sql = outcomeInfo$ohdiSQL,
                   json = outcomeInfo$circeJson,
                   logicDescription = as.character(NA),
                   generateStats = T))

cohortTableNames <- getCohortTableNames(cohortTable = cohortTable)
createCohortTables(connection = cdm_bbdd,
                   cohortTableNames = cohortTableNames,
                   cohortDatabaseSchema = results_sc,
                   incremental = FALSE)
generateCohortSet(connection = cdm_bbdd,
                  cdmDatabaseSchema = cdm_schema,
                  cohortDatabaseSchema = results_sc,
                  cohortTableNames = cohortTableNames,
                  cohortDefinitionSet = cohortDefinitionSet,
                  incremental = FALSE)
(cohortCounts <- getCohortCounts(connection = cdm_bbdd,
                                 cohortDatabaseSchema = results_sc,
                                 cohortTable = cohortTableNames$cohortTable))

###################################################################################################
# Fem el cohortDiagnostic
library(CohortDiagnostics)

insertInclusionRuleNames(connection = cdm_bbdd,
                         cohortDefinitionSet = cohortDefinitionSet,
                         cohortDatabaseSchema = results_sc,
                         cohortInclusionTable = cohortTableNames$cohortInclusionTable)
exportCohortStatsTables(connection = cdm_bbdd,
                        cohortDatabaseSchema = results_sc,
                        cohortTableNames = cohortTableNames,
                        cohortStatisticsFolder = file.path("InclusionStats"))
exportFolder <- "export"
executeDiagnostics(cohortDefinitionSet,
                   connection = cdm_bbdd,
                   cohortTable = cohortTable,
                   cohortDatabaseSchema = results_sc,
                   cdmDatabaseSchema = cdm_schema,
                   # vocabularyDatabaseSchema = cdm_schema,
                   exportFolder = exportFolder,
                   databaseId = "MyCdm",
                   minCellCount = 0)
dropCohortStatsTables(connection = cdm_bbdd,
                      cohortDatabaseSchema = results_sc,
                      cohortTableNames = cohortTableNames)
preMergeDiagnosticsFiles(exportFolder)
launchDiagnosticsExplorer(dataFolder = paste0('~idiap/projects/SOPHIA/',exportFolder),
                          dataFile = "Premerged.RData")

###################################################################################################
# Descriptiva i creació de les covariables
library(FeatureExtraction)
# covariateSettings <- createDefaultCovariateSettings()
# covariateSettings <- createDefaultTemporalCovariateSettings()
covariateSettings <- createCovariateSettings(useDemographicsGender = TRUE,
                                             useDemographicsAge = TRUE,
                                             useDemographicsAgeGroup = TRUE,
                                             useDemographicsIndexYear = TRUE,
                                             useDemographicsPriorObservationTime = TRUE,
                                             useDemographicsPostObservationTime = TRUE,
                                             useDemographicsTimeInCohort = TRUE,
                                             useConditionOccurrenceAnyTimePrior = TRUE,
                                             useDrugExposureAnyTimePrior = TRUE,
                                             useProcedureOccurrenceAnyTimePrior = TRUE,
                                             useMeasurementAnyTimePrior = TRUE,
                                             useMeasurementLongTerm = TRUE,
                                             useMeasurementValueAnyTimePrior = TRUE,
                                             useMeasurementValueLongTerm = TRUE,
                                             longTermStartDays = 2*(-365.25),
                                             endDays = 0)
covariateSettings2 <- convertPrespecSettingsToDetailedSettings(covariateSettings)

analysisDetails <- createAnalysisDetails(analysisId = 999,
                                         sqlFileName = "DomainConcept.sql",
                                         parameters = list(analysisId = 999,
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
                                         excludedCovariateConceptIds = c(201254, 435216, 4058243,
                                                                         40484648,195771, 761051),
                                         addDescendantsToExclude = TRUE)
covariateSettings <- list(covariateSettings,
                          createDetailedCovariateSettings(list(analysisDetails)))

covariateData <- getDbCovariateData(connection = cdm_bbdd,
                                    cdmDatabaseSchema = cdm_schema,
                                    cohortDatabaseSchema = results_sc,
                                    cohortTable = cohortTable,
                                    cohortId = 1,
                                    rowIdField = "subject_id",
                                    covariateSettings = covariateSettings)
summary(covariateData)
covariateData$covariates
tidyCovariates <- tidyCovariateData(covariateData,
                                    minFraction = 0.001,
                                    normalize = FALSE,
                                    removeRedundancy = TRUE)
covariateData2 <- aggregateCovariates(covariateData)
summary(covariateData2)
covariateData2$covariates
covariateData2$covariatesContinuous

sel_id <- c(3020460751706, #C reactive protein [Mass/volume] in Serum or Plasma (milligram per liter)
            3001122748706, #Ferritin [Mass/volume] in Serum or Plasma (microgram per liter)
            3010813848706, #Leukocytes [#/volume] in Blood (thousand per microliter)
            3024561713706, #Albumin [Mass/volume] in Serum or Plasma (gram per deciliter)
            3016723840706, #Creatinine [Mass/volume] in Serum or Plasma (milligram per deciliter)
            40764999117706, #Glomerular filtration rate/1.73 sq M.predicted [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (CKD-EPI) (milliliter per minute per 1.73 square meter)
            46236952117706, #Glomerular filtration rate/1.73 sq M.predicted [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (MDRD) (milliliter per minute per 1.73 square meter)
            3034639554706, #Hemoglobin A1c [Mass/volume] in Blood (percent)
            3004501840706, #Glucose [Mass/volume] in Serum or Plasma (milligram per deciliter)
            3038553531706, #Body mass index (BMI) [Ratio] (kilogram per square meter)
            3036277582706, #Body height (centimeter)
            3025315529706, #Body weight (kilogram)
            4152194876706, #Systolic blood pressure (millimeter mercury column)
            4154790876706, #Diastolic blood pressure (millimeter mercury column)
            3028437840706, #Cholesterol in LDL [Mass/volume] in Serum or Plasma (milligram per deciliter)
            3011884840706, #Cholesterol in HDL [Presence] in Serum or Plasma (milligram per deciliter)
            3027114840706, #Cholesterol [Mass/volume] in Serum or Plasma (milligram per deciliter)
            3022192840706, #Triglyceride [Mass/volume] in Serum or Plasma (milligram per deciliter)
            3006923645706, #Alanine aminotransferase [Enzymatic activity/volume] in Serum or Plasma (unit per liter)
            3038553531705, #Body mass index (BMI) [Ratio] (kilogram per square meter)
            3036277582705, #Body height (centimeter)
            3025315529705, #Body weight (kilogram)
            1002 # Age
            )
sel_id <- as.character(sel_id)
flextable::flextable(covariateData2$covariateRef %>%
                       inner_join(covariateData2$covariatesContinuous) %>%
                       mutate(covariateId = as.character(floor(covariateId))) %>%
                       filter(covariateId %in% sel_id) %>%
                       select(-covariateId, -analysisId, -conceptId) %>%
                       collect())


result <- createTable1(covariateData2, output = 'one column')
View(result)
print(result, row.names = FALSE, right = FALSE)

###################################################################################################
library(BigKnn)

indexFolder <- 'BigKnn'
buildKnn(outcomes = )

###################################################################################################
# Github cluster omop
minFeaturesPerPerson = 100
minPersonsPerFeature = 10
ParallelLogger::logInfo("Filter persons with few non-zero covariates")
filteredPersons <- covariateData$covariates %>%
  group_by(.data$rowId) %>%
  summarise(count = n()) %>%
  filter(.data$count > minFeaturesPerPerson)  %>%
  select(.data$rowId)
covariateData$filteredCovariates <- covariateData$covariates %>%
  inner_join(filteredPersons, by = "rowId")
totalPersonCount <- filteredPersons %>%
  count() %>%
  pull()

ParallelLogger::logInfo("Compute inverse document frequencies")
idfs <- covariateData$filteredCovariates %>%
  group_by(.data$covariateId) %>%
  summarise(count = n()) %>%
  filter(.data$count > minPersonsPerFeature)  %>%
  mutate(idf = log(totalPersonCount / count)) %>%
  select(.data$covariateId, .data$idf)

temp <- covariateData$filteredCovariates %>%
  inner_join(idfs, by = "covariateId") %>%
  select(.data$rowId, .data$covariateId, .data$idf) %>%
  collect()
idxToRowId <- unique(temp$rowId)
idxToCovariateId <- unique(temp$covariateId)
temp$rowId <- match(temp$rowId, idxToRowId)
temp$covariateId <- match(temp$covariateId, idxToCovariateId)
vectors <- slam::simple_triplet_matrix(i = temp$covariateId,
                                       j = temp$rowId,
                                       v = temp$idf)
rm(temp)
ParallelLogger::logInfo("Compute distance matrix")
distances <- 1 - slam::crossprod_simple_triplet_matrix(vectors)/(sqrt(slam::col_sums(vectors^2) %*% t(slam::col_sums(vectors^2))))
idxToRowId <- data.frame(idx = 1:length(idxToRowId),
                         rowId = idxToRowId)

ParallelLogger::logInfo("Computing 2D coordinates")
umapSettings <- umap::umap.defaults
umapSettings$metric <- "cosine"
map <- umap::umap(distances, input = "dist", config = umapSettings)

points <- data.frame(x = map$layout[, 1],
                     y = map$layout[, 2],
                     rowId = idxToRowId$rowId)
ParallelLogger::logInfo("Plotting")
plot <- ggplot2::ggplot(points, ggplot2::aes(x = .data$x, y = .data$y)) +
  ggplot2::geom_point(shape = 16, alpha = 0.4) +
  ggplot2::theme(panel.background = ggplot2::element_blank(),
                 axis.text = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank(),
                 axis.title = ggplot2::element_blank(),
                 legend.position = "none",
                 legend.background = ggplot2::element_blank(),
                 legend.key = ggplot2::element_blank(),
                 legend.title = ggplot2::element_blank())
###################################################################################################
# PatientLevel
library(PatientLevelPrediction)


covariateSettings <- createCovariateSettings(useDemographicsGender = TRUE,
                                             useDemographicsAge = TRUE,
                                             useConditionGroupEraLongTerm = TRUE,
                                             useConditionGroupEraAnyTimePrior = TRUE,
                                             useDrugGroupEraLongTerm = TRUE,
                                             useDrugGroupEraAnyTimePrior = TRUE,
                                             useMeasurementLongTerm = TRUE,
                                             useMeasurementAnyTimePrior = TRUE,
                                             useVisitConceptCountLongTerm = TRUE,
                                             longTermStartDays = -365,
                                             endDays = -1)

plpData <- getPlpData(connectionDetails = connectionDetails,
                      cdmDatabaseSchema = cdm_schema,
                      cohortId = 1,
                      outcomeIds = 2,
                      cohortDatabaseSchema = results_sc,
                      cohortTable = cohortTable,
                      outcomeDatabaseSchema = results_sc,
                      outcomeTable = cohortTable,
                      covariateSettings = covariateSettings)



###################################################################################################
disconnect(cdm_bbdd)
