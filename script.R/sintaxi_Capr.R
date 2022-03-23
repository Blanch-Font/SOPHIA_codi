library(tibble)
library(Capr)
library(magrittr)
library(DatabaseConnector)
library(SqlRender)
library(CohortGenerator)
library(CohortDiagnostics)

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
#Type 2 Diabetes Diagnosis
T2Dx <- getConceptIdDetails(conceptIds = 201826,
                            connection = cdm_bbdd,
                            vocabularyDatabaseSchema = cdm_schema) %>%
  createConceptSetExpression(Name = "Type 2 Diabetes Diagnosis",
                             includeDescendants = TRUE)

#Type 2 Diabetes Medications concept IDS
T2RxIds <- c(1502809L, 1502826L, 1503297L, 1510202L, 1515249L, 1516766L, 1525215L, 1529331L,
             1530014L, 1547504L, 1559684L, 1560171L, 1580747L, 1583722L, 1594973L, 1597756L)
#create concept set expression
T2Rx <- getConceptIdDetails(conceptIds = T2RxIds,
                            connection = cdm_bbdd,
                            vocabularyDatabaseSchema = cdm_schema) %>%
  createConceptSetExpression(Name = "Type 2 Diabetes Medications",
                             includeDescendants = TRUE)

#Type 1 Diabetes Diagnosis
T1Dx <- getConceptIdDetails(conceptIds = 201254,
                            connection = cdm_bbdd,
                            vocabularyDatabaseSchema = cdm_schema) %>%
  createConceptSetExpression(Name = "Type 1 Diabetes Diagnosis",
                             includeDescendants = TRUE)

#Type 1 Diabetes Medications
T1DRxNormCodes <- paste(c(139825,274783,314684, 352385,400008,51428, 5856,86009,139953))
T1Rx <- getConceptCodeDetails(conceptCode = T1DRxNormCodes,
                              vocabulary = "RxNorm",
                              connection = cdm_bbdd,
                              vocabularyDatabaseSchema = cdm_schema,
                              mapToStandard = TRUE) %>%
  createConceptSetExpression(Name = "Type 1 Diabetes Medications",
                             includeDescendants = TRUE)

#Abnormal Lab
AbLabHbA1c <- c("4548-4", "17856-6", "4549-2", "17855-8") %>%
  getConceptCodeDetails(conceptCode = ., #places the lhs vector to the rhs of the pipe
                        vocabulary = "LOINC",
                        connection = cdm_bbdd,
                        vocabularyDatabaseSchema = cdm_schema,
                        mapToStandard = TRUE) %>%
  createConceptSetExpression(Name = "Abnormal Lab HbA1c",
                             includeDescendants = TRUE)

#Ab Lab for Random Glucose (>= 200 mg/dl)
AbLabRandomGluc <- c("2339-0", "2345-7") %>%
  getConceptCodeDetails(conceptCode = .,
                        vocabulary = "LOINC",
                        connection = cdm_bbdd,
                        vocabularyDatabaseSchema = cdm_schema,
                        mapToStandard = TRUE) %>%
  createConceptSetExpression(Name = "Abnormal Lab Random Glucose",
                             includeDescendants = TRUE)

#Ab Lab for Fasting Glucose (>= 125 mg/dl)
AbLabFastingGluc <- c("1558-6") %>%
  getConceptCodeDetails(conceptCode = .,
                        vocabulary = "LOINC",
                        connection = cdm_bbdd,
                        vocabularyDatabaseSchema = cdm_schema,
                        mapToStandard = TRUE) %>%
  createConceptSetExpression(Name = "Abnormal Lab Fasting Glucose",
                             includeDescendants = TRUE)

###################################################################################################
# Building Queries
#T2Rx Drug Exposure Query
T2RxQuery <- createDrugExposure(conceptSetExpression = T2Rx)

#T1Rx Drug Exposure Query
T1RxQuery <- createDrugExposure(conceptSetExpression = T1Rx)

#T2Dx Condition Occurrence Query
T2DxQuery <- createConditionOccurrence(conceptSetExpression = T2Dx)

#T1Dx Condition Occurrence Query
T1DxQuery <- createConditionOccurrence(conceptSetExpression = T1Dx)

#HbA1c Query with value attribute
AbLabHbA1cQuery <- createMeasurement(
  conceptSetExpression = AbLabHbA1c,
  attributeList = list(#add attribute of >= 6% HbA1c
    createValueAsNumberAttribute(Op = "gte", Value = 6)))

#RandomGluc Query with value attribute
AbLabRandomGlucQuery <- createMeasurement(
  conceptSetExpression = AbLabRandomGluc,
  attributeList = list(#add attribute of >= 200 mg/dl
    createValueAsNumberAttribute(Op = "gte", Value = 200L)))

#FastingGluc Query with value attribute
AbLabFastingGlucQuery <- createMeasurement(
  conceptSetExpression = AbLabFastingGluc,
  attributeList = list(#add attribute of >= 125 mg/dl
    createValueAsNumberAttribute(Op = "gte", Value = 125L)))

###################################################################################################
# Creating Counts from Queries
#create timeline
tl1 <- createTimeline(StartWindow = createWindow(StartDays = "All",
                                                 StartCoeff = "Before",
                                                 EndDays = 0L,
                                                 EndCoeff = "After"))

#no occurrence of T1 Diabetes
noT1DxCount <- createCount(Query = T1DxQuery,
                           Logic = "exactly",
                           Count = 0L,
                           Timeline = tl1)

#no occurrence of T2 Diabetes
noT2DxCount <- createCount(Query = T2DxQuery,
                           Logic = "exactly",
                           Count = 0L,
                           Timeline = tl1)

#at least 1 occurrence of T2 Diabetes
atLeast1T2DxCount <- createCount(Query = T2DxQuery,
                                 Logic = "at_least",
                                 Count = 1L,
                                 Timeline = tl1)

#at least 2 occurrence of T2 Diabetes
atLeast2T2DxCount <- createCount(Query = T2DxQuery,
                                 Logic = "at_least",
                                 Count = 2L,
                                 Timeline = tl1)

##################
#Medication Counts
##################
#at least 1 T2DM medication
atLeast1T2RxCount <- createCount(Query = T2RxQuery,
                                 Logic = "at_least",
                                 Count = 1L,
                                 Timeline = tl1)

#no exposure to T2DM medication
noT2RxCount <- createCount(Query = T2RxQuery,
                           Logic = "exactly",
                           Count = 0L,
                           Timeline = tl1)

#at least 1 T1DM medication
atLeast1T1RxCount <- createCount(Query = T1RxQuery,
                                 Logic = "at_least",
                                 Count = 1L,
                                 Timeline = tl1)

#no exposure to T1DM medication
noT1RxCount <- createCount(Query = T1RxQuery,
                           Logic = "exactly",
                           Count = 0L,
                           Timeline = tl1)

#################
#AbLab Counts
#################
#at least 1 abnormal HbA1c Lab
atLeast1AbLabHbA1cCount <- createCount(Query = AbLabHbA1cQuery,
                                       Logic = "at_least",
                                       Count = 1L,
                                       Timeline = tl1)

#at least 1 abnormal Fasting Glucose Lab
atLeast1AbLabFastingGlucCount <- createCount(Query = AbLabFastingGlucQuery,
                                             Logic = "at_least",
                                             Count = 1L,
                                             Timeline = tl1)

#at least 1 abnormal Random Glucose Lab
atLeast1AbLabRandomGlucCount <- createCount(Query = AbLabRandomGlucQuery,
                                            Logic = "at_least",
                                            Count = 1L,
                                            Timeline = tl1)

###################################################################################################
# Creating the Initial Cohort Entry
## We defined initial entry as observed occurrence of all of the following events:
## + T2DM diagnosis,
## + prescription of a T2DM medication and
## + the presence of an abnormal lab
PrimaryCriteria <- createPrimaryCriteria(
  Name = "PC for T2DM Case Phenotype",
  ComponentList = list(T2DxQuery,
                       T2RxQuery,
                       AbLabHbA1cQuery, AbLabFastingGlucQuery, AbLabRandomGlucQuery),
  ObservationWindow = createObservationWindow(PriorDays = 0L,
                                              PostDays = 0L),
  Limit = "All")

#No T1Dx at any point in patient history
NoT1DxGroup <- createGroup(Name = "No Diagnosis of Type 1 Diabetes",
                           type = "ALL",
                           criteriaList = list(noT1DxCount))
#create additional Criteria
#further restrict the initial capture to people with no T1Dx
AdditionalCriteria <- createAdditionalCriteria(Name = "AC for T2DM Case Phenotype",
                                               Contents = NoT1DxGroup,
                                               Limit = "First")

###################################################################################################
# Creating T2D Pathways as Groups

## From the abnormal lab counts created before, we only need 1 to be true for the person to be
## included in the cohort.
atLeast1AbLabGroup <- createGroup(Name = "Abnormal labs for HbA1c, Fasting+Random Glucose",
                                  type = "ANY",
                                  criteriaList = list(atLeast1AbLabHbA1cCount,
                                                      atLeast1AbLabFastingGlucCount,
                                                      atLeast1AbLabRandomGlucCount))

#Path 1: 0 T2Dx, 1+ T2Rx and 1+ AbLab
Pathway1T2DMGroup <- createGroup(Name = "Pathway1",
                                 Description = "0 T2Dx, 1+ T2Rx and 1+ AbLab",
                                 type = "ALL",
                                 criteriaList = list(noT2DxCount,
                                                     atLeast1T2RxCount),
                                 Groups = list(atLeast1AbLabGroup))

#Path 2: 1+ T2Dx, 0 T1Rx, 0 T2Rx, and 1+ AbLab
Pathway2T2DMGroup <- createGroup(Name = "Pathway2",
                                 Description = "1+ T2Dx, 0 T1Rx, 0 T2Rx, and 1+ AbLab",
                                 type = "ALL",
                                 criteriaList = list(atLeast1T2DxCount,
                                                     noT1RxCount,
                                                     noT2RxCount),
                                 Groups = list(atLeast1AbLabGroup))

#Path 3: 1+ T2Dx, 0 T1Rx, and 1+ T2Rx
Pathway3T2DMGroup <- createGroup(Name = "Pathway3",
                                 Description = "1+ T2Dx, 0 T1Rx, and 1+ T2Rx",
                                 type = "ALL",
                                 criteriaList = list(atLeast1T2DxCount,
                                                     noT1RxCount,
                                                     atLeast1T2RxCount))

#Path 5: 1+ T2Dx, 1+ T1Rx, 0 T2Rx and 2+ T2Dx
Pathway5T2DMGroup <- createGroup(Name = "Pathway5",
                                 Description = "1+ T2Dx, 1+ T1Rx, 0 T2Rx and 2+ T2Dx",
                                 type = "ALL",
                                 criteriaList = list(atLeast1T2DxCount,
                                                     atLeast1T1RxCount,
                                                     noT2RxCount,
                                                     atLeast2T2DxCount))
#Path4: Especial
## This attribute nests a group within a query, meaning that the query is only valid if the
## nested group is also true.
tl2 <- createTimeline(StartWindow = createWindow(StartDays = "All",
                                                 StartCoeff = "Before",
                                                 EndDays = 1L,
                                                 EndCoeff = "Before"))

PriorT2RxCount <- createCount(Query = T2RxQuery,
                              Logic = "at_least",
                              Count = 1L,
                              Timeline = tl2)

PriorT2RxNestedGroup <- createCorrelatedCriteriaAttribute(
  createGroup(Name = "Nested Group T2Rx before T1Rx",
              type = "ALL",
              criteriaList = list(PriorT2RxCount)))

T2RxBeforeT1RxCount <- createDrugExposure(conceptSetExpression = T1Rx,
                                          attributeList = list(PriorT2RxNestedGroup)) %>%
  createCount(Logic = "at_least",
              Count = 1L,
              Timeline = tl1)
#Path 4: 1+ T2Dx, 1+ T1Rx, 1+T2Rx, and 1+ T2Rx < T1Rx
Pathway4T2DMGroup <- createGroup(Name = "Pathway4",
                                 Description = "1+ T2Dx, 1+ T1Rx, 1+T2Rx, and 1+ T2Rx < T1Rx",
                                 type = "ALL",
                                 criteriaList = list(atLeast1T2DxCount,
                                                     atLeast1T1RxCount,
                                                     T2RxBeforeT1RxCount))

#T2DM Case Group
T2DMCase <- createGroup(Name = "Case for T2DM using algorithm",
                        type = "ANY",
                        Groups = list(Pathway1T2DMGroup,
                                      Pathway2T2DMGroup,
                                      Pathway3T2DMGroup,
                                      Pathway4T2DMGroup,
                                      Pathway5T2DMGroup))

#keep T2DM cases if they meet 1 of the 5 pathways
InclusionRules <- createInclusionRules(Name = "IRs for T2DM Case Phenotype",
                                       Contents = list(T2DMCase),
                                       Limit = "First")

###################################################################################################
# Finalizing the Cohort Definition
#person exits cohort if there is a diagnosis of T1DM
CensoringCriteria <- createCensoringCriteria(Name = "Censor of T1DM cases",
                                             ComponentList = list(T1DxQuery))

T2DMPhenotype <- createCohortDefinition(Name = "PheKB T2DM Definition",
                                        PrimaryCriteria = PrimaryCriteria,
                                        AdditionalCriteria = AdditionalCriteria,
                                        InclusionRules = InclusionRules,
                                        CensoringCriteria = CensoringCriteria)
# JSON
T2DMPhenotypeJson <- compileCohortDefinition(T2DMPhenotype)

###################################################################################################
# https://ohdsi.github.io/Capr/articles/CAPR_tutorial.html

genOp <- CirceR::createGenerateOptions(cohortIdFieldName = "cohort_definition_id",
                                       cohortId = 9999,
                                       cdmSchema = cdm_schema,
                                       targetTable = paste(results_sc, cohortTable, sep='.'),
                                       resultSchema = results_sc,
                                       vocabularySchema = cdm_schema,
                                       generateStats = T)
cohortInfo <- compileCohortDefinition(T2DMPhenotype, genOp)

###################################################################################################
# Codi meu
executeSql(connection = cdm_bbdd,
           sql = render("CREATE TABLE @resultSchema.@targetTable (
                         cohort_definition_id integer,
                         subject_id           integer,
                         cohort_start_date    date,
                         cohort_end_date      date);",
                        resultSchema = results_sc,
                        targetTable = cohortTable))

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
cohortDefinitionSet <- tibble(atlasId = 9999999999,
                              cohortId = 1,
                              cohortName = "PheKB T2DM Definition",
                              sql = cohortInfo$ohdiSQL,
                              json = cohortInfo$circeJson,
                              logicDescription = as.character(NA))

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
cohortCounts <- getCohortCounts(connection = cdm_bbdd,
                                cohortDatabaseSchema = results_sc,
                                cohortTable = cohortTableNames$cohortTable)
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
                   vocabularyDatabaseSchema = cdm_schema,
                   exportFolder = exportFolder,
                   databaseId = "MyCdm",
                   minCellCount = 5)
dropCohortStatsTables(connection = cdm_bbdd,
                      cohortDatabaseSchema = results_sc,
                      cohortTableNames = cohortTableNames)
preMergeDiagnosticsFiles(exportFolder)
launchDiagnosticsExplorer(dataFolder = paste0('~idiap/projects/SOPHIA/',exportFolder),
                          dataFile = "Premerged.RData")

disconnect(cdm_bbdd)
