# Paquets necessaris
devtools::install_github("ohdsi/CirceR")
devtools::install_github("ohdsi/Capr")
devtools::install_github("ohdsi/CohortGenerator")
devtools::install_github("ohdsi/CohortDiagnostics")
# devtools::install(pkg = '~idiap/projects/SOPHIA')
devtools::install_github('Blanch-Font/SOPHIA')

library(SOPHIA)
library(DatabaseConnector)
library(magrittr)
# library(Andromeda)
library(purrr)
library(uwot)
umap_res <- map(setNames(c("Female", "Male"), c("Female", "Male")),
                ~load_uwot(paste0("umap_model_", .x)))
load('arch_mod.Rdata')

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
cohortTable <- 'cohortTable'

SOPHIAroot <- 'renv/library/R-4.1/x86_64-pc-linux-gnu/SOPHIA/'

cohort_T2DM <- CreateSQL_T2DM(cdm_bbdd,
                              cdm_schema,
                              results_sc,
                              cohortTable)
cohort_T1DM <- CreateSQL_T1DM(cdm_bbdd,
                              cdm_schema,
                              results_sc,
                              cohortTable)
outcome_AMI <- CreateSQL_AMI(cdm_bbdd,
                             cdm_schema,
                             results_sc,
                             cohortTable)
outcome_angor <- CreateSQL_angor(cdm_bbdd,
                                 cdm_schema,
                                 results_sc,
                                 cohortTable)
outcome_stroke_i <- CreateSQL_stroke_i(cdm_bbdd,
                                       cdm_schema,
                                       results_sc,
                                       cohortTable)
outcome_TIA <- CreateSQL_TIA(cdm_bbdd,
                             cdm_schema,
                             results_sc,
                             cohortTable)

cohortDefinitionSet <- data.frame(atlasId = rep(NA, 6),
                                  cohortId = 1:6,
                                  cohortName = c("SIDIAP T2DM-WP5",
                                                 "SIDIAP T1DM-WP4",
                                                 "Outcome: AMI",
                                                 "Outcome: Angor",
                                                 "Outcome: Stroke I",
                                                 "Outcome: TIA"),
                                  sql = c(cohort_T2DM$ohdiSQL,
                                          cohort_T1DM$ohdiSQL,
                                          outcome_AMI$ohdiSQL,
                                          outcome_angor$ohdiSQL,
                                          outcome_stroke_i$ohdiSQL,
                                          outcome_TIA$ohdiSQL),
                                  json = c(cohort_T2DM$circeJson,
                                           cohort_T1DM$circeJson,
                                           outcome_AMI$circeJson,
                                           outcome_angor$circeJson,
                                           outcome_stroke_i$circeJson,
                                           outcome_TIA$circeJson),
                                  logicDescription = rep(as.character(NA), 6),
                                  generateStats = rep(T, 6))

# cohortDefinitionSet <- data.frame(
#   atlasId = rep(NA, 3),
#   CohortGenerator::getCohortDefinitionSet(
#     settingsFileName = 'Cohorts.csv',
#     jsonFolder = file.path(paste0(SOPHIAroot,'extdata')),
#     sqlFolder = file.path(paste0(SOPHIAroot,'sql/sql_server'))),
#   logicDescription = rep(as.character(NA), 3),
#   generateStats = rep(T, 3))

n_cohort <- createCohort(cdm_bbdd,
                         cdm_schema,
                         results_sc,
                         cohortTable,
                         cohortDefinitionSet)
###################################################################################################

###################################################################################################
covariateData_T2DM <- buildData(cdm_bbdd,
                                cdm_schema,
                                results_sc,
                                cohortTable,
                                acohortId = 1)
covariateData2_T2DM <- FeatureExtraction::aggregateCovariates(covariateData_T2DM)

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
cov_cate_resum_T2DM <- covariateData2_T2DM$covariateRef %>%
  dplyr::filter(analysisId %in% c(411, 413) & conceptId %in% sel_med_conceptId |
                  !(analysisId %in% c(411, 413))) %>%
  dplyr::inner_join(covariateData2_T2DM$covariates) %>%
  dplyr::mutate(covariateId = as.character(floor(covariateId)),
                analysisId = as.integer(analysisId),
                conceptId = as.integer(conceptId),
                sumValue = as.integer(sumValue),
                averageValue = averageValue*100) %>%
  dplyr::collect()

cov_num_resum_T2DM <- covariateData2_T2DM$covariateRef %>%
  dplyr::inner_join(covariateData2_T2DM$covariatesContinuous) %>%
  dplyr::mutate(covariateId = as.character(floor(covariateId)),
                analysisId = as.integer(analysisId),
                conceptId = as.integer(conceptId)) %>%
  dplyr::select(-covariateId, -analysisId, -conceptId) %>%
  dplyr::collect()

save(cov_cate_resum_T2DM,
     cov_num_resum_T2DM,
     file = 'taules_desc_T2DM.RData')

bbdd_covar_T2DM <- transformToFlat(covariateData_T2DM)
bbdd_covar_T2DM <- buildFollowUp(cdm_bbdd,
                                 cdm_schema,
                                 results_sc,
                                 cohortTable,
                                 acohortId = 1,
                                 bbdd_covar = bbdd_covar_T2DM)
bbdd_covar <- bbdd_covar_T2DM
IN <- 'Sergio_DT2_analysis_foward_sex_umap.Rmd'
OUT <- 'Sergio_DT2_analysis_foward_sex_umap.html'
.PATH = sprintf('.tmp/%s', strsplit(x = basename(OUT), split = "\\.")[[1]][1])
dir.create(.PATH, showWarnings = F, recursive = T)
file.copy(from = IN, to = .PATH, overwrite = T)
rmarkdown::render(input = paste(.PATH, basename(IN), sep = "/"),
                  output_dir = dirname(OUT),
                  output_file = basename(OUT),
                  clean = T)

###################################################################################################
covariateData_T1DM <- buildData(cdm_bbdd,
                                cdm_schema,
                                results_sc,
                                cohortTable,
                                acohortId = 2)
covariateData2_T1DM <- FeatureExtraction::aggregateCovariates(covariateData_T1DM)

cov_cate_resum_T1DM <- covariateData2_T1DM$covariateRef %>%
  dplyr::filter(analysisId %in% c(411, 413) & conceptId %in% sel_med_conceptId |
                  !(analysisId %in% c(411, 413))) %>%
  dplyr::inner_join(covariateData2_T1DM$covariates) %>%
  dplyr::mutate(covariateId = as.character(floor(covariateId)),
                analysisId = as.integer(analysisId),
                conceptId = as.integer(conceptId),
                sumValue = as.integer(sumValue),
                averageValue = averageValue*100) %>%
  dplyr::collect()

cov_num_resum_T1DM <- covariateData2_T1DM$covariateRef %>%
  dplyr::inner_join(covariateData2_T1DM$covariatesContinuous) %>%
  dplyr::mutate(covariateId = as.character(floor(covariateId)),
                analysisId = as.integer(analysisId),
                conceptId = as.integer(conceptId)) %>%
  dplyr::select(-covariateId, -analysisId, -conceptId) %>%
  dplyr::collect()

save(cov_cate_resum_T1DM,
     cov_num_resum_T1DM,
     file = 'taules_desc_T1DM.RData')

bbdd_covar_T1DM <- transformToFlat(covariateData_T1DM)
bbdd_covar_T1DM <- buildFollowUp(cdm_bbdd,
                                 cdm_schema,
                                 results_sc,
                                 cohortTable,
                                 acohortId = 2,
                                 bbdd_covar = bbdd_covar_T1DM)
bbdd_covar <- bbdd_covar_T1DM
IN <- 'Sergio_DT2_analysis_foward_sex_umap.Rmd'
OUT <- 'Sergio_DT1_analysis_foward_sex_umap.html'
.PATH = sprintf('.tmp/%s', strsplit(x = basename(OUT), split = "\\.")[[1]][1])
dir.create(.PATH, showWarnings = F, recursive = T)
file.copy(from = IN, to = .PATH, overwrite = T)
rmarkdown::render(input = paste(.PATH, basename(IN), sep = "/"),
                  output_dir = dirname(OUT),
                  output_file = basename(OUT),
                  clean = T)
#
# name_html <- c('umap_all', 'umap_all_f', 'umap_sex2', 'umap_sex2_f', 'umap_obesity2', 'umap_obesity2_f')
# for (.name in name_html){
#   bbdd_covar <- abbdd_covar
#   # IN <- sprintf('script.Rmd/%s.Rmd', .name)
#   IN <- sprintf('%s.Rmd', .name)
#   OUT <- sprintf('www/%s.html', .name)
#   .PATH = sprintf('.tmp/%s', strsplit(x=basename(OUT), split="\\.")[[1]][1])
#   dir.create(.PATH, showWarnings = F, recursive= T)
#   file.copy(from=IN, to=.PATH, overwrite=T)
#   rmarkdown::render(input=paste(.PATH,basename(IN),sep="/"),
#                     output_dir=dirname(OUT),
#                     output_file=basename(OUT), clean=T)
# }
#

# fet_diag <- runDiagnostic(cdm_bbdd,
#                           cdm_schema,
#                           results_sc,
#                           cohortTable,
#                           cohortDefinitionSet)
# CohortDiagnostics::launchDiagnosticsExplorer(sqliteDbPath = 'MergedCohortDiagnosticsData.sqlite')
disconnect(cdm_bbdd)
