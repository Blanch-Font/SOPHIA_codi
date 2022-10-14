###################################################################################################
# Auxiliar package
###################################################################################################
devtools::install_github("ohdsi/CirceR")
devtools::install_github("ohdsi/Capr")
devtools::install_github("ohdsi/CohortGenerator")
devtools::install_github("ohdsi/CohortDiagnostics")
# devtools::install(pkg = '~idiap/projects/SOPHIA')
devtools::install_github('Blanch-Font/SOPHIA')

###################################################################################################
# Auxiliar Function
# Funciones candidates a passar al paquete SOPHIA
###################################################################################################
# Funcion para pasar del servidor a una tabla plana
# Necesita totes les coses del servidor més quina cohort volem contruir.
FunCovar <- function(acdm_bbdd = cdm_bbdd,
                     acdm_schema = cdm_schema,
                     aresults_sc = results_sc,
                     acohortTable = cohortTable,
                     aacohortId){
  covariateData_aux <- buildData(cdm_bbdd = acdm_bbdd,
                                 cdm_schema = acdm_schema,
                                 results_sc = aresults_sc,
                                 cohortTable = acohortTable,
                                 acohortId = aacohortId)
  covariateData2_aux <- FeatureExtraction::aggregateCovariates(covariateData_aux)
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
  cov_cate_resum_aux <- covariateData2_aux$covariateRef %>%
    dplyr::filter(analysisId %in% c(411, 413) & conceptId %in% sel_med_conceptId |
                    !(analysisId %in% c(411, 413))) %>%
    dplyr::inner_join(covariateData2_aux$covariates) %>%
    dplyr::mutate(covariateId = as.character(floor(covariateId)),
                  analysisId = as.integer(analysisId),
                  conceptId = as.integer(conceptId),
                  sumValue = as.integer(sumValue),
                  averageValue = averageValue*100) %>%
    dplyr::collect()
  cov_num_resum_aux <- covariateData2_aux$covariateRef %>%
    dplyr::inner_join(covariateData2_aux$covariatesContinuous) %>%
    dplyr::mutate(covariateId = as.character(floor(covariateId)),
                  analysisId = as.integer(analysisId),
                  conceptId = as.integer(conceptId)) %>%
    dplyr::select(-covariateId, -analysisId, -conceptId) %>%
    dplyr::collect()
  bbdd_covar_aux <- transformToFlat(covariateData_aux)
  bbdd_covar_aux <- buildFollowUp(cdm_bbdd = acdm_bbdd,
                                  cdm_schema = acdm_schema,
                                  results_sc = aresults_sc,
                                  cohortTable = acohortTable,
                                  acohortId = aacohortId,
                                  bbdd_covar = bbdd_covar_aux)
  return(list(cov_cate_resum = cov_cate_resum_aux,
              cov_num_resum = cov_num_resum_aux,
              bbdd_covar = bbdd_covar_aux))
}

###################################################################################################
# Package
###################################################################################################
library(SOPHIA)
library(DatabaseConnector)
library(magrittr)
library(tidyverse)

###################################################################################################
# Server Configuration
###################################################################################################
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

# Name in the server. Better in Renviron?
cdm_schema <- 'omop21t2_test'
results_sc <- 'sophia_test'
cohortTable <- 'cohortTable'

# SOPHIA package's directory
SOPHIAroot <- 'renv/library/R-4.1/x86_64-pc-linux-gnu/SOPHIA/'

###################################################################################################
# Cohort and Outcome creation
###################################################################################################
# Cohort T2DM
cohort_T2DM <- CreateSQL_T2DM(cdm_bbdd,
                              cdm_schema,
                              results_sc,
                              cohortTable)
# Cohort T1DM
cohort_T1DM <- CreateSQL_T1DM(cdm_bbdd,
                              cdm_schema,
                              results_sc,
                              cohortTable)
# Outcome AMI
outcome_AMI <- CreateSQL_AMI(cdm_bbdd,
                             cdm_schema,
                             results_sc,
                             cohortTable)
# Outcome Angor
outcome_angor <- CreateSQL_angor(cdm_bbdd,
                                 cdm_schema,
                                 results_sc,
                                 cohortTable)
# Outcome Stroke Ischemic
outcome_stroke_i <- CreateSQL_stroke_i(cdm_bbdd,
                                       cdm_schema,
                                       results_sc,
                                       cohortTable)
# Outcome TIA
outcome_TIA <- CreateSQL_TIA(cdm_bbdd,
                             cdm_schema,
                             results_sc,
                             cohortTable)

# Outcome Nephropathy
outcome_nephro <- CreateSQL_nephro(cdm_bbdd,
                                   cdm_schema,
                                   results_sc,
                                   cohortTable)

# Outcome Retinopathy
outcome_retino <- CreateSQL_retino(cdm_bbdd,
                                   cdm_schema,
                                   results_sc,
                                   cohortTable)

# Outcome Neuropathy
outcome_neuro <- CreateSQL_neuro(cdm_bbdd,
                                 cdm_schema,
                                 results_sc,
                                 cohortTable)

# Outcome PAD
outcome_PAD <- CreateSQL_PAD(cdm_bbdd,
                             cdm_schema,
                             results_sc,
                             cohortTable)

# Definition Set for cohort and outcome
cohortDefinitionSet <- data.frame(atlasId = rep(NA, 10),
                                  cohortId = 1:10,
                                  cohortName = c("SIDIAP T2DM-WP5",
                                                 "SIDIAP T1DM-WP4",
                                                 "Outcome: AMI",
                                                 "Outcome: Angor",
                                                 "Outcome: Stroke I",
                                                 "Outcome: TIA",
                                                 "Outcome: Nephropathy",
                                                 "Outcome: Retinopathy",
                                                 "Outcome: Neuropathy",
                                                 "Outcome: PAD"),
                                  sql = c(cohort_T2DM$ohdiSQL,
                                          cohort_T1DM$ohdiSQL,
                                          outcome_AMI$ohdiSQL,
                                          outcome_angor$ohdiSQL,
                                          outcome_stroke_i$ohdiSQL,
                                          outcome_TIA$ohdiSQL,
                                          outcome_nephro$ohdiSQL,
                                          outcome_retino$ohdiSQL,
                                          outcome_neuro$ohdiSQL,
                                          outcome_PAD$ohdiSQL),
                                  json = c(cohort_T2DM$circeJson,
                                           cohort_T1DM$circeJson,
                                           outcome_AMI$circeJson,
                                           outcome_angor$circeJson,
                                           outcome_stroke_i$circeJson,
                                           outcome_TIA$circeJson,
                                           outcome_nephro$circeJson,
                                           outcome_retino$circeJson,
                                           outcome_neuro$circeJson,
                                           outcome_PAD$circeJson),
                                  logicDescription = rep(as.character(NA), 10),
                                  generateStats = rep(T, 10))

# Creation and saving in the server
n_cohort <- createCohort(cdm_bbdd,
                         cdm_schema,
                         results_sc,
                         cohortTable,
                         cohortDefinitionSet)

###################################################################################################
# T1DM Cohort Analysis
###################################################################################################
bbdd_covar_T1DM_list <- FunCovar(aacohortId = 2)

bbdd_covar <- bbdd_covar_T1DM_list$bbdd_covar
if(!'DKA' %in% names(bbdd_covar)){
  bbdd_covar$DKA <- 0
}
if(!'Hypoglyc' %in% names(bbdd_covar)){
  bbdd_covar$Hypoglyc <- 0
}

df <- bbdd_covar %>%
  transmute(id = rowId,
            hba1c = HbA1c,
            cat.hba1c = factor(as.numeric(hba1c < 7)),
            fglucose = Glucose,
            cat.fglucose = factor(as.numeric(fglucose < 126)),
            dka = DKA,
            hypoglyc = factor(as.numeric(Glucose < 70 | Hypoglyc == 1)),
            sex = factor(sex_male),
            smoking = factor(Current),
            age,
            cat.age = factor(as.numeric(18 <= age)),
            bmi = BMI,
            cat.bmi = factor(if_else(cat.age == 1,
                                     if_else(bmi < 18.5, 'Underweight',
                                             if_else(bmi < 25, 'Normal weight',
                                                     if_else(bmi <= 30, 'Overweight', 'Obesity'))),
                                     'Pediatrics'),
                             levels = c('Normal weight', 'Underweight', 'Overweight', 'Obesity')),
            insulin = NULL,
            chol = cT,
            hdl = cHDL,
            ldl = cLDL,
            tg = Tg,
            cpep = PEPTIDCs,
            #complicacions
            #antibodies
            ica = NULL,
            gad = NULL,
            iaa = NULL,
            ia2a = NULL,
            znt8a = NULL,
            creatinine = Creatinine,
            albumin = Albumin,
            EGFR = CKDEPI,
            ast = GOT,
            alt = ALT,
            gamma.gt = GGT,
            vit.d = vitD,
            microalbumin = NULL)

source('script.R/get_descriptives.R')
