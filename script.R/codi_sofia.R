###################################################################################################
# Auxiliar package
###################################################################################################
devtools::install_github("ohdsi/CirceR")
devtools::install_github("ohdsi/Capr@v1.0.3",)
devtools::install_github("ohdsi/CohortGenerator")
devtools::install_github("ohdsi/CohortDiagnostics")
# devtools::install(pkg = '~idiap/projects/SOPHIA')
devtools::install_github('Blanch-Font/SOPHIA')

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
# cdm_schema <- 'omop21t2_test'
# results_sc <- 'sophia_test'
# cohortTable <- 'cohortTable'
cdm_schema <- 'omop21t2'
results_sc <- 'results21t2_cmbd'
cohortTable <- 'sophia_sensecmbd'

# SOPHIA package's directory
SOPHIAroot <- 'renv/library/R-4.2/x86_64-pc-linux-gnu/SOPHIA/'

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

# Outcome Angor unstable
outcome_angor_unstable <- CreateSQL_angor_unstable(cdm_bbdd,
                                                   cdm_schema,
                                                   results_sc,
                                                   cohortTable)

# Outcome AMI
outcome_AMI_WP4 <- CreateSQL_AMI_WP4(cdm_bbdd,
                                     cdm_schema,
                                     results_sc,
                                     cohortTable)

# Outcome Stroke
outcome_strokeWP4 <- CreateSQL_strokeWP4(cdm_bbdd,
                                         cdm_schema,
                                         results_sc,
                                         cohortTable)

# Outcome Neuropathy
outcome_neuroWP4 <- CreateSQL_neuroWP4(cdm_bbdd,
                                       cdm_schema,
                                       results_sc,
                                       cohortTable)

# Outcome Nephropathy
outcome_nephroWP4 <- CreateSQL_nephroWP4(cdm_bbdd,
                                         cdm_schema,
                                         results_sc,
                                         cohortTable)

# Outcome Retinopathy
outcome_retinoWP4 <- CreateSQL_retinoWP4(cdm_bbdd,
                                         cdm_schema,
                                         results_sc,
                                         cohortTable)

# Outcome Diabetic foot
outcome_footWP4 <- CreateSQL_footWP4(cdm_bbdd,
                                     cdm_schema,
                                     results_sc,
                                     cohortTable)

# Outcome Diabetic ketoacidosis
outcome_DKAWP4 <- CreateSQL_DKAWP4(cdm_bbdd,
                                   cdm_schema,
                                   results_sc,
                                   cohortTable)

# Definition Set for cohort and outcome
cohortDefinitionSet <- data.frame(#atlasId = rep(NA, 17),
                                  atlasId = rep(NA, 18),
                                  cohortId = 1:18,
                                  # cohortId = c(1, 3:18),
                                  cohortName = c("SIDIAP T2DM-WP5",
                                                 "SIDIAP T1DM-WP4",
                                                 "Outcome: AMI",
                                                 "Outcome: Angor",
                                                 "Outcome: Stroke I",
                                                 "Outcome: TIA",
                                                 "Outcome: Nephropathy",
                                                 "Outcome: Retinopathy",
                                                 "Outcome: Neuropathy",
                                                 "Outcome: PAD",
                                                 "Outcome: Unstable Angor WP4",
                                                 "Outcome: AMI WP4",
                                                 "Outcome: Stroke WP4",
                                                 "Outcome: Neuro WP4",
                                                 "Outcome: Nephropathy due to DM1",
                                                 "Outcome: Retinopathy due to DM1",
                                                 "Outcome: Diabetic Foot due to DM1",
                                                 "Outcome: DKA due to DM1"),
                                  sql = c(cohort_T2DM$ohdiSQL,
                                          cohort_T1DM$ohdiSQL,
                                          outcome_AMI$ohdiSQL,
                                          outcome_angor$ohdiSQL,
                                          outcome_stroke_i$ohdiSQL,
                                          outcome_TIA$ohdiSQL,
                                          outcome_nephro$ohdiSQL,
                                          outcome_retino$ohdiSQL,
                                          outcome_neuro$ohdiSQL,
                                          outcome_PAD$ohdiSQL,
                                          outcome_angor_unstable$ohdiSQL,
                                          outcome_AMI_WP4$ohdiSQL,
                                          outcome_strokeWP4$ohdiSQL,
                                          outcome_neuroWP4$ohdiSQL,
                                          outcome_nephroWP4$ohdiSQL,
                                          outcome_retinoWP4$ohdiSQL,
                                          outcome_footWP4$ohdiSQL,
                                          outcome_DKAWP4$ohdiSQL),
                                  json = c(cohort_T2DM$circeJson,
                                           cohort_T1DM$circeJson,
                                           outcome_AMI$circeJson,
                                           outcome_angor$circeJson,
                                           outcome_stroke_i$circeJson,
                                           outcome_TIA$circeJson,
                                           outcome_nephro$circeJson,
                                           outcome_retino$circeJson,
                                           outcome_neuro$circeJson,
                                           outcome_PAD$circeJson,
                                           outcome_angor_unstable$circeJson,
                                           outcome_AMI_WP4$circeJson,
                                           outcome_strokeWP4$circeJson,
                                           outcome_neuroWP4$circeJson,
                                           outcome_nephroWP4$circeJson,
                                           outcome_retinoWP4$circeJson,
                                           outcome_footWP4$circeJson,
                                           outcome_DKAWP4$circeJson),
                                  logicDescription = rep(as.character(NA), 18),
                                  # logicDescription = rep(as.character(NA), 17),
                                  generateStats = rep(T, 18))
                                  # generateStats = rep(T, 17))

# Creation and saving in the server
n_cohort <- createCohort(cdm_bbdd,
                         cdm_schema,
                         results_sc,
                         cohortTable,
                         cohortDefinitionSet)

sql_cohort <- SqlRender::render(sql = "SELECT *
                                       FROM @results.@table
                                       WHERE cohort_definition_id = 2",
                                results = results_sc,
                                table = cohortTable)
cohort <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                      sql = sql_cohort)
vec_insulin <- c(1596977, 19058398, 19078552, 19078559, 19095211, 19095212, 19112791, 19133793,
                 19135264, 21076306, 21086042, 35410536, 35412958, 40051377, 40052768, 42479783,
                 42481504, 42481541, 42899447, 42902356, 42902587, 42902742, 42902821, 42902945,
                 42903059, 44058584, 46233969, 46234047, 46234234, 46234237, 1502905, 1513843,
                 1513876, 1516976, 1531601, 1544838, 1550023, 1562586, 1567198, 1586346, 1586369,
                 1588986, 1590165, 19013926, 19013951, 19090180, 19090187, 19090204,
                 19090221, 19090226, 19090229, 19090244, 19090247, 19090249, 19091621, 35198096,
                 35602717, 46221581)
# sql_insulin <- "SELECT *
#                 FROM @bbdd.DRUG_ERA
#                 WHERE person_id IN (@id)
#                   AND DRUG_CONCEPT_ID IN (@insulin)"
sql_insulin <- "SELECT *
                FROM @bbdd.DRUG_ERA de
                  JOIN @results.@table res ON de.person_id = res.subject_id
                WHERE res.cohort_definition_id = 2
                  AND DRUG_CONCEPT_ID IN (@insulin)"
insulin <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                       sql = SqlRender::render(sql_insulin,
                                                               bbdd = cdm_schema,
                                                               results = results_sc,
                                                               table = cohortTable,
                                                               # id = cohort$SUBJECT_ID,
                                                               insulin = vec_insulin))
insulin_treat <- dplyr::arrange(.data = insulin,
                                PERSON_ID, DRUG_ERA_START_DATE)
n_prev <- pull(count(insulin_treat), n)
cond <- TRUE
n_washout <- 1
while(cond){
  insulin_treat <- group_by(insulin_treat, PERSON_ID)
  insulin_treat <- mutate(insulin_treat,
                          canvi = lag(DRUG_ERA_END_DATE) + n_washout*30 < DRUG_ERA_START_DATE,
                          canvi = dplyr::if_else(is.na(canvi), FALSE, canvi),
                          treat = cumsum(canvi))
  insulin_treat <- group_by(insulin_treat, PERSON_ID, treat)
  insulin_treat <- summarise(insulin_treat,
                             DRUG_ERA_START_DATE = min(DRUG_ERA_START_DATE),
                             DRUG_ERA_END_DATE = max(DRUG_ERA_END_DATE),
                             .groups = 'drop')
  n_act <- pull(count(insulin_treat), n)
  cat('Prev: ', n_prev, ' Act: ', n_act, '\n')
  cond <- n_prev > n_act
  n_prev <- n_act
}
insulin_treat <- left_join(insulin_treat,
                           cohort %>% select(-COHORT_DEFINITION_ID),
                           by = c('PERSON_ID' = 'SUBJECT_ID'))
insulin_treat <- mutate(insulin_treat,
                        dbegin = pmax(DRUG_ERA_START_DATE, COHORT_START_DATE),
                        dend = pmin(DRUG_ERA_END_DATE, COHORT_END_DATE),
                        time = pmax(0, as.numeric(dend - dbegin + 1)/365.25),
                        time_obs = as.numeric(COHORT_END_DATE - COHORT_START_DATE + 1)/365.25)
insulin_treat <- group_by(insulin_treat, PERSON_ID)
insulin_treat <- summarise(insulin_treat,
                           time = sum(time),
                           time_obs = max(time_obs))
insulin_treat <- mutate(insulin_treat,
                        mpr = time/time_obs*100)
cohort <- dplyr::left_join(cohort,
                           insulin_treat,
                           by = c('SUBJECT_ID' = 'PERSON_ID'))

block_size <- 100
n_2 <- dim(cohort)[1]
cohort_mpr <- subset(cohort,
                     mpr < 75,
                     select = 'SUBJECT_ID')
n_mpr <- dim(cohort_mpr)[1]
n_cops <- n_mpr/block_size
for (i in 1:ceiling(n_cops)){
  executeSql(connection = cdm_bbdd,
             sql = SqlRender::render(sql = "DELETE FROM @results_sc.@cohortTable
                                            WHERE cohort_definition_id = 2
                                             AND subject_id IN (@id)",
                                     results_sc = results_sc,
                                     cohortTable = cohortTable,
                                     id = cohort_mpr[((i-1)*block_size + 1):min(i*block_size, n_mpr),]))
  sql_cohort_all <- "SELECT * FROM @results_sc.@cohortTable"
  cohort_all <- DatabaseConnector::querySql(connection = cdm_bbdd,
                                            sql = SqlRender::render(sql_cohort_all,
                                                                    results_sc = results_sc,
                                                                    cohortTable = cohortTable))
  print(table(cohort_all$COHORT_DEFINITION_ID))
}

###################################################################################################
# T1DM Cohort Analysis
###################################################################################################
bbdd_covar_T1DM_list <- FunCovar_T1(cdm_bbdd,
                                    cdm_schema,
                                    results_sc,
                                    cohortTable,
                                    acohortId = 2)

bbdd_covar <- bbdd_covar_T1DM_list$bbdd_covar
# Per agafar els incidents
sel <- c(which(bbdd_covar$TimeT1DM == 0 & bbdd_covar$TimeT1Rx == 0),
         which(is.na(bbdd_covar$TimeT1DM) & bbdd_covar$TimeT1Rx == 0),
         which(bbdd_covar$TimeT1DM == 0 & is.na(bbdd_covar$TimeT1Rx)))
bbdd_covar <- bbdd_covar[sel ,]

if(!'DKA' %in% names(bbdd_covar)){
  bbdd_covar$DKA <- 0
}
if(!'Hypoglyc' %in% names(bbdd_covar)){
  bbdd_covar$Hypoglyc <- 0
}

df <- bbdd_covar %>%
  transmute(id = rowId,
            AgeT1DM,
            hba1c = HbA1c,
            cat.hba1c = factor(as.numeric(hba1c < 7)),
            fglucose = Glucose,
            cat.fglucose = factor(as.numeric(fglucose < 126)),
            dka_t0 = factor(as.numeric(DKA)),
            hypoglyc = factor(as.numeric(Glucose < 70 | Hypoglyc == 1)),
            hypoglyc1 = factor(as.numeric(54 <= Glucose & Glucose < 70)),
            hypoglyc2 = factor(as.numeric(Glucose < 54)),
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
                             levels = c('Normal weight', 'Underweight', 'Overweight', 'Obesity', 'Pediatrics')),
            insulin = NULL,
            chol = cT,
            hdl = cHDL,
            ldl = cLDL,
            tg = Tg,
            # cat.lipid = factor(as.numeric(cT_1y <= 190 &
            #                                 (cLDL_1y < 100 & age < 35 | cLDL_1y < 116 & 35 <= age) &
            #                                 (50 <= cHDL_1y & sex_female == 1 | 40 <= cHDL_1y & sex_male == 1) &
            #                                 Tg_1y < 150)),
            cpep = if_else(PEPTIDCs != -99999, PEPTIDCs, NA),
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
            microalbumin = NULL,
            disdur = if_else(!is.na(TimeT1DM), TimeT1DM, TimeT1Rx)/365.25,
            sbp = SBP,
            dbp = DBP,
            card = factor(pmax(i.ep_AMI_WP4, i.ep_stroke_WP4,
                               as.numeric(!is.na(DEATH_DATE) & (DEATH_DATE - dintro) < 365))),
            card.mi = factor(i.ep_AMI_WP4),
            card.stroke = factor(i.ep_stroke_WP4),
            card.angina = factor(i.ep_Angor_unstable),
            death = factor(as.numeric(!is.na(DEATH_DATE) & (DEATH_DATE - dintro) < 365)),
            time.card = pmin(t.ep_AMI_WP4, t.ep_stroke_WP4),
            time.card.mi = t.ep_AMI_WP4,
            time.card.stroke = t.ep_stroke_WP4,
            time.card.angina = t.ep_Angor_unstable,
            # time.death = as.numeric(pmin(DEATH_DATE, OBSERVATION_PERIOD_END_DATE, na.rm = T) - dintro),
            time.death = pmin(as.numeric(if_else(is.na(DEATH_DATE),
                                                 OBSERVATION_PERIOD_END_DATE,
                                                 DEATH_DATE) - dintro)/365.25,
                              1),
            neuro = factor(i.ep_neuroWP4),
            time.neuro = t.ep_neuroWP4,
            nefro =factor(i.ep_nephroWP4),
            time.nefro = t.ep_nephroWP4,
            ret = factor(i.ep_retinoWP4),
            time.ret = t.ep_retinoWP4,
            foot = factor(i.ep_footWP4),
            time.foot = t.ep_footWP4,
            dka = factor(i.ep_DKAWP4),
            time.dka = t.ep_DKAWP4)

sql <- "SELECT cohort.subject_id,
               cohort.cohort_start_date,
               meas.measurement_concept_id,
               meas.measurement_date,
               meas.value_as_number
        FROM @cdm_database_schema.MEASUREMENT meas
              INNER JOIN @cohort_table cohort
                    ON cohort.subject_id = meas.person_id
                       AND cohort.cohort_definition_id = 2
        WHERE meas.measurement_concept_id IN (3034639, 3004410)
        ORDER BY cohort.subject_id, meas.measurement_date"
sql <- SqlRender::render(sql,
                         cdm_database_schema = cdm_schema,
                         cohort_table = paste(results_sc, cohortTable, sep = '.'))
sql <- SqlRender::translate(sql, targetDialect = attr(cdm_bbdd, "dbms"))
# Retrieve the covariate:
meas <- DatabaseConnector::querySql(cdm_bbdd, sql, snakeCaseToCamelCase = TRUE)
meas <- meas %>%
  mutate(cont.hba1c = 7 <= valueAsNumber)
meas_post <- df %>%
  filter(cat.hba1c == 1) %>%
  inner_join(meas %>%
               filter(cohortStartDate < measurementDate &
                        cont.hba1c == TRUE) %>%
               group_by(subjectId) %>%
               summarise_all(first),
             by = c("id" = "subjectId")) %>%
  mutate(time.hba1c = as.numeric(measurementDate - cohortStartDate)/365.25)

# df <- df %>%
#   left_join(meas_post %>%
#               select(id, cont.hba1c, time.hba1c),
#             by = 'id')

source('script.R/get_descriptives_v3.0.R')
source('deliverable_4_2.R')
rmarkdown::render(input = 'deliverable_4_2.Rmd',
                  output_file = 'deliverable_4_2.pdf',
                  clean=T)
