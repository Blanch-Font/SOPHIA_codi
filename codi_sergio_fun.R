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
# Package
###################################################################################################
library(SOPHIA)
library(DatabaseConnector)
library(magrittr)
library(dplyr)
library(tidyr)
library(purrr)
library(uwot)

# library(flextable)
# library(ggplot2)
# library(patchwork)
# library(purrr)
# library(gridExtra)
# library(ClusterR)
# library(splines)

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

# Function to remove outlier defined as +-5*sd
remove_outliers <- function(x){
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  upperbound <- m + (5*s)
  lowerbound <- m - (5*s)
  ifelse((x > lowerbound) & (x < upperbound), x, NaN)
}

# Function to create Database for analysis
# Entrada taula plana creada amb FunCovar
# Sortida taula per fer l¡anàlisis d'en Daniel
FunDbUMAP <- function(abbdd_covar){
  abbdd_covar %>%
    dplyr::mutate(sex = dplyr::if_else(sex_female == 1, 'Female', 'Male'),
                  WHR = dplyr::if_else(sex_female == 1,
                                       dplyr::if_else(obesity == 1 | 30 <= BMI, 0.9, 0.8),
                                       dplyr::if_else(obesity == 1 | 30 <= BMI, 1, 0.9)),
                  CRP = dplyr::if_else(20 < CRP, as.numeric(NA), CRP)) %>%
    dplyr::select(eid = rowId,
                  age,
                  sex,
                  bmi = BMI,
                  whr = WHR,
                  sbp = SBP,
                  dbp = DBP,
                  alt = ALT,
                  scr =  Creatinine,
                  crp = CRP,
                  hdl = cHDL,
                  tg = Tg,
                  ldl = cLDL,
                  fg = Glucose,
                  smoking = Current) %>%
    ## Removing outliers for all variables (except age)
    dplyr::mutate(dplyr::across(-c(eid, sex, age), remove_outliers))
}

FunRecodeDat <- function(abbdd_umap){
  bbdd_umap %>%
    dplyr::select(-crp) %>%
    ## Only complete cases
    tidyr::drop_na() %>%
    dplyr::left_join(bbdd_umap %>%
                       dplyr::select(eid, crp),
                     by = 'eid')
}

FunStratDat <- function(arecoded_dat){
  aux_strat_dat <- arecoded_dat %>%
    dplyr::select(-crp) %>%
    split(f = .$sex) %>%
    map(~{.x %>%
        names %>%
        setdiff(c("eid", "sex", "age", "smoking", "bmi")) %>%
        map_dfc(function(feature){
          paste(feature, "~ age + smoking + bmi") %>%
            lm(data = .x) %>%
            resid %>%
            scale %>%
            data.frame %>%
            setNames(feature)
        }) %>%
        dplyr::mutate(eid = .x$eid, .before = 1)
      })
  map2(
    aux_strat_dat,
    arecoded_dat %>%
      split(f = .$sex),
    ~{y_cc <- .y %>%
      dplyr::select(eid, crp, age, smoking, bmi) %>%
      tidyr::drop_na()
    ay_cc <- lm(formula = crp ~ age + smoking + bmi,
                data = y_cc) %>%
      resid %>%
      scale %>%
      data.frame %>%
      setNames('crp_nou')
    y_cc <- y_cc %>%
      dplyr::bind_cols(ay_cc)
    .x %>%
      dplyr::left_join(y_cc %>%
                         dplyr::select(eid, crp = crp_nou),
                       by = 'eid') %>%
      dplyr::mutate(crp = dplyr::if_else(is.na(crp), 0, crp))
  })
}

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
# External objects
###################################################################################################
umap_res <- map(setNames(c("Female", "Male"), c("Female", "Male")),
                ~load_uwot(paste0("umap_model_", .x)))
load('arch_mod.Rdata')

###################################################################################################
# T2DM Cohort Analysis
###################################################################################################
bbdd_covar_T2DM_list <- FunCovar(aacohortId = 1)
save(bbdd_covar_T2DM_list$cov_cate_resum,
     bbdd_covar_T2DM_list$cov_num_resum,
     file = 'taules_desc_T2DM.RData')

bbdd_covar <- bbdd_covar_T2DM_list$bbdd_covar
bbdd_umap <- FunDbUMAP(bbdd_covar)
recoded_dat <- FunRecodeDat(bbdd_umap)
strat_dat <- FunStratDat(recoded_dat)
umap_embed <- strat_dat %>%
  map2(umap_res, ~{
    dat1 <- tibble::tibble(eid = .x$eid)
    dat2 <- data.frame(uwot::umap_transform(X = .x %>% dplyr::select(-eid),
                                            model = .y))
    dplyr::bind_cols(dat1, dat2)
  })
archdat <- arch_mod %>%
  map(~ data.frame(.x$archetypes),
      .id = "sex")

arch_dict <- l<- list(
  Female = tibble::tribble(
    ~archnum, ~archnam,
    1, "High HDL/High BP",
    2, "High LDL",
    3, "High TG",
    4, "High HDL",
    5, "High WHR",
    6, "High CRP",
    7, "High BP",
    8, "Low BP",
    9, "High ALT",
    10, "High SCr",
    11, "Low WHR",
    12, "High FG"
  ),
  Male = tribble(
    ~archnum, ~archnam,
    1, "High SCr",
    2, "High FG",
    3, "High BP",
    4, "High CRP",
    5, "High HDL",
    6, "Low BP",
    7, "High TG",
    8, "High HDL/High BP",
    9, "Low WHR",
    10, "High ALT"
  )
)

arch_labs <- archdat %>%
  imap(~{
    dplyr::bind_cols(arch_dict[[.y]], .x)
  })
archdat_umap <- map(
  setNames(c("Female", "Male"), c("Female", "Male")),
  ~{
    d <- arch_labs[[.x]] %>%
      dplyr::select(-c(archnum, archnam))
    res <- uwot::umap_transform(d,
                                umap_res[[.x]])
    res <- data.frame(res)
    arch_labs[[.x]] %>%
      dplyr::select(archnam) %>%
      dplyr::bind_cols(res)
  }
)

arch_probs <- map2(
  arch_mod,
  strat_dat,
  ~{
    mod <- .x
    mod$family$which <- "original"
    dat <- .y %>%
      dplyr::select(-eid)
    p <- archetypes:::predict.archetypes(mod, dat)
    data.frame(eid = .y$eid, p)
  }) %>%
  map2(arch_labs,
       ~{colnames(.x) <- c("eid", gsub(" |/", "_", .y$archnam))
       .x})
arch_probs_long <- arch_probs %>%
  map(tidyr::pivot_longer, -eid, names_to = "archnam", values_to = "prob") %>%
  map(dplyr::mutate, archnam = gsub("_", " ", archnam))
umap_disease <- umap_embed %>%
  map2(.y = bbdd_covar %>%
         dplyr::rename(eid = rowId) %>%
         dplyr::mutate(sex = dplyr::if_else(sex_female == 1, 'Female', 'Male')) %>%
         split(f = .$sex), ~{
           .x %>%
             dplyr::left_join(.y,
                       by = 'eid')
         })
IN <- 'umap_daniel.Rmd'
OUT <- 'umap_daniel_T2DM.html'
.PATH = sprintf('.tmp/%s', strsplit(x = basename(OUT), split = "\\.")[[1]][1])
dir.create(.PATH, showWarnings = F, recursive = T)
file.copy(from = IN, to = .PATH, overwrite = T)
rmarkdown::render(input = paste(.PATH, basename(IN), sep = "/"),
                  output_dir = dirname(OUT),
                  output_file = basename(OUT),
                  clean = T)

###################################################################################################
# T1DM Cohort Analysis
###################################################################################################
bbdd_covar_T1DM_list <- FunCovar(aacohortId = 2)
save(bbdd_covar_T1DM_list$cov_cate_resum,
     bbdd_covar_T1DM_list$cov_num_resum,
     file = 'taules_desc_T1DM.RData')

bbdd_covar <- bbdd_covar_T1DM_list$bbdd_covar
bbdd_umap <- FunDbUMAP(bbdd_covar)
recoded_dat <- FunRecodeDat(bbdd_umap)
strat_dat <- FunStratDat(recoded_dat)
umap_embed <- strat_dat %>%
  map2(umap_res, ~{
    dat1 <- tibble::tibble(eid = .x$eid)
    dat2 <- data.frame(uwot::umap_transform(X = .x %>% dplyr::select(-eid),
                                            model = .y))
    dplyr::bind_cols(dat1, dat2)
  })
archdat <- arch_mod %>%
  map(~ data.frame(.x$archetypes),
      .id = "sex")

arch_dict <- l<- list(
  Female = tibble::tribble(
    ~archnum, ~archnam,
    1, "High HDL/High BP",
    2, "High LDL",
    3, "High TG",
    4, "High HDL",
    5, "High WHR",
    6, "High CRP",
    7, "High BP",
    8, "Low BP",
    9, "High ALT",
    10, "High SCr",
    11, "Low WHR",
    12, "High FG"
  ),
  Male = tribble(
    ~archnum, ~archnam,
    1, "High SCr",
    2, "High FG",
    3, "High BP",
    4, "High CRP",
    5, "High HDL",
    6, "Low BP",
    7, "High TG",
    8, "High HDL/High BP",
    9, "Low WHR",
    10, "High ALT"
  )
)

arch_labs <- archdat %>%
  imap(~{
    dplyr::bind_cols(arch_dict[[.y]], .x)
  })
archdat_umap <- map(
  setNames(c("Female", "Male"), c("Female", "Male")),
  ~{
    d <- arch_labs[[.x]] %>%
      dplyr::select(-c(archnum, archnam))
    res <- uwot::umap_transform(d,
                                umap_res[[.x]])
    res <- data.frame(res)
    arch_labs[[.x]] %>%
      dplyr::select(archnam) %>%
      dplyr::bind_cols(res)
  }
)

arch_probs <- map2(
  arch_mod,
  strat_dat,
  ~{
    mod <- .x
    mod$family$which <- "original"
    dat <- .y %>%
      dplyr::select(-eid)
    p <- archetypes:::predict.archetypes(mod, dat)
    data.frame(eid = .y$eid, p)
  }) %>%
  map2(arch_labs,
       ~{colnames(.x) <- c("eid", gsub(" |/", "_", .y$archnam))
       .x})
arch_probs_long <- arch_probs %>%
  map(tidyr::pivot_longer, -eid, names_to = "archnam", values_to = "prob") %>%
  map(dplyr::mutate, archnam = gsub("_", " ", archnam))
umap_disease <- umap_embed %>%
  map2(.y = bbdd_covar %>%
         dplyr::rename(eid = rowId) %>%
         dplyr::mutate(sex = dplyr::if_else(sex_female == 1, 'Female', 'Male')) %>%
         split(f = .$sex), ~{
           .x %>%
             dplyr::left_join(.y,
                              by = 'eid')
         })
IN <- 'umap_daniel.Rmd'
OUT <- 'umap_daniel_T1DM.html'
.PATH = sprintf('.tmp/%s', strsplit(x = basename(OUT), split = "\\.")[[1]][1])
dir.create(.PATH, showWarnings = F, recursive = T)
file.copy(from = IN, to = .PATH, overwrite = T)
rmarkdown::render(input = paste(.PATH, basename(IN), sep = "/"),
                  output_dir = dirname(OUT),
                  output_file = basename(OUT),
                  clean = T)

###################################################################################################
# Cohort diagnosis
###################################################################################################
fet_diag <- runDiagnostic(cdm_bbdd,
                          cdm_schema,
                          results_sc,
                          cohortTable,
                          cohortDefinitionSet)
# CohortDiagnostics::launchDiagnosticsExplorer(sqliteDbPath = 'MergedCohortDiagnosticsData.sqlite')
disconnect(cdm_bbdd)
