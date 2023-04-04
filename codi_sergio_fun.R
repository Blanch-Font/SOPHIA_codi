###################################################################################################
# Auxiliar package
###################################################################################################
devtools::install_github("ohdsi/CirceR")
devtools::install_github("ohdsi/Capr@a79c4d6d614dc916ae81c6c403e4f9be5c5f8eef")
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

FunStratDat_vr2 <- function(arecoded_dat){
  strat_mod <- arecoded_dat %>%
    dplyr::select(-crp) %>%
    tidyr::pivot_longer(-c(eid, age, sex, smoking, bmi),
                        names_to = "trait") %>%
    tidyr::nest(data = -c(sex, trait)) %>%
    dplyr::mutate(mod = purrr::map(data,
                                   ~ lm(value ~ age + smoking + bmi,
                                        data = .x)))
  strat_predrsd <- strat_mod %>%
    dplyr::transmute(sex,
                     trait,
                     eid = purrr::map(data, select, eid),
                     pred = purrr::map(mod, fitted),
                     rsd = purrr::map(mod, resid),
                     std_rsd = purrr::map(rsd, ~as.vector(scale(.x)))) %>%
    unnest(c(eid, pred, rsd, std_rsd))
  crp_data <- arecoded_dat %>%
    dplyr::select(eid, age, sex, smoking, bmi, crp) %>%
    tidyr::pivot_longer(-c(eid, age, sex, smoking, bmi),
                        names_to = "trait")
  crp_mod <- crp_data %>%
    dplyr::filter(!is.na(value)) %>%
    tidyr::nest(data = -c(sex, trait)) %>%
    dplyr::mutate(mod = purrr::map(data,
                                   ~ lm(value ~ age + smoking + bmi,
                                        data = .x)))
  strat_predrsd <- strat_predrsd %>%
    bind_rows(crp_data %>%
                dplyr::left_join(crp_mod %>%
                                   dplyr::transmute(sex,
                                                    trait,
                                                    eid = purrr::map(data, select, eid),
                                                    pred = purrr::map(mod, fitted),
                                                    rsd = purrr::map(mod, resid),
                                                    std_rsd = purrr::map(rsd, ~as.vector(scale(.x)))) %>%
                                   unnest(c(eid, pred, rsd, std_rsd)),
                                 by = c('sex', 'trait', 'eid')) %>%
                dplyr::select(sex, trait, eid, pred, rsd, std_rsd) %>%
                dplyr::mutate(rsd = dplyr::if_else(is.na(rsd), 0, rsd),
                              std_rsd = dplyr::if_else(is.na(std_rsd), 0, std_rsd))) %>%
    dplyr::arrange(sex, trait, eid)
  return(strat_predrsd)
}

# Function that returns probabilities for a given mixture of Gaussian distributions
# X = Data
# center = List of centers of each Gaussian distribution
# covmats = List of covariance matrices for each Gaussian distribution
# weights = List o weights for each Gaussian distribution
getclusprob <- function(X, centers, covmats, weights){
  # Calculating probability density functions
  pdfs <- purrr::map2(centers,
                      covmats,
                      function(mu, covmat) mvtnorm::dmvnorm(X, mu, covmat))
  # Calculating likelihoods
  L <- purrr::map2(pdfs,
                   weights,
                   function(pd, w) pd*w)
  # Joining in a matrix
  Lmat <- do.call(cbind, L)
  # Scaling by row to obtain probabilities
  probs <- Lmat/rowSums(Lmat)
  return(probs)
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
# cdm_schema <- 'omop21t2_test'
# results_sc <- 'sophia_test'
# cohortTable <- 'cohortTable'
cdm_schema <- 'omop21t2_cmbd'
results_sc <- 'results21t2_cmbd'
cohortTable <- 'sophia'

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
cohortDefinitionSet <- data.frame(atlasId = rep(NA, 18),
                                  cohortId = 1:18,
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
                                  generateStats = rep(T, 18))

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
# load('arch_mod.Rdata')
load('cluster_params.RData')

###################################################################################################
# T2DM Cohort Analysis
###################################################################################################
bbdd_covar_T2DM_list <- FunCovar(cdm_bbdd,
                                 cdm_schema,
                                 results_sc,
                                 cohortTable,
                                 acohortId = 1)
save(bbdd_covar_T2DM_list$cov_cate_resum,
     bbdd_covar_T2DM_list$cov_num_resum,
     file = 'taules_desc_T2DM.RData')

bbdd_covar <- bbdd_covar_T2DM_list$bbdd_covar
bbdd_umap <- FunDbUMAP(bbdd_covar)
recoded_dat <- FunRecodeDat(bbdd_umap)
# strat_dat <- FunStratDat(recoded_dat)
strat_predrsd <- FunStratDat_vr2(recoded_dat)
strat_dat <- strat_predrsd %>%
  dplyr::select(-c(pred, rsd)) %>%
  tidyr::pivot_wider(names_from = trait, values_from = std_rsd) %>%
  split(f = .$sex) %>%
  map(select, -sex)
umap_embed <- strat_dat %>%
  map2(umap_res, ~{
    an <- 65000
    nn <- ceiling(dim(.x)[1]/an)
    # print(nn)
    bind_rows(lapply(1:nn, function(i, xx = .x, yy = .y){
      # print(i)
      # print(min((1:an) + (i-1)*an, dim(xx)[1]))
      ax <- xx[(1:an) + (i-1)*an,]
      ax <- ax[complete.cases(ax),]
      # print(dim(ax))
      dat1 <- tibble::tibble(eid = ax$eid)
      # print(summary(ax))
      dat2 <- data.frame(uwot::umap_transform(X = ax %>% dplyr::select(-eid),
                                              model = yy))
      dplyr::bind_cols(dat1, dat2)
    }))
    # dat1 <- tibble::tibble(eid = .x$eid)
    # dat2 <- data.frame(uwot::umap_transform(X = .x %>% dplyr::select(-eid),
    #                                         model = .y))
    # dplyr::bind_cols(dat1, dat2)
  })

probs <- map2(strat_dat,
              cluster_params, ~{
                d <- select(.x, -eid)
                cluspars <- .y
                centers <- map(cluspars, pluck, "center")
                covmats <- map(cluspars, pluck, "cov")
                weights <- map(cluspars, pluck, "weight")
                getclusprob(d, centers, covmats, weights)
                })

clusmap <- map(probs, ~{
  data.frame(cluspos = 1:ncol(.x),
             cluster = colnames(.x))
})
maxprob <- map2(map2(strat_dat,
                     probs, ~{
                       data.frame(eid = .x$eid,
                                  cluspos = apply(.y, 1, which.max),
                                  maxprob = apply(.y, 1, max))
                      }),
                clusmap, ~{
                  .x %>%
                    inner_join(.y,
                               by = "cluspos") %>%
                    select(-cluspos)})
umap_embed_cluster <- map2(umap_embed,
                          maxprob, ~{
                            .x %>%
                              inner_join(.y,
                                         by = "eid")
                          }) %>%
  bind_rows(.id = 'sex')

# archdat <- arch_mod %>%
#   map(~ data.frame(.x$archetypes),
#       .id = "sex")
#
# arch_dict <- l<- list(
#   Female = tibble::tribble(
#     ~archnum, ~archnam,
#     1, "High HDL/High BP",
#     2, "High LDL",
#     3, "High TG",
#     4, "High HDL",
#     5, "High WHR",
#     6, "High CRP",
#     7, "High BP",
#     8, "Low BP",
#     9, "High ALT",
#     10, "High SCr",
#     11, "Low WHR",
#     12, "High FG"
#   ),
#   Male = tribble(
#     ~archnum, ~archnam,
#     1, "High SCr",
#     2, "High FG",
#     3, "High BP",
#     4, "High CRP",
#     5, "High HDL",
#     6, "Low BP",
#     7, "High TG",
#     8, "High HDL/High BP",
#     9, "Low WHR",
#     10, "High ALT"
#   )
# )
#
# arch_labs <- archdat %>%
#   imap(~{
#     dplyr::bind_cols(arch_dict[[.y]], .x)
#   })
# archdat_umap <- map(
#   setNames(c("Female", "Male"), c("Female", "Male")),
#   ~{
#     d <- arch_labs[[.x]] %>%
#       dplyr::select(-c(archnum, archnam))
#     res <- uwot::umap_transform(d,
#                                 umap_res[[.x]])
#     res <- data.frame(res)
#     arch_labs[[.x]] %>%
#       dplyr::select(archnam) %>%
#       dplyr::bind_cols(res)
#   }
# )
#
# arch_probs <- map2(
#   arch_mod,
#   strat_dat,
#   ~{
#     mod <- .x
#     mod$family$which <- "original"
#     dat <- .y %>%
#       dplyr::select(-eid)
#     p <- archetypes:::predict.archetypes(mod, dat)
#     data.frame(eid = .y$eid, p)
#   }) %>%
#   map2(arch_labs,
#        ~{colnames(.x) <- c("eid", gsub(" |/", "_", .y$archnam))
#        .x})
# arch_probs_long <- arch_probs %>%
#   map(tidyr::pivot_longer, -eid, names_to = "archnam", values_to = "prob") %>%
#   map(dplyr::mutate, archnam = gsub("_", " ", archnam))
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
bbdd_covar_T1DM_list <- FunCovar(cdm_bbdd,
                                 cdm_schema,
                                 results_sc,
                                 cohortTable,
                                 acohortId = 2)
save(bbdd_covar_T1DM_list$cov_cate_resum,
     bbdd_covar_T1DM_list$cov_num_resum,
     file = 'taules_desc_T1DM.RData')

bbdd_covar <- bbdd_covar_T1DM_list$bbdd_covar
bbdd_umap <- FunDbUMAP(bbdd_covar)
recoded_dat <- FunRecodeDat(bbdd_umap)
strat_dat <- FunStratDat(recoded_dat)
# umap_embed <- strat_dat %>%
#   map2(umap_res, ~{
#     dat1 <- tibble::tibble(eid = .x$eid)
#     dat2 <- data.frame(uwot::umap_transform(X = .x %>% dplyr::select(-eid),
#                                             model = .y))
#     dplyr::bind_cols(dat1, dat2)
#   })
umap_embed <- strat_dat %>%
  map2(umap_res, ~{
    an <- 65000
    nn <- ceiling(dim(.x)[1]/an)
    # print(nn)
    bind_rows(lapply(1:nn, function(i, xx = .x, yy = .y){
      # print(i)
      # print(min((1:an) + (i-1)*an, dim(xx)[1]))
      ax <- xx[(1:an) + (i-1)*an,]
      ax <- ax[complete.cases(ax),]
      # print(dim(ax))
      dat1 <- tibble::tibble(eid = ax$eid)
      # print(summary(ax))
      dat2 <- data.frame(uwot::umap_transform(X = ax %>% dplyr::select(-eid),
                                              model = yy))
      dplyr::bind_cols(dat1, dat2)
    }))
    # dat1 <- tibble::tibble(eid = .x$eid)
    # dat2 <- data.frame(uwot::umap_transform(X = .x %>% dplyr::select(-eid),
    #                                         model = .y))
    # dplyr::bind_cols(dat1, dat2)
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
