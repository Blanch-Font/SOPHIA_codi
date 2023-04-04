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
cdm_schema <- 'omop21t2_test'
results_sc <- 'sophia_test'
cohortTable <- 'cohortTable'

# SOPHIA package's directory
SOPHIAroot <- 'renv/library/R-4.1/x86_64-pc-linux-gnu/SOPHIA/'

###################################################################################################
# External objects
###################################################################################################
umap_res <- map(setNames(c("Female", "Male"), c("Female", "Male")),
                ~load_uwot(paste0("umap_model_", .x)))
# load('arch_mod.Rdata')
load('cluster_params.RData')

###################################################################################################
# T1DM Cohort Analysis
###################################################################################################

# UMAP WP4
bbdd_covar_T1DM_list <- FunCovar(cdm_bbdd,
                                 cdm_schema,
                                 results_sc,
                                 cohortTable,
                                 acohortId = 2)
save(bbdd_covar_T1DM_list$cov_cate_resum,
     bbdd_covar_T1DM_list$cov_num_resum,
     file = 'taules_desc_T1DM.RData')

bbdd_covar <- bbdd_covar_T1DM_list$bbdd_covar
# Descriptiva i Correlacions
if(!'DKA' %in% names(bbdd_covar)){
  bbdd_covar$DKA <- 0
}
if(!'Hypoglyc' %in% names(bbdd_covar)){
  bbdd_covar$Hypoglyc <- 0
}

df <- bbdd_covar %>%
  transmute(id = rowId,
            # AgeT1DM,
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
            cat.lipid = factor(as.numeric(cT_1y <= 190 &
                                            (cLDL_1y < 100 & age < 35 | cLDL_1y < 116 & 35 <= age) &
                                            (50 <= cHDL_1y & sex_female == 1 | 40 <= cHDL_1y & sex_male == 1) &
                                            Tg_1y < 150)),
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
            microalbumin = NULL,
            disdur = if_else(!is.na(TimeT1DM), TimeT1DM, TimeT1Rx)/365.25,
            sbp = SBP,
            dbp = DBP,
            card = pmax(i.ep_AMI_WP4, i.ep_stroke_WP4, as.numeric(!is.na(DEATH_DATE))),
            card.mi = factor(i.ep_AMI_WP4),
            card.stroke = factor(i.ep_stroke_WP4),
            card.angina = factor(i.ep_Angor_unstable),
            death = factor(as.numeric(!is.na(DEATH_DATE))),
            time.card = pmin(t.ep_AMI_WP4, t.ep_stroke_WP4),
            time.card.mi = t.ep_AMI_WP4,
            time.card.stroke = t.ep_stroke_WP4,
            time.card.angina = t.ep_Angor_unstable,
            time.death = as.numeric(pmin(DEATH_DATE, OBSERVATION_PERIOD_END_DATE, na.rm = T) - dintro)/365.25,
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

df <- df %>%
  left_join(meas_post %>%
              select(id, cont.hba1c, time.hba1c),
            by = 'id')

library(ggcorrplot)
tmp <- df %>%
  select(bmi, dbp, sbp, ldl, hdl, hba1c, tg, chol) %>%
  na.omit()

corr <- round(cor(tmp), 3)
p.mat <- cor_pmat(tmp)

corrplot <- ggcorrplot(corr,
                       hc.order = TRUE,
                       p.mat = p.mat,
                       insig = "blank",
                       type = "lower",
                       lab = TRUE)

ggsave("corrplot.pdf", corrplot)

source('get_descriptives_v3.0.R')
source('deliverable_4_2.R')
rmarkdown::render(input = 'deliverable_4_2.Rmd',
                  output_file = 'deliverable_4_2.pdf',
                  clean=T)

# Anàlisi WP4
IN <- 'NewUMAP_drugs_T1DM_sergio.Rmd'
OUT <- 'NewUMAP_drugs_T1DM_sergio.html'
.PATH = sprintf('.tmp/%s', strsplit(x = basename(OUT), split = "\\.")[[1]][1])
dir.create(.PATH, showWarnings = F, recursive = T)
file.copy(from = IN, to = .PATH, overwrite = T)
rmarkdown::render(input = paste(.PATH, basename(IN), sep = "/"),
                  output_dir = dirname(OUT),
                  output_file = basename(OUT),
                  clean = T)

# UMAP
bbdd_covar <- bbdd_covar_T1DM_list$bbdd_covar
bbdd_umap <- FunDbUMAP(bbdd_covar)
recoded_dat <- FunRecodeDat(bbdd_umap)
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

# ###################################################################################################
# # Cohort diagnosis
# ###################################################################################################
# fet_diag <- runDiagnostic(cdm_bbdd,
#                           cdm_schema,
#                           results_sc,
#                           cohortTable,
#                           cohortDefinitionSet)
# # CohortDiagnostics::launchDiagnosticsExplorer(sqliteDbPath = 'MergedCohortDiagnosticsData.sqlite')
disconnect(cdm_bbdd)
