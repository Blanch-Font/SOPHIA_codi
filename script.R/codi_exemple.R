# devtools::install_github("ohdsi/CirceR")
# devtools::install_github("ohdsi/Capr")
# devtools::install_github("ohdsi/CohortGenerator")
# devtools::install_github("ohdsi/CohortDiagnostics")
# devtools::install(pkg = '~idiap/projects/SOPHIA')
# devtools::install_github("Blanch-Font/SOPHIA")

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

exportFolder <- 'export'
CohortDiagnostics::launchDiagnosticsExplorer(dataFolder = paste0('~idiap/projects/SOPHIA_codi/',
                                                                 exportFolder),
                                             dataFile = "Premerged_SIDIAP.RData")

###################################################################################################
covariateData <- buildData(cdm_bbdd,
                           cdm_schema,
                           results_sc,
                           cohortTable)
# tidyCovariates <- FeatureExtraction::tidyCovariateData(covariateData,
#                                                        minFraction = 0.001,
#                                                        normalize = FALSE,
#                                                        removeRedundancy = TRUE)
# covariateData2 <- FeatureExtraction::aggregateCovariates(tidyCovariates)
covariateData2 <- FeatureExtraction::aggregateCovariates(covariateData)

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

bbdd_covar <- transformToFlat(covariateData)

save(bbdd_covar,
     file = 'data.RData')

###################################################################################################

ind_cc <- complete.cases(bbdd_covar %>%
                           dplyr::select(rowId, age, BMI, Leukocytes, cT, Tg, cHDL,
                                         DBP, SBP, Glucose)) #, HbA1c
bbdd_covar_cc <- bbdd_covar[ind_cc,] %>%
  dplyr::select(rowId, age, BMI, Leukocytes, cT, Tg, cHDL, DBP, SBP, Glucose,
                sex_female, obesity, Current, Former,
                A10, C01, C02, C03, C07, C08, C09, C10, M01A) %>%
  dplyr::mutate(obesity_BMI = as.numeric(obesity == 1 | 30 <= BMI))

bbdd_covar_cc %>%
  dplyr::select(rowId, age, BMI, Leukocytes, cT, Tg, cHDL, DBP, SBP, Glucose,
                sex_female) %>%
  tidyr::pivot_longer(-c(rowId, sex_female)) %>%
  ggplot2::ggplot(ggplot2::aes(value)) +
  ggplot2::geom_histogram(bins = 50) +
  ggplot2::facet_grid(sex_female ~ name, scales = "free_x")

round(cor(bbdd_covar_cc %>%
            dplyr::select(age, BMI, Leukocytes, cT, Tg, cHDL, DBP, SBP, Glucose)), 2)

bbdd_covar_cc %>%
  dplyr::select(rowId, age, BMI, Leukocytes, cT, Tg, cHDL, DBP, SBP, Glucose,
                sex_female) %>%
  dplyr::group_split(sex_female) %>%
  purrr::map(~{
    .x %>%
      dplyr::select(-c(rowId, sex_female)) %>%
      cor %>%
      reshape2::melt() %>%
      ggplot2::ggplot(ggplot2::aes(Var1, Var2, fill = value)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                                    limit = c(-1,1)) +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = NULL, y = NULL, fill = "Correlation")
  })#%>%
  # patchwork::wrap_plots(nrow = 1, guides = "collect")

strat_dat <- bbdd_covar_cc %>%
  dplyr::group_by(sex_female) %>%
  dplyr::group_modify(~{
    .x %>%
      dplyr::transmute(
        rowId, age, BMI, obesity, Current, Former, A10, C01, C02, C03, C07, C08, C09, C10, M01A,
        dplyr::across(c(Leukocytes, cT, Tg, cHDL, DBP, SBP, Glucose),
               function(v,...){
                 d <- data.frame(vcol = v, a = .x$age, b = .x$BMI, c = .x$Current)
                 lm(vcol ~ a + b, data = d) %>%
                   resid %>% scale %>% as.vector
               }))
  }) %>%
  dplyr::ungroup()

strat_dat %>%
  dplyr::select(rowId, age, BMI, Leukocytes, cT, Tg, cHDL,
                DBP, SBP, Glucose, sex_female) %>%
  tidyr::pivot_longer(-c(rowId, sex_female)) %>%
  ggplot2::ggplot(ggplot2::aes(value)) +
  ggplot2::geom_histogram(bins = 50) +
  ggplot2::facet_grid(sex_female ~ name, scales = "free_x")

strat_dat %>%
  dplyr::select(rowId, age, BMI, Leukocytes, cT, Tg, cHDL, DBP, SBP, Glucose,
                sex_female) %>%
  dplyr::group_split(sex_female) %>%
  purrr::map(~{
    .x %>%
      dplyr::select(-c(rowId, sex_female)) %>%
      cor %>%
      reshape2::melt() %>%
      ggplot2::ggplot(ggplot2::aes(Var1, Var2, fill = value)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                                    limit = c(-1,1)) +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = NULL, y = NULL, fill = "Correlation")
  })

pca_res <- strat_dat %>%
  dplyr::select(rowId, Leukocytes, cT, Tg, cHDL, DBP, SBP, Glucose, sex_female) %>%
  split(f = .$sex_female) %>%
  purrr::map(~.x %>% dplyr::select(-c(rowId, sex_female)) %>% prcomp)

pca_res %>%
  purrr::imap(
    ~{
      eigs <- .x$sdev^2
      varexp <- round(eigs / sum(eigs), 4)
      data.frame(ncomp = 1:length(varexp), ve = 100 * varexp) %>%
        dplyr::mutate(ve_cs = cumsum(ve)) %>%
        ggplot2::ggplot(ggplot2::aes(ncomp, ve_cs)) +
        ggplot2::geom_area(fill = "lightblue", alpha = .5) +
        ggplot2::geom_line(group = 1) +
        ggplot2::geom_point() +
        ggplot2::geom_hline(yintercept = 95, linetype = "dashed", color = "red") +
        ggplot2::scale_x_continuous(breaks = 1:length(varexp)) +
        ggplot2::theme_bw() +
        ggplot2::labs(title = .y, x = NULL, y = NULL)
    }
  ) %>%
  purrr::modify_at(1, ~.x + ggplot2::labs(y = "Cumulative % variance explained")) #%>%
  # wrap_plots(nrow = 1) %>%
  # wrap_plots(grid::textGrob("Number of components"), ncol = 1, heights = c(.99, .01))

pca_res %>%
  purrr::imap(
    ~{
      eigs <- .x$sdev^2
      varexp <- round(eigs / sum(eigs), 4)
      .x %>%
        "$"("x") %>%
        "["(,c(1,2)) %>%
        data.frame %>%
        ggplot2::ggplot(ggplot2::aes(PC1, PC2)) +
        ggplot2::geom_point(alpha = .1) +
        ggplot2::theme_bw() +
        ggplot2::labs(title = .y,
             x = paste0("PC1 (", varexp[1] * 100, "%)"),
             y = paste0("PC2 (", varexp[2] * 100, "%)"))
    }
  )#%>%
  # wrap_plots(nrow = 1)

pca_res %>%
  purrr::imap(
    ~{
      eigs <- .x$sdev^2
      varexp <- round(eigs / sum(eigs), 4)
      .x %>%
        "$"("x") %>%
        "["(,c(1,3)) %>%
        data.frame %>%
        ggplot2::ggplot(ggplot2::aes(PC1, PC3)) +
        ggplot2::geom_point(alpha = .1) +
        ggplot2::theme_bw() +
        ggplot2::labs(title = .y,
                      x = paste0("PC1 (", varexp[1] * 100, "%)"),
                      y = paste0("PC3 (", varexp[2] * 100, "%)"))
    }
  )#%>%

aux_kmeans <- kmeans(x = bbdd_covar_cc, centers = 2)
plot(aux_kmeans)
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
                       dplyr::collect()) %>%
  flextable::colformat_double(x = ., digits = 1)

result <- createTable1(covariateData2, output = 'one column')
View(result)
print(result, row.names = FALSE, right = FALSE)

disconnect(cdm_bbdd)
