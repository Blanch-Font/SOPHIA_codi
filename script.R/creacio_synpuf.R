#' SYNPUF Creation as SQL
#'
#' Build a SYNPUF database as SQL
#'
#' @return SQL connection
#' @exports
#'
#' @examples
#' connection <- crea_synpuf()
#' DatabaseConnector::querySql(connection,
#'          "SELECT COUNT(*) FROM person;")
#' DatabaseConnector::getTableNames(connection,
#'                                  databaseSchema = 'main')
crea_synpuf <- function(){

  # Connexió
  # con <- dbConnect(RSQLite::SQLite(),
  #                  dbname = ":memory:")
  # con <- DatabaseConnector::connect(dbms = "sqlite",
  #                                   server = tempfile())
  con <- DatabaseConnector::connect(dbms = 'postgresql',
                                    user = 'jordi',
                                    password = 'jordi',
                                    server = 'localhost/postgres')

  person <- data.table::fread(input = 'inst/extdata/person.csv',
                              sep = '\t',
                              header = FALSE)
  names(person) <- c('person_id',
                     'gender_concept_id',
                     'year_of_birth',
                     'month_of_birth',
                     'day_of_birth',
                     'birth_datetime',
                     'race_concept_id',
                     'ethnicity_concept_id',
                     'location_id',
                     'provider_id',
                     'care_site_id',
                     'person_source_value',
                     'gender_source_value',
                     'gender_source_concept_id',
                     'race_source_value',
                     'race_source_concept_id',
                     'ethnicity_source_value',
                     'ethnicity_source_concept_id')
  DatabaseConnector::insertTable(connection = con,
                                 databaseSchema = 'synpuf',
                                 tableName = "PERSON",
                                 data = person,
                                 createTable = TRUE)
  # DBI::dbWriteTable(conn = con,
  #                   name = DBI::Id(schema = SQL('synpuf'), name = SQL("PERSON")),
  #                   value =  person)
  # DBI::dbGetQuery(conn = con,
  #                 statement = "SELECT * INTO SYNPUF.PERSON FROM PERSON")

  obs_per <- data.table::fread(input = 'inst/extdata/observation_period.csv',
                               sep = '\t',
                               header = FALSE)
  names(obs_per) <- c('observation_period_id',
                      'person_id',
                      'observation_period_start_date',
                      'observation_period_end_date',
                      'period_type_concept_id')
  obs_per[, `:=`(observation_period_start_date = as.Date(observation_period_start_date),
                 observation_period_end_date = as.Date(observation_period_end_date))]
  DatabaseConnector::insertTable(connection = con,
                                 databaseSchema = 'synpuf',
                                 tableName = "OBSERVATION_PERIOD",
                                 data = obs_per,
                                 createTable = TRUE)
  # dbWriteTable(conn = con,
  #              name = "OBSERVATION_PERIOD",
  #              value =  obs_per)

  vis_occ <- data.table::fread(input = 'inst/extdata/visit_occurrence.csv',
                               sep = '\t',
                               header = FALSE)
  names(vis_occ) <- c('visit_occurrence_id',
                      'person_id',
                      'visit_concept_id',
                      'visit_start_date',
                      'visit_start_datetime',
                      'visit_end_date',
                      'visit_end_datetime',
                      'visit_type_concept_id',
                      'provider_id',
                      'care_site_id',
                      'visit_source_value',
                      'visit_source_concept_id',
                      'admitting_source_concept_id',
                      'admitting_source_value',
                      'discharge_to_concept_id',
                      'discharge_to_source_value',
                      'preceding_visit_occurrence_id')
  vis_occ[, `:=`(visit_start_date = as.Date(visit_start_date),
                 visit_start_datetime = visit_start_datetime + 1,
                 visit_end_date = as.Date(visit_end_date),
                 visit_end_datetime = visit_end_datetime + 1)]
  DatabaseConnector::insertTable(connection = con,
                                 databaseSchema = 'synpuf',
                                 tableName = "VISIT_OCCURRENCE",
                                 data = vis_occ,
                                 createTable = TRUE)
  # dbWriteTable(conn = con,
  #              name = "VISIT_OCCURRENCE",
  #              value = vis_occ)

  co2 <- data.table::fread(input = 'inst/extdata/condition_occurrence.csv',
                           sep = '\t',
                           header = FALSE)
  names(co2) <- c('condition_occurrence_id',
                  'person_id',
                  'condition_concept_id',
                  'condition_start_date',
                  'condition_start_datetime',
                  'condition_end_date',
                  'condition_end_datetime',
                  'condition_type_concept_id',
                  # 'condition_status_concept_id',
                  'stop_reason',
                  'provider_id',
                  'visit_occurrence_id',
                  # 'visit_detail_id',
                  'condition_source_value',
                  'condition_source_concept_id',
                  'condition_status_source_value',
                  'condition_status_concept_id')
  co2[, `:=`(condition_start_date = as.Date(condition_start_date),
             condition_start_datetime = condition_start_datetime + 1,
             condition_end_date = as.Date(condition_end_date))]
  DatabaseConnector::insertTable(connection = con,
                                 databaseSchema = 'synpuf',
                                 tableName = "CONDITION_OCCURRENCE",
                                 data = co2,
                                 createTable = TRUE)
  # dbWriteTable(conn = con,
  #              name = "CONDITION_OCCURRENCE",
  #              value = co2)

  drug_exp <- data.table::fread(input = 'inst/extdata/drug_exposure.csv',
                                sep = '\t',
                                header = FALSE)
  names(drug_exp) <- c('drug_exposure_id',
                       'person_id',
                       'drug_concept_id',
                       'drug_exposure_start_date',
                       'drug_exposure_start_datetime',
                       'drug_exposure_end_date',
                       'drug_exposure_end_datetime',
                       'verbatim_end_date',
                       'drug_type_concept_id',
                       'stop_reason',
                       'refills',
                       'quantity',
                       'days_supply',
                       'sig',
                       'route_concept_id',
                       'lot_number',
                       'provider_id',
                       'visit_occurrence_id',
                       # 'visit_detail_id',
                       'drug_source_value',
                       'drug_source_concept_id',
                       'route_source_value',
                       'dose_unit_source_value')
  drug_exp[, `:=`(drug_exposure_start_date =as.Date(drug_exposure_start_date),
                  drug_exposure_start_datetime = drug_exposure_start_datetime + 1,
                  drug_exposure_end_date = as.Date(drug_exposure_end_date),
                  drug_exposure_end_datetime = drug_exposure_end_datetime + 1,
                  verbatim_end_date = as.Date(verbatim_end_date))]
  DatabaseConnector::insertTable(connection = con,
                                 databaseSchema = 'synpuf',
                                 tableName = "DRUG_EXPOSURE",
                                 data = drug_exp,
                                 createTable = TRUE)
  # dbWriteTable(conn = con,
  #              name = "DRUG_EXPOSURE",
  #              value = drug_exp)

  proc_occ <- data.table::fread(input = 'inst/extdata/procedure_occurrence.csv',
                                sep = '\t',
                                header = FALSE)
  names(proc_occ) <- c('procedure_occurrence_id',
                       'person_id',
                       'procedure_concept_id',
                       'procedure_date',
                       'procedure_datetime',
                       'procedure_type_concept_id',
                       'modifier_concept_id',
                       'quantity',
                       'provider_id',
                       'visit_occurrence_id',
                       # 'visit_detail_id',
                       'procedure_source_value',
                       'procedure_source_concept_id',
                       'qualifier_source_value')
                       # 'modifier_source_value')
  proc_occ[, `:=`(procedure_date = as.Date(procedure_date),
                  procedure_datetime = procedure_datetime + 1)]
  DatabaseConnector::insertTable(connection = con,
                                 databaseSchema = 'synpuf',
                                 tableName = "PROCEDURE_OCCURRENCE",
                                 data = proc_occ,
                                 createTable = TRUE)
  # dbWriteTable(conn = con,
  #              name = "PROCEDURE_OCCURRENCE",
  #              value = proc_occ)

  dev_exp <- data.table::fread(input = 'inst/extdata/device_exposure.csv',
                               sep = '\t',
                               header = FALSE)
  names(dev_exp) <- c('device_exposure_id',
                      'person_id',
                      'device_concept_id',
                      'device_exposure_start_date',
                      'device_exposure_start_datetime',
                      'device_exposure_end_date',
                      'device_exposure_end_datetime',
                      'device_type_concept_id',
                      'unique_device_id',
                      'quantity',
                      'provider_id',
                      'visit_occurrence_id',
                      'device_source_value',
                      'device_source_concept_id')
  dev_exp[, `:=`(device_exposure_start_date = as.Date(device_exposure_start_date),
                 device_exposure_start_datetime = device_exposure_start_datetime + 1,
                 device_exposure_end_date = as.Date(device_exposure_end_date),
                 device_exposure_end_datetime = device_exposure_end_datetime + 1)]
  DatabaseConnector::insertTable(connection = con,
                                 databaseSchema = 'synpuf',
                                 tableName = "DEVICE_EXPOSURE",
                                 data = dev_exp,
                                 createTable = TRUE)
  # dbWriteTable(conn = con,
  #              name = "DEVICE_EXPOSURE",
  #              value = dev_exp)

  meas <- data.table::fread(input = 'inst/extdata/measurement.csv',
                            sep = '\t',
                            header = FALSE)
  names(meas) <- c('measurement_id',
                   'person_id',
                   'measurement_concept_id',
                   'measurement_date',
                   'measurement_datetime',
                   'measurement_type_concept_id',
                   'operator_concept_id',
                   'value_as_number',
                   'value_as_concept_id',
                   'unit_concept_id',
                   'range_low',
                   'range_high',
                   'provider_id',
                   'visit_occurrence_id',
                   'measurement_source_value',
                   'measurement_source_concept_id',
                   'unit_source_value',
                   'value_source_value')
  meas[, `:=`(measurement_date = as.Date(measurement_date),
              measurement_datetime = measurement_datetime + 1)]
  DatabaseConnector::insertTable(connection = con,
                                 databaseSchema = 'synpuf',
                                 tableName = "MEASUREMENT",
                                 data = meas,
                                 createTable = TRUE)
  # dbWriteTable(conn = con,
  #              name = "MEASUREMENT",
  #              value = meas)

  obs <- data.table::fread(input = 'inst/extdata/observation.csv',
                           sep = '\t',
                           header = FALSE)
  names(obs) <- c('observation_id',
                  'person_id',
                  'observation_concept_id',
                  'observation_date',
                  'observation_datetime',
                  'observation_type_concept_id',
                  'value_as_number',
                  'value_as_string',
                  'value_as_concept_id',
                  'qualifier_concept_id',
                  'unit_concept_id',
                  'provider_id',
                  'visit_occurrence_id',
                  'observation_source_value',
                  'observation_source_concept_id',
                  'unit_source_value',
                  'qualifier_source_value')
  obs[, `:=`(observation_date = as.Date(observation_date),
             observation_datetime = observation_datetime + 1)]
  DatabaseConnector::insertTable(connection = con,
                                 databaseSchema = 'synpuf',
                                 tableName = "OBSERVATION",
                                 data = obs,
                                 createTable = TRUE)
  # dbWriteTable(conn = con,
  #              name = "OBSERVATION",
  #              value = obs)

  death <- data.table::fread(input = 'inst/extdata/death.csv',
                             sep = '\t',
                             header = FALSE)
  names(death) <- c('person_id',
                    'death_date',
                    'death_datetime',
                    'death_type_concept_id',
                    'cause_concept_id',
                    'cause_source_value',
                    'cause_source_concept_id')
  death[, `:=`(death_date = as.Date(death_date),
               death_datetime = death_datetime + 1)]
  DatabaseConnector::insertTable(connection = con,
                                 databaseSchema = 'synpuf',
                                 tableName = "DEATH",
                                 data = death,
                                 createTable = TRUE)
  # dbWriteTable(conn = con,
  #              name = "DEATH",
  #              value = death)

  loca <- data.table::fread(input = 'inst/extdata/location.csv',
                            sep = '\t',
                            header = FALSE)
  names(loca) <- c('location_id',
                   'address_1',
                   'address_2',
                   'city',
                   'state_loca',
                   'zip',
                   'county',
                   'location_source_value')
  DatabaseConnector::insertTable(connection = con,
                                 databaseSchema = 'synpuf',
                                 tableName = "LOCATION",
                                 data = loca,
                                 createTable = TRUE)
  # dbWriteTable(conn = con,
  #              name = "LOCATION",
  #              value = loca)

  care <- data.table::fread(input = 'inst/extdata/care_site.csv',
                            sep = '\t',
                            header = FALSE)
  names(care) <- c('care_site_id',
                   'care_site_name',
                   'place_of_service_concept_id',
                   'location_id',
                   'care_site_source_value',
                   'place_of_service_source_value')
  DatabaseConnector::insertTable(connection = con,
                                 databaseSchema = 'synpuf',
                                 tableName = "CARE_SITE",
                                 data = care,
                                 createTable = TRUE)
  # dbWriteTable(conn = con,
  #              name = "CARE_SITE",
  #              value = care)

  prov <- data.table::fread(input = 'inst/extdata/provider.csv',
                            sep = '\t',
                            header = FALSE)
  names(prov) <- c('provider_id',
                   'provider_name',
                   'npi',
                   'dea',
                   'specialty_concept_id',
                   'care_site_id',
                   'year_of_birth',
                   'gender_concept_id',
                   'provider_source_value',
                   'specialty_source_value',
                   'specialty_source_concept_id',
                   'gender_source_value',
                   'gender_source_concept_id')
  DatabaseConnector::insertTable(connection = con,
                                 databaseSchema = 'synpuf',
                                 tableName = "PROVIDER",
                                 data = prov,
                                 createTable = TRUE)
  # dbWriteTable(conn = con,
  #              name = "PROVIDER",
  #              value = prov)

  payer <- data.table::fread(input = 'inst/extdata/payer_plan_period.csv',
                             sep = '\t',
                             header = FALSE)
  names(payer) <- c('payer_plan_period_id',
                    'person_id',
                    'payer_plan_period_start_date',
                    'payer_plan_period_end_date',
                    'payer_source_value',
                    'plan_source_value',
                    'family_source_value')
  payer[, `:=`(payer_plan_period_start_date = payer_plan_period_start_date + 1,
               payer_plan_period_end_date = payer_plan_period_end_date + 1)]
  DatabaseConnector::insertTable(connection = con,
                                 databaseSchema = 'synpuf',
                                 tableName = "PAYER_PLAN_PERIOD",
                                 data = payer,
                                 createTable = TRUE)
  # dbWriteTable(conn = con,
  #              name = "PAYER_PLAN_PERIOD",
  #              value = payer)

  cost <- data.table::fread(input = 'inst/extdata/cost.csv',
                            sep = '\t',
                            header = FALSE)
  names(cost) <- c('cost_id',
                   'cost_event_id',
                   'cost_domain_id',
                   'cost_type_concept_id',
                   'currency_concept_id',
                   'total_charge',
                   'total_cost',
                   'total_paid',
                   'paid_by_payer',
                   'paid_by_patient',
                   'paid_patient_copay',
                   'paid_patient_coinsurance',
                   'paid_patient_deductible',
                   'paid_by_primary',
                   'paid_ingredient_cost',
                   'paid_dispensing_fee',
                   'payer_plan_period_id',
                   'amount_allowed',
                   'revenue_code_concept_id',
                   'revenue_code_source_value',
                   'drg_concept_id',
                   'drg_source_value')
  DatabaseConnector::insertTable(connection = con,
                                 databaseSchema = 'synpuf',
                                 tableName = "COST",
                                 data = cost,
                                 createTable = TRUE)
  # dbWriteTable(conn = con,
  #              name = "COST",
  #              value = cost)

  drug_era <- data.table::fread(input = 'inst/extdata/drug_era.csv',
                                sep = '\t',
                                header = FALSE)
  names(drug_era) <- c('drug_era_id',
                       'person_id',
                       'drug_concept_id',
                       'drug_era_start_date',
                       'drug_era_end_date',
                       'drug_exposure_count',
                       'gap_days')
  drug_era[, `:=`(drug_era_start_date = as.Date(drug_era_start_date),
                  drug_era_end_date = as.Date(drug_era_end_date))]
  DatabaseConnector::insertTable(connection = con,
                                 databaseSchema = 'synpuf',
                                 tableName = "DRUG_ERA",
                                 data = drug_era,
                                 createTable = TRUE)
  # dbWriteTable(conn = con,
  #              name = "DRUG_ERA",
  #              value = drug_era)

  cond_era <- data.table::fread(input = 'inst/extdata/condition_era.csv',
                                sep = '\t',
                                header = FALSE)
  names(cond_era) <- c('condition_era_id',
                       'person_id',
                       'condition_concept_id',
                       'condition_era_start_date',
                       'condition_era_end_date',
                       'condition_occurrence_count')
  cond_era[, `:=`(condition_era_start_date = as.Date(condition_era_start_date),
                  condition_era_end_date = as.Date(condition_era_end_date))]
  DatabaseConnector::insertTable(connection = con,
                                 databaseSchema = 'synpuf',
                                 tableName = "CONDITION_ERA",
                                 data = cond_era,
                                 createTable = TRUE)
  # dbWriteTable(conn = con,
  #              name = "CONDITION_ERA",
  #              value = cond_era)

  cdm_source <- data.table::fread(input = 'inst/extdata/cdm_source.csv',
                                  sep = '\t',
                                  header = FALSE)
  names(cdm_source) <- c('cdm_source_name',
                         'cdm_source_abbreviation',
                         'cdm_holder',
                         'source_description',
                         'source_documentation_reference',
                         'cdm_etl_reference',
                         'source_release_date',
                         'cdm_release_date',
                         'cdm_version',
                         'vocabulary_version')
  cdm_source[, `:=`(source_release_date = as.Date(source_release_date),
                    cdm_release_date = as.Date(cdm_release_date))]
  DatabaseConnector::insertTable(connection = con,
                                 databaseSchema = 'synpuf',
                                 tableName = "CDM_SOURCE",
                                 data = cdm_source,
                                 createTable = TRUE)
  # dbWriteTable(conn = con,
  #              name = "CDM_SOURCE",
  #              value = cdm_source)

  concept <- data.table::fread(input = 'inst/extdata/concept.csv',
                               sep = '\t',
                               header = FALSE)
  names(concept) <- c('concept_id',
                      'concept_name',
                      'domain_id',
                      'vocabulary_id',
                      'concept_class_id',
                      'standard_concept',
                      'concept_code',
                      'valid_start_date',
                      'valid_end_date',
                      'invalid_reason')
  concept[, `:=`(valid_start_date = as.Date(valid_start_date),
                 valid_end_date = as.Date(valid_end_date))]
  DatabaseConnector::insertTable(connection = con,
                                 databaseSchema = 'synpuf',
                                 tableName = "CONCEPT",
                                 data = concept,
                                 createTable = TRUE)
  # dbWriteTable(conn = con,
  #              name = "CONCEPT",
  #              value = concept)

  vocabulary <- data.table::fread(input = 'inst/extdata/vocabulary.csv',
                                  sep = '\t',
                                  header = FALSE)
  names(vocabulary) <- c('vocabulary_id',
                         'vocabulary_name',
                         'vocabulary_reference',
                         'vocabulary_version',
                         'vocabulary_concept_id')
  DatabaseConnector::insertTable(connection = con,
                                 databaseSchema = 'synpuf',
                                 tableName = "VOCABULARY",
                                 data = vocabulary,
                                 createTable = TRUE)
  # dbWriteTable(conn = con,
  #              name = "VOCABULARY",
  #              value = vocabulary)

  domain <- data.table::fread(input = 'inst/extdata/domain.csv',
                              sep = '\t',
                              header = FALSE)
  names(domain) <- c('domain_id',
                     'domain_name',
                     'domain_concept_id')
  DatabaseConnector::insertTable(connection = con,
                                 databaseSchema = 'synpuf',
                                 tableName = "DOMAIN_meu",
                                 data = domain,
                                 createTable = TRUE)
  # dbWriteTable(conn = con,
  #              name = "DOMAIN",
  #              value = domain)

  con_class <- data.table::fread(input = 'inst/extdata/concept_class.csv',
                                 sep = '\t',
                                 header = FALSE)
  names(con_class) <- c('concept_class_id',
                        'concept_class_name',
                        'concept_class_concept_id')
  DatabaseConnector::insertTable(connection = con,
                                 databaseSchema = 'synpuf',
                                 tableName = "CONCEPT_CLASS",
                                 data = con_class,
                                 createTable = TRUE)
  # dbWriteTable(conn = con,
  #              name = "CONCEPT_CLASS",
  #              value = con_class)

  con_rela <- data.table::fread(input = 'inst/extdata/concept_relationship.csv',
                                sep = '\t',
                                header = FALSE)
  names(con_rela) <- c('concept_id_1',
                       'concept_id_2',
                       'relationship_id',
                       'valid_start_date',
                       'valid_end_date',
                       'invalid_reason')
  con_rela[, `:=`(valid_start_date = as.Date(valid_start_date),
                  valid_end_date = as.Date(valid_end_date))]
  DatabaseConnector::insertTable(connection = con,
                                 databaseSchema = 'synpuf',
                                 tableName = "CONCEPT_RELATIONSHIP",
                                 data = con_rela,
                                 createTable = TRUE)
  # dbWriteTable(conn = con,
  #              name = "CONCEPT_RELATIONSHIP",
  #              value = con_rela)

  relation <- data.table::fread(input = 'inst/extdata/relationship.csv',
                                sep = '\t',
                                header = FALSE)
  names(relation) <- c('relationship_id',
                       'relationship_name',
                       'is_hierarchical',
                       'defines_ancestry',
                       'reverse_relationship_id',
                       'relationship_concept_id')
  DatabaseConnector::insertTable(connection = con,
                                 databaseSchema = 'synpuf',
                                 tableName = "RELATIONSHIP",
                                 data = relation,
                                 createTable = TRUE)
  # dbWriteTable(conn = con,
  #              name = "RELATIONSHIP",
  #              value = relation)

  con_syn <- data.table::fread(input = 'inst/extdata/concept_synonym.csv',
                               sep = '\t',
                               header = FALSE)
  names(con_syn) <- c('concept_id',
                      'concept_synonym_name',
                      'language_concept_id')
  DatabaseConnector::insertTable(connection = con,
                                 databaseSchema = 'synpuf',
                                 tableName = "CONCEPT_SYNONYM",
                                 data = con_syn,
                                 createTable = TRUE)
  # dbWriteTable(conn = con,
  #              name = "CONCEPT_SYNONYM",
  #              value = con_syn)

  con_anc <- data.table::fread(input = 'inst/extdata/concept_ancestor.csv',
                               sep = '\t',
                               header = FALSE)
  names(con_anc) <- c('ancestor_concept_id',
                      'descendant_concept_id',
                      'min_levels_of_separation',
                      'max_levels_of_separation')
  DatabaseConnector::insertTable(connection = con,
                                 databaseSchema = 'synpuf',
                                 tableName = "CONCEPT_ANCESTOR",
                                 data = con_anc,
                                 createTable = TRUE)
  # dbWriteTable(conn = con,
  #              name = "CONCEPT_ANCESTOR",
  #              value = con_anc)

  drug_str <- data.table::fread(input = 'inst/extdata/drug_strength.csv',
                                sep = '\t',
                                header = FALSE)
  names(drug_str) <- c('drug_concept_id',
                       'ingredient_concept_id',
                       'amount_value',
                       'amount_unit_concept_id',
                       'numerator_value',
                       'numerator_unit_concept_id',
                       'denominator_value',
                       'denominator_unit_concept_id',
                       'box_size',
                       'valid_start_date',
                       'valid_end_date',
                       'invalid_reason')
  DatabaseConnector::insertTable(connection = con,
                                 databaseSchema = 'synpuf',
                                 tableName = "DRUG_STRENGTH",
                                 data = drug_str,
                                 createTable = TRUE)
  # dbWriteTable(conn = con,
  #              name = "DRUG_STRENGTH",
  #              value = drug_str)
  return(con)
}

# synpuf <- crea_synpuf()
# save(synpuf,
#      file = 'data/synpuf5pct.RData')
