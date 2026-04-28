library(targets)
library(tarchetypes)
library(crew)

use_cores <- parallel::detectCores() - 2

tar_option_set(
  packages = yaml::read_yaml("settings/packages.yaml")$packages, 
  controller = crew_controller_local(workers = use_cores),
  envir = 
)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# tar source -------
tar_source()

# targets -------

sensitivity_imputed <- tar_read(sensitivity_imputed, store = tars$sensitivity$store)
threats <- tar_read(threats, store = tars$threat$store)
exposure_indicator <- tar_read(exposure_indicator, store = tars$exposure$store)
exposurethreat_prefix <- tar_read(exp_cols, store = tars$exposure$store)
syn_db <- tar_read(syn_db, store = tars$database$store)

tar_plan(
  
  ## Join tables -------
  
  tar_target(name = info_table,
             command = sensitivity_imputed %>% 
               dplyr::left_join(exposure_indicator, by = "search_term") %>% 
               clean_taxa_df(taxa = search_term, commoncol = common) %>% 
               join_database_(threats %>%
                                clean_taxa_df(commoncol = common,
                                              taxa = scientific_name), 
                              prefix = "t_", 
                              syn_db = syn_db) %>%
               dplyr::select(-any_of(c("t_poptrend_description", "t_scientific_name", "t_code")))
  ),
  
  ## Build iData -------
  tar_target(name = iData,
             command = info_table %>% 
               dplyr::select(-Genus, -Species) %>% 
               dplyr::rename(uName = common,
                             uCode = search_term) %>%
               dplyr::mutate(uCode = gsub(" ", "_", uCode))  
  ),
  
  ## Build iMeta -------
  sensitivity_iMeta = build_iMeta(iData,
                                  build_sensitivity_groups(iData),
                                  index_name = "Sensitivity",
                                  negative_patterns = c(
                                    "ElevationalRange|RlEoo|rec_",
                                    "anthro_",
                                    "Db|DB|HB|Hb")),
  
  pressure_iMeta = build_iMeta(iData,
                               build_threat_groups(iData),
                               index_name = "Pressure"),
  
  exposure_iMeta = build_iMeta(iData,
                               build_exposure_groups(iData, expcols = exposurethreat_prefix),
                               index_name = "Exposure"),
  
  iMeta = merge_iMeta(iMeta_list = list(sensitivity_iMeta, pressure_iMeta, exposure_iMeta),
                      parent_name = "Vulnerability") %>% assign_iNames(),
  
  ## Build COIN ------
  coin = COINr::new_coin(iData = iData,
                         iMeta = iMeta,
                         level_names = c("Variable", "Indicator", "Sub-index", "Index")),
  
  coinA = COINr::new_coin(iData = iData %>% dplyr::select(-dplyr::any_of(exposure_iMeta$iCode)),
                          iMeta = merge_iMeta(iMeta_list = list(sensitivity_iMeta, pressure_iMeta),
                                              parent_name = "Vulnerability") %>% assign_iNames(),
                          level_names = c("Variable", "Indicator", "Sub-index", "Index"))
)
