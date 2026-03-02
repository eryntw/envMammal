library(targets)
library(tarchetypes)
library(crew)
library(crew.cluster)

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

sensitivity <- tar_read(sensitivity, store = tars$sensitivity$store)
threats <- tar_read(threats, store = tars$threat$store)
exposure <- tar_read(exposure, store = tars$exposure$store)
syn_db <- tar_read(syn_db, store = tars$database$store)

tar_plan(
  
  ## Join subindex groups -------
  tar_target(
    name = info_table,
    command = sensitivity %>% 
      clean_taxa_df(commoncol = common,
                    taxa = search_term) %>% 
      join_database_(threats %>%
                      clean_taxa_df(commoncol = common,
                                    taxa = scientific_name), 
                    prefix = "t_", 
                    syn_db = syn_db) %>% 
      dplyr::select(-any_of(c("t_poptrend_description", "t_scientific_name", "t_code")))
  ),
  
  ## Make table for manual filling -------
  tar_target(
    name = impute_table,
    command = make_manual_table(info_table, dir = "data")
  )
  
)
