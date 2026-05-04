library(targets)
library(tarchetypes)
library(crew)

tar_option_set(
  packages = yaml::read_yaml("settings/packages.yaml")$packages, 
  controller = crew_controller_local(workers = 20)
)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# tar source -------
tar_source()

# targets -------

pilot_subset <- tar_read(pilot_subset, store = tars$database$store)

tar_plan(
  
  ## API ------
  api = Sys.getenv("IUCN_REDLIST_KEY"),
  
  ## Get IUCN data for pilot_subset -------
  tar_target(name = iucn_data,
             command = get_iucn_species_data(pilot_subset, api = iucnredlist::init_api(api))
  ),
  
  ## Extract threats ------
  targets::tar_target(name = iucn_threat,
                      command = iucn_data$iucn_data %>% 
                        dplyr::select(scientific_name, common, code, starts_with("threat_")) %>% 
                        dplyr::distinct()
  ),
  
  ## Score threats ------
  scored_threat = score_threat(iucn_threat, score_system = "ward_modified"),
  
  
  # Summarise threats ------
  threatsum = summarise_iucn_threat(scored_threat),
  
  # Score trend and status ------
  targets::tar_target(name = scored_trendstatus,
                      command = iucn_data$iucn_data %>% 
                        dplyr::select(scientific_name, common, code, poptrend_description) %>% 
                        dplyr::distinct() %>% 
                        score_trend_status(trend_col = "poptrend_description",
                                           status_col = "code")
  ),
  
  # Join tables -----
  targets::tar_target(name = threats,
                      command = scored_trendstatus %>%
                        dplyr::left_join(threatsum$species_summary, 
                                         by=c("scientific_name","common")) %>% 
                        dplyr::mutate(dplyr::across(where(is.numeric),~ round(.x, 2)))
  )
)