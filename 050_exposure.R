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

pilot_subset <- tar_read(pilot_subset, store = tars$database$store)
syn_db <- tar_read(syn_db, store = tars$database$store)
scored_threat <- tar_read(scored_threat, store = tars$threat$store)

tar_plan(
  
  targets::tar_target(name = exposure0,
                      command = 
                        pilot_subset %>%
                        select(
                          
                          ## Names ------
                          search_term, common,
                          
                          # Arid zone vegetation ----
                          dplyr::matches("^aub_Breeding.*(Savanna|DryScler|Mallee|Shrub|Hummock|OtherGrass)"),
                          aub_BreedingHB,
                          
                          # Nesting substrate/structure ----
                          dplyr::starts_with("bbn_"),
                          aub_NonBreedingOnly4,
                          
                          # Foraging strategy ----
                          dplyr::matches("^elt_ForStrat(?!Source|SpecLevel|EnteredBy)", perl = TRUE),
                          elt_PelagicSpecialist,
                          
                          # Territoriality ----
                          bhv_Territoriality,
                          
                          # Adult body size ----
                          bb_AverageMass,
                          
                          # Maneuverability ----
                          avo_HandWingIndex_mean,
                          
                          # Activity ----
                          elt_Nocturnal,
                          
                          # Diet ----
                          bb_NeWt, 
                          bb_Db)
  ),
  
  ## Add Invasives as Threat data ------
  
  threat_inv = summarise_threat_L1(df = scored_threat,
                                   lv1_filter = 8,
                                   agg_method = "max"
  ),
  
  ## Add Fire as Threat data ------
  
  threat_fire = summarise_threat_L1(df = scored_threat,
                                    lv1_filter = 7,
                                    lv2_filter = 1,
                                    agg_method = "max"
  ),
  
  ## Join threats to exposure ------
  
  targets::tar_target(name = exposure,
                      command = exposure0 %>%
                        dplyr::left_join(threat_inv, by = c("search_term" = "scientific_name")) %>%
                        dplyr::left_join(threat_fire, by = c("search_term" = "scientific_name")) %>%
                        dplyr::mutate(dplyr::across(dplyr::starts_with("threat_"),
                                                    ~ tidyr::replace_na(., 0)))
  ),
  
  
  ## Append NA to the manually imputed table -------
  
  tar_target(
    name = impute_exposure,
    command = make_manual_table(exposure0, dir = "data")
  ),
  
  ## Read manually processed mtable -------
  
  tar_file_read(name = processed_mtable,
                command = "data/current_mtable.csv",
                read = readr::read_csv(!!.x, col_types = readr::cols())
  ),
  
  ## impute -------
  
  tar_target(name = exposure_imputed,
             command = map_traits(A = processed_mtable, 
                                  B = exposure,
                                  x = "search_term", 
                                  Atype = "long")
  ),
  
  ## organise exposure traits ------
)
