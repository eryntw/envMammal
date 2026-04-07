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
                          
                          # Migratory ----
                          bb_Mig,
                          
                          # Territoriality ----
                          bhv_Territoriality,
                          
                          # Adult body size ----
                          bb_AverageMass,
                          
                          # Maneuverability ----
                          avo_HandWingIndex,
                          
                          # Activity ----
                          elt_Nocturnal,
                          
                          # Diet ----
                          bb_NeWt, 
                          bb_Db)
  ),
  
  ## Add invasive herbivores | predators as threat data ------
  
  threat_inv = summarise_ias_pressure(scored_threat, agg_method = "max"),
  
  ## Add increasing fire as threat data ------
  
  threat_fire = summarise_threat_L1(df = scored_threat, # 7_1_2 absent in bird subset
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
  
  ## Detect missing values ------
  exposure_missing = find_missing_values(exposure0),
  
  
  ## Creates/updates the manual table file ------
  tar_target(
    exposure_mtable_file,
    update_manual_table(
      exposure_missing,
      "data/current_mtable.csv"
    ),
    format = "file"
  ),
  
  ## Read the curated table ------
  tarchetypes::tar_file_read(name = processed_mtable,
                             command = exposure_mtable_file,
                             read = readr::read_csv(!!.x)
                             
  ),
  
  ## Join manual values back into dataset ------
  tar_target(name = exposure_imputed,
             command = map_traits(A = processed_mtable, 
                                  B = exposure,
                                  x = "search_term", 
                                  Atype = "long")
  ),
  
  ## Map exposure with traits ------
  
  mapped_exposure = map_exposure_cols(exposure_imputed) %>% 
    dplyr::select(search_term, common, starts_with("exp_")),
  
  
  ## Read stressor matrix ------
  
  exp_cols = c("HabitatLoss",
               "iasCat",
               "iasRabbitGoat",
               "Fragmentation",
               "IncreasedFire",
               "CollisionSolar",
               "CollisionWind"),
  
  tarchetypes::tar_file_read(name = stressor_matrix,
                             command = fs::path(here::here(), "..", "TraitsMeta",
                                                "ExposureTraits.xlsx"),
                             read = readxl::read_excel(path = !!.x, 
                                                       sheet = "StressorTraitMapping",
                                                       col_types = "guess") %>%
                               rescale_exposure_matrix(threat_cols = exp_cols) %>% 
                               dplyr::mutate(
                                 Category = to_upper_camel(Category)
                               )
  ),
  
  ## Combine mapped exposure and stressor matrix ------
  
  exposure_indicator = calculate_category_exposure(mapped_exposure,
                                                   stressor_matrix,
                                                   exp_cols) %>% 
    dplyr::select(where(~ dplyr::n_distinct(.x, na.rm = FALSE) > 1)), # remove useless cols
  
  exposure_matrix = calculate_exposure_matrix(mapped_exposure,
                                              stressor_matrix,
                                              exp_cols) %>% 
    dplyr::left_join(mapped_exposure[,c("search_term", "common")],
                     by = "search_term") # check mapped_exposure to validate
  
)
