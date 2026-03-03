library(targets)
library(tarchetypes)
library(crew)
library(crew.cluster)

use_cores <- parallel::detectCores() - 2

tar_option_set(
  packages = yaml::read_yaml("settings/packages.yaml")$packages, 
  controller = crew_controller_local(workers = use_cores),
  workspace_on_error = TRUE # inspect the error using tar_traceback(target)
)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# tar source -------
tar_source()

# targets -------

pilot_subset <- tar_read(pilot_subset, store = tars$database$store)

tar_plan(
  
  ## select cols for sensitivity scoring -------
  tar_target(
    name = sensitivity,
    command = 
      pilot_subset %>%
      dplyr::select(
        
        ## Names ------
        search_term, common, # Names
        
        ## Climate ------
        bb_ElevationalRange,
        bl_RlEooSmallerOfBreedingAndNonBreedingEoo,
        rec_stern_dehoedt_2000_minor_simpson,
        rec_geom_90M_s10e110_simpson, 
        contains("range_90_10"),
        contains("q10"),
        contains("q90"),
        
        ## Habitat ------
        aub_FeedingHB,
        aub_BreedingHB,
        bb_Hb,
        bl_HB_L1,
        bl_score_HB_L2,
        
        ## Diet ------
        bb_Db,
        bb_Db_simpson,
        
        ## Adaptability ------
        aub_score_anthro_habitat,
        bl_prop_anthro_total,
        bl_prop_anthro_max,
        
        ## Constraints------
        # ID raptors
        bl_is_raptor, 
        
        # Migratory
        contains("obligate_migrant"),
        
        # Restricted Range
        bb_Rr,
        
        # Generation length
        bl_GenerationLength
      ) %>% 
      dplyr::mutate(dplyr::across(where(is.numeric),~ round(.x, 2)))
  ),
  
  ## Append NA to the manually imputed table -------
  
  tar_target(
    name = impute_sensitivity,
    command = make_manual_table(sensitivity, dir = "data")
  ),
  
  ## Read manually processed mtable -------
  
  tar_file_read(name = processed_mtable,
                command = "data/current_mtable.csv",
                read = readr::read_csv(!!.x, col_types = readr::cols())
  ),
  
  ## impute -------
  
  tar_target(name = sensitivity_imputed,
             command = map_traits(A = processed_mtable, 
                                  B = sensitivity,
                                  x = "search_term", 
                                  Atype = "long")
  ),
  
)