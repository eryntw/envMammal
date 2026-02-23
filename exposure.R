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

pilot_subset <- tar_read(pilot_subset, store = tars$bird_db$store)

tar_plan(
  
  ## select cols for sensitivity scoring -------
  tar_target(
    name = expo_df,
    command = 
      pilot_subset %>%
      dplyr::select(
        
        ## Names ------
        search_term, common, # Names
        
        ## Nesting substrate ------
        bb_NestSbs,
        
        ## Habitat ------
        contains("aub_BreedingHabitat"),
        contains("aub_FeedingHabitat"),
        
        ## Foraging strategy ------
        bb_Db,
        bb_db_simpson,
        
        ## Adaptability ------
        aub_score_anthro_habitat,
        bl_prop_anthro_total,
        bl_prop_anthro_max,
        
        ## Constraints------
        # ID raptors
        bl_is_raptor, 
        
        # Migratory
        aub_obligate_migrant,
        bb_obligate_migrant,
        bl_obligate_migrant,
        
        # Restricted Range
        bb_Rr,
        
        # Generation length
        bl_GenerationLength
      )
  )
)