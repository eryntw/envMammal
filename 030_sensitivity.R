library(targets)
library(tarchetypes)
library(crew)

tar_option_set(
  packages = yaml::read_yaml("settings/packages.yaml")$packages, 
  controller = crew_controller_local(workers = 10),
  workspace_on_error = TRUE # inspect the error using tar_traceback(target)
)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# tar source -------
tar_source("../envBird/R")
tar_source()

# targets -------

pilot_subset <- tar_read(pilot_subset, store = tars$database$store)

tar_plan(
  
  ## Imputation from multiple dataset with rules ------
  pilot_subset_selfimpute = self_impute_mammals(pilot_subset),
  
  ## select cols for sensitivity scoring -------
  tar_target(
    name = sensitivity,
    command = 
      pilot_subset_selfimpute %>%
      dplyr::select(
        
        ## Names ------
        search_term, common, # Names
        
        ## Climate ------
        rec_stern_clim_minor_simpson,
        rec_geomorphon_simpson, 
        rec_bioclim_simpson,
        # contains("range_90_10"),
        # contains("q10"),
        # contains("q90"),
        
        ## Habitat ------
        rec_vegstr_simpson,
        iucn_HB_L1,
        hc_HB,
        
        ## Diet ------
        elt_simpson,
        
        ## Adaptability ------
        
        ## Constraints------
        # Trophic level
        pan_TrophicLevel,
        
        # Reproduction rate
        littersize_mean,
        litterperyear_mean,
        
        # Restricted Range
        pan_GR_Area_km2,
        
        # Generation length
        female_maturity_day_mean,
        male_maturity_day_mean,
        
        # Dependent period
        gestation_day_mean,
        weaning_day_mean,
        
        # Longevity
        maxlongevity_mean,
        
        # Body weight
        bodymass_mean
        
      ) %>% 
      dplyr::mutate(dplyr::across(where(is.numeric),~ round(.x, 2)))
  ),
  
  ## Detect missing values ------
  sensitivity_missing = find_missing_values(sensitivity),

  
  ## Creates/updates the manual table file ------
  tar_target(
    sensitivity_mtable_file,
    update_manual_table(
      sensitivity_missing,
      "data/current_mtable.csv"
    ),
    format = "file"
  ),
  
  ## Read the curated table ------
  tarchetypes::tar_file_read(name = processed_mtable,
                             command = sensitivity_mtable_file,
                             read = readr::read_csv(!!.x)
      
  ),
  
  ## Join manual values back into dataset ------
  tar_target(name = sensitivity_imputed,
             command = map_traits(A = processed_mtable, 
                                  B = sensitivity,
                                  x = "search_term", 
                                  Atype = "long")
  )
)