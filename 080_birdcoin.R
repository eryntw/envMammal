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

coin <- tar_read(coin, store = tars$coin$store)

tar_plan(
  
  ## Select indicators ------
  indicator = c(
    
    #### Sensitivity ####
    
    # Climate ----
    "bl_RlEooSmallerOfBreedingAndNonBreedingEoo",
    "rec_stern_dehoedt_2000_minor_simpson",
    "rec_geom_90M_s10e110_simpson",
    
    # Habitat ----
    "aub_BreedingHB",
    "aub_FeedingHB",
    
    # Adaptability / Anthropogenic exposure ----
    "aub_score_anthro_habitat",
    
    # Diet ----
    "bb_Db_simpson",
    
    # Constraints / Life history ----
    "bb_obligate_migrant",
    "bb_Rr",
    "bl_GenerationLength",
    "bl_is_raptor",
    
    #### Pressure ####
    
    # Trend ----
    "t_trend_score",
    
    # Status ----
    "t_status_score",
    
    # Threats ----
    "t_score_sum_sp",
    
    #### Exposure ####
    
    # HabitatLoss ----
    grep("HabitatLoss_", names(coin$data$raw), value = TRUE),
    
    # Invasive_cat|rabbitgoat ----
    grep("Invasive", names(coin$data$raw), value = TRUE),
    
    # Fragmentation ----
    grep("Fragmentation_", names(coin$data$raw), value = TRUE),
    
    # IncreasedFire ----
    grep("IncreasedFire_", names(coin$data$raw), value = TRUE),
    
    # CollisionSolar|Wind----
    grep("Collision", names(coin$data$raw), value = TRUE)
    
  ),
  
  ## Individual Treatments -------
  treat_indiv_specs = list(
    bl_RlEooSmallerOfBreedingAndNonBreedingEoo = list(f1 = "log10"), # standard base-10 log
    bl_GenerationLength = list(f1 = "log") # Uses natural log
  ),
  
  ## COIN - subset 1 ------
  coin_sub1 = COINr::change_ind(coin, 
                                drop = setdiff(coin$Meta$Ind$iCode[coin$Meta$Ind$Level == 1],
                                               indicator), 
                                regen = TRUE) %>% 
    COINr::Treat(dset = "Raw", indiv_specs = treat_indiv_specs) %>% 
    COINr::Normalise(coin, dset = "Treated")
  
  
)
