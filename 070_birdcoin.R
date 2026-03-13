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
coinA <- tar_read(coinA, store = tars$coin$store)
exposure_indicator <- tar_read(exposure_indicator, store = tars$exposure$store)

tar_plan(
  
  ## Select indicators ------
  
  #### Sensitivity ####
  sensitivity_ind = c(
    
    # Climate ----
    "bl_RlEooSmallerOfBreedingAndNonBreedingEoo",
    "rec_stern_dehoedt_2000_minor_simpson",
    "rec_geom_90M_s10e110_simpson",
    
    # Habitat ----
    "bb_Hb",
    "aub_FeedingHB",
    
    # Adaptability / Anthropogenic exposure ----
    "aub_score_anthro_habitat",
    
    # Diet ----
    "bb_Db_simpson",
    
    # Constraints / Life history ----
    "bb_obligate_migrant",
    "bb_Rr",
    "bl_GenerationLength",
    "bl_is_raptor"
    ),
  
  #### Pressure ####
  pressure_ind = c(
    
    # Trend ----
    "t_trend_score",
    
    # Status ----
    "t_status_score",
    
    # Threats ----
    "t_score_sum_sp",
    "t_score_max_sp",
    "t_n_lv1",
    "t_n_lv2_total"
    ),
  
  #### Exposure ####
  exposure_ind = names(exposure_indicator[-1]),

  
  ## Individual Treatments -------
  treat_indiv_specs = list(
    bl_RlEooSmallerOfBreedingAndNonBreedingEoo = list(f1 = "log10"), # standard base-10 log
    bl_GenerationLength = list(f1 = "log"), # Uses natural log
    t_score_sum_sp = list(f1 = "log")
  ),
  
  ## COIN_1 baseline ------
  
  coin_1 = COINr::change_ind(coin, 
                             drop = setdiff(coin$Meta$Ind$iCode[coin$Meta$Ind$Level == 1],
                                            c(sensitivity_ind, pressure_ind, exposure_ind)), 
                             regen = TRUE) %>% 
    COINr::Treat(dset = "Raw", 
                 indiv_specs = treat_indiv_specs,
                 global_specs = "none") %>% 
    COINr::Normalise(dset = "Treated") %>%
    COINr::Aggregate(dset = "Normalised", f_ag = "a_amean"),
  
  ## COIN1_1 exclude exposure ------

  coinA_1 = COINr::change_ind(coinA,
                             drop = setdiff(coinA$Meta$Ind$iCode[coinA$Meta$Ind$Level == 1],
                                            c(sensitivity_ind, pressure_ind)),
                             regen = TRUE) %>%
    COINr::Treat(dset = "Raw",
                 indiv_specs = treat_indiv_specs,
                 global_specs = "none") %>%
    COINr::Normalise(dset = "Treated") %>%
    COINr::Aggregate(dset = "Normalised", f_ag = "a_amean")
  
)
