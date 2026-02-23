library(targets)
library(tarchetypes)
library(crew)
library(crew.cluster)

use_cores <- parallel::detectCores() - 2
tar_option_set(packages = yaml::read_yaml("settings/packages.yaml")$packages
               , controller = crew_controller_local(workers = use_cores),
               workspace_on_error = TRUE)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# tar source -------
tar_source()

# targets -------

info_table <- tar_read(info_table, store = tars$bird_mtable$store)
joined_table <- tar_read(joined_table, store = tars$bird_db$store)

tar_plan(
  
  ## Read manually processed mtable -------
  
  tar_file_read(name = processed_mtable,
                command = "data/current_mtable.csv",
                read = readr::read_csv(!!.x, col_types = readr::cols())
  ),
  
  ## Scaling range, generation length and elevation using all AU birds ------
  
  tar_target(name = scaled_infotable,
             command = map_traits(A = processed_mtable, 
                                  B = joined_table %>% 
                                    dplyr::select(dplyr::any_of(names(info_table))),
                                  x = "search_term", 
                                  Atype = "long") %>% 
               dplyr::mutate(
                 bl_eoo_log10 = log10(bl_RlEooSmallerOfBreedingAndNonBreedingEoo),
                 bl_genlen_log = log(bl_GenerationLength),
                 bl_log10ElevScaled = log10(`rec_dem-9s_range_90_10_norm`),
               ) %>% 
               dplyr::inner_join(info_table %>% dplyr::select("search_term"),
                                 by = "search_term")
             
  ),
  
  ## Final score table ------
  
  # scored_table_v1 = birdsens_v1_original(scaled_infotable,
  #                                        outpath = tars$bird_score$store,
  #                                        return = "scored"),
  # 
  # scored_table_v2 = birdsens_v2_poolall(scaled_infotable,
  #                                       outpath = tars$bird_score$store,
  #                                       return = "scored"),
  # 
  # scored_table_v3 = birdsens_v3_mixed(scaled_infotable,
  #                                     outpath = tars$bird_score$store,
  #                                     return = "scored")
)





