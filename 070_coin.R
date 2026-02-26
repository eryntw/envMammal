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

info_table <- tar_read(info_table, store = tars$impute$store)

tar_plan(
  
  ## Read manually processed mtable -------
  
  tar_file_read(name = processed_mtable,
                command = "data/current_mtable.csv",
                read = readr::read_csv(!!.x, col_types = readr::cols())
  ),
  
  ## Build iData -------
  tar_target(name = iData,
             command = map_traits(A = processed_mtable, 
                                  B = info_table,
                                  x = "search_term", 
                                  Atype = "long") %>% 
               rename(uName = search_term, 
                      uCode = common)
  ),
  
  ## Build iMeta -------
  sensitivity_iMeta = build_iMeta(iData, 
                                  build_sensitivity_groups(iData), 
                                  index_name = "Sensitivity"),
  pressure_iMeta = build_iMeta(iData, 
                             build_threat_groups(iData),
                             index_name = "Pressure"),
  
  bird_iMeta = merge_iMeta(iMeta_list = list(sensitivity_iMeta, pressure_iMeta),
                           parent_name = "Vulnerability")
)
