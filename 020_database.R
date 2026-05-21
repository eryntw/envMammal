library(targets)
library(tarchetypes)

targets::tar_option_set(packages = yaml::read_yaml("settings/packages.yaml")$packages, 
                        controller = crew::crew_controller_local(workers = 30))

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# tar source -------
tar_source("../envBird/R")

# targets -------

splist <- tar_read(splist, store = tars$taxa$store)
sa_mammals <- tar_read(sa_mammals, store = tars$taxa$store)

tar_plan(
  
  ## Extracted environmental variables -------
  
  tarchetypes::tar_file_read(name = summary_df,
                             command = fs::path("../RecExtract/output/summary_df.parquet"),
                             read = arrow::open_dataset(!!.x) %>%
                               dplyr::collect() %>% 
                               replace_taxa(taxa_col = "taxa") %>% # ATTENTION: species split
                               dplyr::inner_join(sa_mammals, by = c("taxa" = "search_term"))
  ),
  
  
  ## Static database -------
  
  ## Elton Mammals
  tar_target(name = elt_mammals, 
             command = {
               df <- traitdata::elton_mammals %>%
                 dplyr::distinct() %>%
                 janitor::clean_names(case = "upper_camel")
               
               diet_cols <- names(df) %>%
                 grep("^Diet", x = ., value = TRUE) %>%
                 setdiff(c("DietSource", "DietCertainty"))
               
               div <- calculate_diversity_index(df, diet_cols)
               
               dplyr::bind_cols(df,dplyr::select(div, simpson))
             }
             
  ),
  
  ## Pantheria
  tar_target(name = pantheria, 
             command =  traitdata::pantheria %>% 
               dplyr::distinct()
  ),
  
  ## Amniota
  tar_target(name = amniota, 
             command =  traitdata::amniota %>% 
               dplyr::distinct() %>% 
               clean_taxa_df(commoncol = common_name)
  ),
  
  ## IUCN
  api = Sys.getenv("IUCN_REDLIST_KEY"),
  tar_target(name = iucn_habitat,
             command = map_iucn_data(sa_mammals, 
                                     api = iucnredlist::init_api(api),
                                     query_fn = get_iucn_habitat) %>% 
               .$iucn_data %>% 
               dplyr::select(scientific_name, common, starts_with("habitat_")) %>% 
               dplyr::filter((habitat_season %in% c("Resident", "Non-Breeding Season") | 
                                is.na(habitat_season)) &
                               habitat_suitability == "Suitable") %>% 
               dplyr::mutate(habitat_code1 = stringr::str_extract(habitat_code, "^[0-9]+")) %>%
               dplyr::group_by(scientific_name) %>%
               dplyr::mutate(HB_L1 = n_distinct(habitat_code1)) %>% 
               dplyr::ungroup() %>% 
               dplyr::select(scientific_name, common, HB_L1) %>% 
               dplyr::distinct() %>% 
               clean_taxa_df(taxacol = scientific_name)
  ),
  
  ## AnAge
  tar_target(name = anage, 
             command =  hagr::age %>% 
               dplyr::distinct() %>% 
               janitor::clean_names(case = "upper_camel") %>% 
               clean_taxa_df(commoncol = CommonName)
  ),
  
  ## Habitat coding 
  tarchetypes::tar_file_read(name = habitatcoding,
                             command = fs::path("/mnt/envshare/data/traits/meta/HabitatCoding.xlsx") 
                             read = readxl::read_excel(!!.x) %>% 
                               tibble::column_to_rownames("ausbird_habitat") %>%  
                               t() %>%                                         
                               tibble::as_tibble(rownames = "search_term") %>%
                               dplyr::mutate(dplyr::across(where(\(x) is.character(x) && 
                                                                   all(x %in% c("0", "1"))),
                                                           as.integer)) %>%
                               dplyr::mutate(HB = rowSums(dplyr::pick(where(is.numeric)), na.rm = TRUE)) %>% 
                               dplyr::mutate(
                                 `Special requirement` = dplyr::if_else(
                                   `Special requirement` == "none", 0L, 1L),
                                 `Cryptic species` = dplyr::if_else(
                                   `Cryptic species` == "yes",  1L, 0L)) %>% 
                               clean_taxa_df(taxacol = search_term)
                             
  ),

  ## Match database species using synonym database -------
  tar_target(name = syn_db, 
             command = match_synonym(splist$search_term)
  ),
  
  ## join database: sa_mammals -------
  tar_target(name = joined_table,
             command = sa_mammals %>%
               join_database_(summary_df, prefix = "rec_", syn_db = syn_db) %>%  
               join_database_(elt_mammals, prefix = "elt_", syn_db = syn_db) %>%
               join_database_(pantheria, prefix = "pan_", syn_db = syn_db) %>% 
               join_database_(amniota, prefix = "amn_", syn_db = syn_db) %>% 
               join_database_(iucn_habitat, prefix = "iucn_", syn_db = syn_db) %>% 
               join_database_(anage, prefix = "age_", syn_db = syn_db) %>% 
               join_database_(habitatcoding, prefix = "hc_", syn_db = syn_db)

  ),
  
  ## join database: pilot areas -------
  pilot_subset = left_join(splist %>% select(-Species, -Genus),
                           joined_table %>% dplyr::select(-common),
                           by = "search_term")
  
)