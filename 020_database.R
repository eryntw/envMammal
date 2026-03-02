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

splist <- tar_read(splist, store = tars$taxa$store)
sa_birds <- tar_read(sa_birds, store = tars$taxa$store)

tar_plan(
  
  ## Extracted environmental variables -------
  
  tarchetypes::tar_file_read(name = summary_df,
                             command = fs::path("../RecExtract/output/summary_df.parquet"),
                             read = arrow::open_dataset(!!.x) %>%
                               dplyr::collect() %>% 
                               replace_taxa(taxa_col = "taxa") %>% # ATTENTION: species split
                               dplyr::inner_join(sa_birds, by = c("taxa" = "search_term"))
  ),
  
  
  ## Static database -------
  
  ## BirdBase 2025 ------
  
  tarchetypes::tar_file_read(name = birdbase,
                             command = "database/data_BIRDBASE v2025.1 Sekercioglu et al. Final.csv",
                             read = readr::read_csv(file = !!.x, 
                                                    col_types = readr::cols(),
                                                    locale = readr::locale(encoding = "ASCII")) %>% 
                               janitor::clean_names(case = "upper_camel") %>% 
                               clean_taxa_df(commoncol = EnglishNameBirdLifeIocClementsAviList) %>% 
                               get_birdbase(subset = FALSE) # ATTENTION
  ),
  
  ## BirdBase Nest binary ------
  
  tarchetypes::tar_file_read(name = bb_nest,
                             command = "database/BIRDBASE v2025.1 Sekercioglu et al. Final.xlsx",
                             read = readxl::read_excel(path = !!.x, 
                                                       sheet = "Nest Details",
                                                       col_types = "guess") %>% 
                               janitor::clean_names(case = "upper_camel") %>% 
                               clean_taxa_df(commoncol = EnglishName,
                                             taxacol = LatinName) %>% 
                               dplyr::select(-Ioc15_1)
  ),
  
  ## Birdlife Generation Length 2025 ------
  
  tarchetypes::tar_file_read(name = genlength,
                             command = "database/latest_generation_lengths_of_the_world's_birds_2025.xlsx",
                             read = readxl::read_excel(path = !!.x, 
                                                       sheet = 1,
                                                       skip = 1,
                                                       col_types = "guess") %>%
                               janitor::clean_names(case = "upper_camel") %>% 
                               clean_taxa_df(commoncol = EnglishName2024,
                                             taxa = SpeciesName2024)
  ),
  
  ## Birdlife attributes 2026 ------
  
  tarchetypes::tar_file_read(name = birdlife_attr,
                             command = "database/BirdLife data on Australian birds/Australian bird species attributes.xlsx",
                             read = readxl::read_excel(path = !!.x, 
                                                       sheet = 1,
                                                       col_types = "guess") %>%
                               janitor::clean_names(case = "upper_camel") %>% 
                               clean_taxa_df(commoncol = CommonName,
                                             taxa = ScientificName) %>% 
                               get_birdlife_attr()
  ),
  
  ## Birdlife habitats 2026 ------
  
  tarchetypes::tar_file_read(name = birdlife_hab,
                             command = "database/BirdLife data on Australian birds/Habitats.xlsx",
                             read = readxl::read_excel(path = !!.x, 
                                                       sheet = 1,
                                                       col_types = "guess") %>%
                               janitor::clean_names(case = "upper_camel") %>% 
                               score_iucn_habitat() %>% 
                               clean_taxa_df(commoncol = CommonName,
                                             taxa = ScientificName)
  ),
  
  ## Australian Birds 2015 ------
  
  tar_target(name = ausbird,
             command = traitdata::australian_birds %>%
               setNames(gsub("^X\\d+_", "", names(.))) %>%
               dplyr::filter(Extinct_4 == 0) %>% 
               janitor::clean_names(case = "upper_camel") %>% 
               clean_taxa_df(commoncol = TaxonCommonName2) %>% 
               filter(is.na(SubspeciesName2)) %>%  # Most subspecies have no data
               get_ausbird(subset = FALSE)
  ),
  
  ## AVONET 2021 ------
  
  tar_target(name = avonet, 
             command = traitdata::avonet %>% 
               dplyr::filter(Age == 0) %>% 
               janitor::clean_names(case = "upper_camel") %>% 
               dplyr::group_by(Genus, Species) %>%
               dplyr::summarise(dplyr::across(dplyr::where(is.numeric),
                                              list(
                                                mean = ~ mean(.x, na.rm = TRUE),
                                                median = ~ median(.x, na.rm = TRUE)
                                              ),
                                              .names = "{.col}_{.fn}"
               ),
               .groups = "drop") %>% 
               dplyr::filter(!(base::is.na(Genus) & base::is.na(Species))) %>% 
               dplyr::mutate(dplyr::across(where(is.numeric),~ round(.x, 2)))
  ),
  
  ## Elton Birds 2014 ------
  
  tar_target(name = elt_birds, 
             command =  traitdata::elton_birds %>% 
               dplyr::distinct() %>% 
               janitor::clean_names(case = "upper_camel") %>% 
               clean_taxa_df(commoncol = English)
  ),
  
  ## Bird Behaviour 2019 ------
  
  tar_target(name = bird_behav, 
             command = traitdata::bird_behav %>% distinct()
  ),
  
  
  ## Match database species -------
  
  tar_file_read(name = syn_db, 
                command = fs::path("data/synonyms.csv"),
                read = readr::read_csv(!!.x, col_types = readr::cols())
  ),
  
  ## join database: sa_birds -------
  tar_target(name = joined_table,
             command = sa_birds %>%
               join_database_(summary_df, prefix = "rec_", syn_db = syn_db) %>%  
               join_database_(birdbase, prefix = "bb_", syn_db = syn_db) %>%
               join_database_(bb_nest, prefix = "bbn_", syn_db = syn_db) %>% 
               join_database_(genlength, prefix = "bl_", syn_db = syn_db) %>%
               join_database_(birdlife_attr %>% dplyr::select(-ScientificName), 
                              prefix = "bl_", syn_db = syn_db) %>%
               join_database_(birdlife_hab %>% dplyr::select(-ScientificName), 
                              prefix = "bl_", syn_db = syn_db) %>%
               join_database_(ausbird, prefix = "aub_", syn_db = syn_db) %>% 
               join_database_(bird_behav, prefix = "bhv_", syn_db = syn_db) %>% 
               join_database_(avonet, prefix = "avo_", syn_db = syn_db) %>% 
               join_database_(elt_birds, prefix = "elt_", syn_db = syn_db)

  ),
  
  ## join database: pilot areas -------
  pilot_subset = left_join(splist, 
                           joined_table %>% dplyr::select(-Genus, -Species, -common), 
                           by = "search_term")
  
)



