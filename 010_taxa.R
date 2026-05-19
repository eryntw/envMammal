library(targets)
library(tarchetypes)

targets::tar_option_set(packages = yaml::read_yaml("settings/packages.yaml")$packages, 
                        controller = crew::crew_controller_local(workers = 20))


# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# tar source -------
tar_source("../envBird/R")

# targets -------

sa_animals <- tar_read(sa_animals, store = tars$envBird$taxa$store)
usg <- tar_read(usg, store = tars$envBird$taxa$store)
bp <- tar_read(bp, store = tars$envBird$taxa$store)

tar_plan(
  
  ## Input species list options -------
  
  # All SA mammals
  tar_target(name = sa_mammals,
             command = {
               # Galah config
               potions::brew(.pkg = "galah")
               galah::galah_config(
                 atlas = Sys.getenv("ALA_ATLAS"),
                 email = Sys.getenv("ALA_EMAIL"))
               
               galah::search_taxa(sa_animals$taxa) %>% 
                 dplyr::distinct() %>% 
                 dplyr::filter(class == "Mammalia") %>%
                 replace_taxa(taxa_col = "search_term") %>% 
                 clean_taxa_df(commoncol = vernacular_name,
                               taxacol = search_term) %>%
                 dplyr::select(search_term, common, Genus, Species)
               
             }
  ),
  
  ## Target species list ------
  
  tar_target(name = splist,
             command = {
               # Galah config
               potions::brew(.pkg = "galah")
               galah::galah_config(
                 atlas = Sys.getenv("ALA_ATLAS"),
                 email = Sys.getenv("ALA_EMAIL"))
               
               usg %>% 
                 dplyr::bind_rows(bp) %>% 
                 organise_piaout() %>% 
                 .$Mammalia %>% 
                 replace_taxa(taxa_col = "search_term") %>% 
                 clean_taxa_df(taxacol = search_term, 
                               commoncol = ala_vernacular_name) %>% 
                 add_common() %>% 
                 dplyr::bind_rows(sa_mammals %>% 
                                    filter(search_term == "Rattus rattus")) %>% # Base species
                 tibble::add_row(search_term = "Oryctolagus cuniculus")
             }
  )
)
