library(targets)
library(tarchetypes)

targets::tar_option_set(packages = yaml::read_yaml("settings/packages.yaml")$packages, 
                        controller = crew::crew_controller_local(workers = 20))


# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# tar source -------
tar_source()

# targets -------
# Galah config

tar_plan(
  
  ## Input species list options -------
  
  # All SA animals
  tarchetypes::tar_file_read(name = sa_animals,
                             command = fs::path(tars$envCleaned$clean$store, "objects", "bio_clean"),
                             read = arrow::open_dataset(!!.x) %>%
                               dplyr::filter(grepl(" ", taxa), kingdom == "Animalia") %>%
                               dplyr::select(taxa, common) %>%
                               dplyr::distinct() %>%
                               dplyr::collect()
  ),
  
  # All SA birds
  tar_target(name = sa_birds,
             command = {
               # Galah config
               potions::brew(.pkg = "galah")
               galah::galah_config(
                 atlas = Sys.getenv("ALA_ATLAS"),
                 email = Sys.getenv("ALA_EMAIL"))
               
               galah::search_taxa(sa_animals$taxa) %>% 
                 dplyr::distinct() %>% 
                 dplyr::filter(class == "Aves") %>%
                 replace_taxa(taxa_col = "search_term") %>% 
                 clean_taxa_df(commoncol = vernacular_name,
                               taxacol = search_term) %>%
                 dplyr::select(search_term, common, Genus, Species)
               
             }
  ),
  
  # USG species
  
  stores = fs::dir_ls(dirname(tars$envPIA$setup$store)
                      , regexp = "concern\\/objects\\/concern$"
                      , recurse = TRUE
  ),
  
  tarchetypes::tar_file_read(name = usg, 
                             command = stores[1], 
                             read = readRDS(!!.x)
  ),
  
  # BP species
  tarchetypes::tar_file_read(name = bp, 
                             command = stores[2], 
                             read = readRDS(!!.x)
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
                 .$Aves %>% 
                 replace_taxa(taxa_col = "search_term") %>% 
                 clean_taxa_df(taxacol = search_term, 
                               commoncol = ala_vernacular_name) %>% 
                 add_common() 
             }
  )
)
