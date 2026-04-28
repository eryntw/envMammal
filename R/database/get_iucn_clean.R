#' Get IUCN assessment data and combine elements
#'
#' @param api IUCN API object
#' @param genus Character, genus name
#' @param species Character, species name
#' @param elements Named list of elements to extract with optional prefixes
#' @return A list with combined dfs: `df_combined` and `syms_combined`
#' 

get_iucn_clean <- function(api, genus, species) {
  
  # ---- Query assessments ----
  assess <- tryCatch(
    {
      iucnredlist::assessments_by_name(api, genus, species)
    },
    error = function(e) {
      # Ignore errors and return empty df
      data.frame()
    }
  )
  
  # ---- If no assessment exists, return nothing (NULL) ----
  if (nrow(assess) == 0) {
    message("No IUCN assessment found for ", genus, " ", species)
    return(list(main = NULL, syms = NULL))
  }
  
  # ---- If assessment exists, continue ----
  assess_id <- assess$assessment_id[1]
  dat <- iucnredlist::assessment_data_many(api, assessment_ids = assess_id)
  
  # ---- Extract elements ----
  ## Single entry
  taxon <- iucnredlist::extract_element(dat, "taxon")[, c(2:4)]
  
  common <- iucnredlist::extract_element(dat, "taxon_common_names") %>%
    dplyr::filter(main == TRUE) %>%
    dplyr::rename(common = name)
  
  synonyms <- iucnredlist::extract_element(dat, "taxon_synonyms") %>%
    dplyr::select(any_of(c("assessment_id", "genus_name", "species_name"))) %>%
    dplyr::distinct()
  
  status <- iucnredlist::extract_element(dat, "red_list_category")[, c(2,5)]
  
  ## Multiple entry
  threats <- iucnredlist::extract_element(dat, "threats") %>%
    dplyr::rename_with(~ paste0("threat_", .x), -assessment_id)
  
  poptrend <- iucnredlist::extract_element(dat, "population_trend") %>%
    dplyr::rename_with(~ paste0("poptrend_", .x), -assessment_id)
  
  conservation <- iucnredlist::extract_element(dat, "conservation_actions_in_place") %>%
    dplyr::rename_with(~ paste0("conservation_", .x), -assessment_id)
  
  # ---- Combine data (main) ----
  dfs <- list(taxon, common, status, threats, poptrend, conservation)
  dfs_nonempty <- dfs[vapply(dfs, function(x) nrow(x) > 0, logical(1))]
  
  if (length(dfs_nonempty) == 0) {
    combined <- NULL
  } else if (length(dfs_nonempty) == 1) {
    combined <- dfs_nonempty[[1]]
  } else {
    combined <- purrr::reduce(dfs_nonempty, dplyr::left_join, by = "assessment_id")
  }
  
  # ---- Combine synonym-related dfs ----
  syms <- list(taxon, common, synonyms)
  syms_nonempty <- syms[vapply(syms, function(x) nrow(x) > 0, logical(1))]
  
  if (length(syms_nonempty) == 0) {
    syms_combined <- NULL
  } else if (length(syms_nonempty) == 1) {
    syms_combined <- syms_nonempty[[1]]
  } else {
    syms_combined <- purrr::reduce(syms_nonempty, dplyr::left_join, by = "assessment_id")
  }
  
  # ---- Output ----
  return(list(
    main = combined,
    syms = syms_combined
  ))
}

