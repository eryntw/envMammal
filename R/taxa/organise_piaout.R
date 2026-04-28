#' Organise PIA Output into a Clean Species List
#'
#' Cleans and standardises a PIA species list. It removes duplicates,
#' determines listing hierarchy (EPBC → NPW → future → AOI contribution),
#' prefixes columns with "pia_", retrieves taxonomy from ALA,
#' and splits species tables by major taxonomic class.
#'
#' @param df A data frame containing at minimum:
#'   taxa, epbc, npw, future, aoi_cont
#'
#' @return A named list of data frames split by taxonomic class
#' @export

organise_piaout <- function(df) {
  
  # 1 Clean and build listing hierarchy
  final <- df %>%
    dplyr::select(
      taxa,
      epbc,
      npw,
      future,
      aoi_cont,
      common_vals,
      kingdom
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      listing_hierarchy = dplyr::case_when(
        epbc == TRUE ~ "epbc",
        npw == TRUE ~ "npw",
        future == TRUE ~ "future",
        aoi_cont == TRUE ~ "aoi_cont",
        TRUE ~ "unknown"
      )
    )
  
  # 2 Add pia_ prefix
  final <- final %>%
    dplyr::rename_with(
      ~ paste0("pia_", .x),
      -taxa
    )
  
  # 3 Retrieve taxonomy from ALA
  ala <- galah::search_taxa(final$taxa) %>%
    dplyr::distinct() %>%
    dplyr::select(
      search_term,
      kingdom,
      class,
      family,
      vernacular_name,
      match_type
    ) %>%
    dplyr::rename_with(
      ~ paste0("ala_", .x),
      -search_term
    )
  
  # 4 Join ALA taxonomy
  splist <- final %>%
    dplyr::left_join(
      ala,
      by = dplyr::join_by(taxa == search_term)
    ) %>%
    dplyr::rename(search_term = taxa)
  
  # 5 Keep major groups
  class_list <- c(
    "Equisetopsida",
    "Aves",
    "Mammalia",
    "Reptilia",
    "Actinopterygii",
    "Amphibia"
  )
  
  splist <- splist %>%
    dplyr::filter(ala_class %in% class_list)
  
  # 6 Split by class
  sp_tables <- base::split(
    splist,
    splist$ala_class
  )
  
  return(sp_tables)
  
}