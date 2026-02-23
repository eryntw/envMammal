#' Retrieve IUCN Red List data with synonym retry
#'
#' Queries the IUCN API for a list of species and returns the cleaned
#' main species records. Species that are not found are automatically
#' re-queried using a synonym lookup table.
#'
#' The function expects a species list containing Genus and Species
#' columns and a column `search_term` with the binomial name
#' ("Genus species").
#'
#' @param splist A data frame containing species names. Must include
#'   columns `Genus`, `Species`, and `search_term`.
#' @param api An authenticated IUCN API object used by `get_iucn_clean()`.
#' @param synonym_path File path to a CSV containing synonym mappings.
#'   The CSV must include columns `.id` (original name) and `name_bi`
#'   (accepted binomial name).
#'
#' @return A list with two elements:
#' \describe{
#'   \item{iucn_data}{Data frame of all successfully retrieved IUCN records}
#'   \item{splist_iucn}{Original species list joined with IUCN data}
#' }
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Queries IUCN for each species
#'   \item Extracts valid "main" species records
#'   \item Identifies species not found
#'   \item Matches them to a synonym table
#'   \item Re-queries the API using accepted names
#' }
#'
#' Requires a working `get_iucn_clean()` function that returns a list
#' with elements `main` and `syms`.
#'
#' @examples
#' \dontrun{
#' output <- get_iucn_species_data(
#'   splist = species_list,
#'   api = api,
#'   synonym_path = "data/synonyms.csv"
#' )
#'
#' iucn_data <- output$iucn_data
#' splist_iucn <- output$splist_iucn
#' }
#'
#' @export
get_iucn_species_data <- function(splist, 
                                  api, 
                                  synonym_path = "H:/dev/eryn/envSens/data/synonyms.csv") {
  
  # ---- First IUCN query ----
  results <- splist %>%
    dplyr::mutate(
      result = purrr::pmap(
        list(Genus, Species),
        ~ get_iucn_clean(api = api, ..1, ..2)
      )
    )
  
  # ---- Extract main records ----
  mains_df <- results %>%
    dplyr::mutate(main = purrr::map(result, "main")) %>%
    dplyr::pull(main) %>%
    purrr::discard(is.null) %>%
    dplyr::bind_rows()
  
  # ---- Join to species list ----
  splist_iucn <- splist %>%
    dplyr::left_join(mains_df, by = c("search_term" = "scientific_name"))
  
  # ---- Identify not found species ----
  rows_null <- results %>%
    dplyr::mutate(
      both_null = purrr::map_lgl(result, ~ is.null(.x$main) && is.null(.x$syms))
    ) %>%
    dplyr::filter(both_null)
  
  # ---- Read synonym table ----
  synonyms <- readr::read_csv(
    synonym_path,
    col_types = readr::cols()
  )
  
  # ---- Match synonyms ----
  synmatch <- rows_null %>%
    dplyr::select(search_term) %>%
    dplyr::left_join(synonyms, by = c("search_term" = ".id")) %>%
    tidyr::separate(
      name_bi,
      into = c("Genus", "Species"),
      sep = " "
    )
  
  # ---- Second IUCN query using synonyms ----
  results2 <- synmatch %>%
    dplyr::mutate(
      result = purrr::pmap(
        list(Genus, Species),
        ~ get_iucn_clean(api = api, ..1, ..2)
      )
    )
  
  mains_df2 <- results2 %>%
    dplyr::mutate(main = purrr::map(result, "main")) %>%
    dplyr::pull(main) %>%
    purrr::discard(is.null) %>%
    dplyr::bind_rows()
  
  # ---- Combine results ----
  iucn_data <- dplyr::bind_rows(mains_df, mains_df2)
  
  return(list(
    iucn_data = iucn_data,
    splist_iucn = splist_iucn
  ))
}
