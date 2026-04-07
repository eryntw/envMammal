#' Detect missing values for manual curation table (mtable)
#'
#' This function identifies missing or placeholder values in a trait dataset
#' and prepares a table of entries that require manual inspection or
#' imputation. The resulting table is used to create or update a
#' **manual curation table (mtable)**, where users can manually provide
#' corrected or imputed values.
#'
#' The function converts the dataset to long format and returns unique
#' combinations of `search_term` and `trait` where values are missing or
#' contain placeholder codes.
#'
#' Placeholder values currently detected include:
#' - `NA`
#' - `"-999"`
#' - `"NAV"`
#' - `"NaN"`
#'
#' These rows will be added to the manual table so they can be filled
#' manually during the data curation process.
#'
#' @param df A data frame containing species or taxon trait data.
#'   The data frame must contain a `search_term` column used as the
#'   species identifier.
#'
#' @return A tibble with two columns:
#' \describe{
#'   \item{search_term}{Species or taxon identifier}
#'   \item{trait}{Trait name containing missing or placeholder values}
#' }
#'
#' The output is typically used to generate or update a manual table
#' (`mtable`) where missing values can be filled by the user.
#'
#' @examples
#' \dontrun{
#' missing_tbl <- find_missing_values(sensitivity)
#' }
#'
#' @export
#'
#' @importFrom dplyr mutate across filter distinct
#' @importFrom tidyr pivot_longer
find_missing_values <- function(df) {
  
  cols <- setdiff(names(df), c("search_term", "aub_Taxon_common_name_2"))
  
  df %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(cols), as.character)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(cols),
      names_to = "trait",
      values_to = "value"
    ) %>%
    dplyr::filter(is.na(value) | value %in% c("-999", "NAV", "NaN")) %>%
    dplyr::distinct(search_term, trait)
  
}