#' Create or update a manual curation table (mtable)
#'
#' This function creates or updates a **manual curation table (mtable)** used
#' for filling missing or placeholder values in a trait dataset. The mtable
#' stores rows that require manual input and allows users to curate values
#' outside the automated pipeline.
#'
#' The function performs the following steps:
#'
#' 1. If the manual table does not exist, it creates a new file using the
#'    supplied `missing_tbl` and adds an empty `value` column for manual input.
#' 2. If the manual table already exists, it reads the existing file and
#'    identifies new `(search_term, trait)` combinations that are not yet
#'    present.
#' 3. Newly detected rows are appended to the existing table while preserving
#'    any manually entered values.
#'
#' This function is typically used within a **`targets` pipeline** to maintain
#' a persistent manual table that updates automatically when new missing
#' values appear in the dataset.
#'
#' @param missing_tbl A data frame produced by `find_missing_values()`
#'   containing rows that require manual curation. It must contain the
#'   columns `search_term` and `trait`.
#'
#' @param path Character. File path where the manual table (`mtable`)
#'   should be stored.
#'
#' @return Character string giving the path to the manual table file.
#'   Returning the file path allows `targets` to track the file when
#'   used with `format = "file"` or `tar_file_read()`.
#'
#' @details
#' The resulting manual table has the structure:
#'
#' \describe{
#'   \item{search_term}{Species or taxon identifier}
#'   \item{trait}{Trait name requiring manual input}
#'   \item{value}{User-supplied value to fill missing data}
#' }
#'
#' Users should edit the `value` column manually to supply curated values.
#'
#' @examples
#' \dontrun{
#' missing_tbl <- find_missing_values(sensitivity)
#'
#' update_manual_table(
#'   missing_tbl,
#'   path = "data/current_mtable.csv"
#' )
#' }
#'
#' @export
#'
#' @importFrom dplyr mutate anti_join bind_rows
#' @importFrom readr read_csv write_csv
update_manual_table <- function(missing_tbl, path) {
  
  if (!file.exists(path)) {
    
    csv <- missing_tbl %>%
      dplyr::mutate(value = NA_character_)
    
    readr::write_csv(csv, path)
    message("Created new manual table")
    
    return(path)
  }
  
  processed <- readr::read_csv(path, show_col_types = FALSE)
  
  new_rows <- missing_tbl %>%
    dplyr::anti_join(processed, by = c("search_term","trait")) %>%
    dplyr::mutate(value = NA_character_)
  
  if (nrow(new_rows) > 0) {
    
    csv <- dplyr::bind_rows(processed, new_rows)
    
    readr::write_csv(csv, path)
    
    message(nrow(new_rows), " new rows added")
    
  }
  
  return(path)
}