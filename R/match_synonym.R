#' Retrieve and cache taxonomic synonyms from ITIS
#'
#' Queries the Integrated Taxonomic Information System (ITIS) via
#' \code{taxize::synonyms()} to retrieve taxonomic synonym information for a
#' vector of taxon names. Results are stored in a persistent CSV file so that
#' previously queried taxa are not requested again in subsequent runs.
#'
#' The function works in two stages:
#'
#' \itemize{
#' \item If the synonym file does not exist, all supplied taxa are queried from ITIS.
#' \item If the file already exists, only taxa not present in the file are queried.
#' }
#'
#' This design avoids repeated API calls to ITIS and allows synonym results
#' to accumulate over time as additional taxa are processed.
#'
#' @param taxa Character vector of taxon names (e.g. `"Genus species"`).
#'   Duplicate taxa are automatically removed.
#'
#' @param path Character. File path where the synonym lookup table will be
#'   stored. If the file exists, it will be read and updated only with new taxa.
#'   Defaults to `"data/synonyms.csv"`.
#'
#' @details
#' The returned table contains synonym records retrieved from ITIS. Each row
#' represents a synonym relationship associated with a queried taxon.

#' @return
#' A \code{data.frame} containing synonym relationships for all queried taxa.
#' If the cache file already exists, previously retrieved results are included.
#'
#' @seealso
#' \code{\link[taxize]{synonyms}}
#'
#' @examples
#' \dontrun{
#' taxa <- c("Puma concolor", "Panthera leo")
#'
#' syn_df <- match_synonym(
#'   taxa,
#'   path = "data/synonyms.csv"
#' )
#' }
#'
#' @export

#' Internal helper to fetch and tidy ITIS synonym records
#'
#' Queries ITIS via \code{taxize::synonyms()} and standardises the output so
#' that taxa with no synonyms still produce a valid row in the returned table.
#'
#' @param x Character vector of taxon names.
#'
#' @return
#' A data.frame containing synonym records for each queried taxon.
#'
#' @keywords internal

fetch_synonyms <- function(x) {
  
  syn <- taxize::synonyms(x, db = "itis", ask = FALSE)
  
  # Step 1: standardize empty data frames
  lst_standard <- Map(
    function(df, nm) {
      if (is.data.frame(df) && nrow(df) == 0 && ncol(df) == 0) {
        data.frame(
          .id       = nm,
          sub_tsn    = NA_character_,
          acc_tsn    = NA_character_,
          syn_author = NA_character_,
          syn_name   = NA_character_,
          syn_tsn    = NA_character_,
          stringsAsFactors = FALSE
        )
      } else {
        df
      }
    },
    syn,
    names(syn)
  )
  
  # Step 2: bind rows with .id = "taxa"
  df_bound <- dplyr::bind_rows(lst_standard, .id = ".id") %>% 
    select(-matches("^dummy$")) %>% 
    dplyr::mutate(across(everything(), ~ tidyr::replace_na(.x, "NoData")))
  
  return(df_bound)
  
}

## main function ------

match_synonym <- function(taxa, path = "../data/synonyms.csv") {
  
  # ensure unique input
  taxa <- unique(taxa)
  
  # ---- Case 1: no existing file: make new synonym file ----
  if (!base::file.exists(path)) {
    
    message("No existing synonym file found — querying all taxa")
    
    syn_df <- fetch_synonyms(taxa)
    readr::write_csv(syn_df, path)
    
    return(syn_df)
  }
  
  # ---- Case 2: file exists: only process new taxa ----
  processed <- readr::read_csv(path, col_types = readr::cols())
  
  new_taxa <- base::setdiff(taxa, processed$.id)
  
  if (length(new_taxa) == 0) {
    message("No new taxa to query — returning existing file")
    return(processed)
  }
  
  message("Querying synonyms for ", length(new_taxa), " new taxa")
  
  syn_new <- fetch_synonyms(new_taxa)
  
  combined <- dplyr::bind_rows(processed, syn_new) %>%
    dplyr::distinct()
  
  readr::write_csv(combined, path)
  
  return(combined)
}
