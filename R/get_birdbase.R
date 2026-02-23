#' Prepare and optionally subset birdbase dataset
#'
#' Cleans birdbase, calculates migration score and dietary diversity
#'
#' @param birdbase Data frame containing bird traits
#' @param subset Logical. If TRUE returns only analysis variables. 
#' If FALSE returns original data with added variables.
#'
#' @return A processed tibble
#' @export

get_birdbase <- function(birdbase, subset = FALSE) {
  
  hb_cols <- c("F", "Bm", "Wd", "Sh", "Sv", "G", "Pl", "R",
               "D", "A", "C", "Rv", "W", "Se", "O")
  
  db_cols <- birdbase %>%
    dplyr::select(dplyr::contains("Wt"), -SumWt) %>%
    names()
  
  # ---- Step 1: ALWAYS process data ----
  birdbase_proc <- birdbase %>%
    
    # ---- Fix T ----
    dplyr::mutate(
      dplyr::across(dplyr::where(is.character), ~ dplyr::na_if(., "T"))
    ) %>%
    # ---- Fix NA in habitat cols ----
    dplyr::mutate(
      dplyr::across(dplyr::all_of(hb_cols), ~ tidyr::replace_na(., 0))
    ) %>%
    # ---- Score migration ----
    dplyr::mutate(
      obligate_migrant = dplyr::case_when(
        Mig == 1 ~ 1,   # full migrant
        TRUE     ~ 0
      )
    ) %>%
    # ---- character/numeric/integer ----
    readr::type_convert() %>% 
    
    # ---- force numeric ----
    mutate(across(contains("bb_Norm"), readr::parse_number)) %>% 
    
    # ---- bind diversity indices ----
    dplyr::bind_cols(
      calc_diversity(., db_cols) |>
        dplyr::rename(db_shannon = shannon,
                      db_simpson = simpson)
    )
  
  # ---- Step 2: OPTIONAL subsetting ----
  if (subset) {
    
    birdbase_proc <- birdbase_proc %>%
      dplyr::select(
        Genus, Species, common,
        PrimaryDiet, Db, Hb, Rr, ElevationalRange, Mig, Alt,
        dplyr::all_of(db_cols),
        dplyr::all_of(hb_cols),
        mig_score, db_shannon, db_simpson
      )
  }
  
  return(birdbase_proc)
}
