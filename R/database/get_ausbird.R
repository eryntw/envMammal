#' Prepare ausbird habitat dataset
#'
#' Cleans habitat binary variables and calculates habitat breadth metrics
#'
#' @param ausbird Data frame containing AusBird species attributes
#' @param subset Logical. If TRUE returns only key variables and metrics.
#'
#' @return A tibble with BreedingHB and FeedingHB habitat breadth indices
#' @export

get_ausbird <- function(ausbird, subset = FALSE) {
  
  ausbird <- dplyr::as_tibble(ausbird)
  
  # ---- identify habitat columns ----
  breeding_cols <- grep("BreedingHabitat", names(ausbird), value = TRUE)
  feeding_cols  <- grep("FeedingHabitat",  names(ausbird), value = TRUE)
  
  # ---- replace NA with 0 (migratory birds) ----
  ausbird_proc <- ausbird %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(c(breeding_cols, 
                                    feeding_cols, 
                                    "NonBreedingOnly4")),
                    ~ tidyr::replace_na(., 0))
    )

  ausbird_proc <- ausbird_proc %>%
    dplyr::mutate(
      
      # ---- calculate habitat breadth ----
      BreedingHB = rowSums(dplyr::across(dplyr::all_of(breeding_cols)) == 1, na.rm = TRUE),
      FeedingHB  = rowSums(dplyr::across(dplyr::all_of(feeding_cols))  == 1, na.rm = TRUE),
      
      # ---- score migration ----
      obligate_migrant = dplyr::case_when(Migratory6 == "Full migrant" ~ 1, TRUE ~ 0),
      
      # ---- score anthro habitats ----
      score_anthro_habitat = score_ag_urb_habitats(.)

    )

  
  # ---- optional subsetting ----
  if (subset) {
    
    ausbird_proc <- ausbird_proc %>%
      dplyr::select(
        dplyr::any_of(c("scientific_name", "common_name",
                        "BreedingHB", "FeedingHB"))
      )
  }
  
  return(ausbird_proc)
}
