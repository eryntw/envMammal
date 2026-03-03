#' Calculate Species Exposure by Threat and Category
#'
#' This function combines a mapped exposure matrix and a stressor matrix,
#' multiplies species exposure values by threat intensities, and aggregates
#' exposure scores by Species × Threat × Category.
#'
#' The returned data frame contains:
#'   - Rows: Species
#'   - Columns: Threat_Category combinations
#'   - Values: Summed exposure scores
#'
#' @param mapped_exposure Data frame containing exposure values.
#'   Must include:
#'     - `search_term`
#'     - species columns (binomial format, e.g. "Genus species")
#'
#' @param stressor_matrix Data frame containing threat information.
#'   Must include:
#'     - `birdcol_new`
#'     - `Category`
#'     - threat columns specified in `threat_cols`
#'
#' @param threat_cols Character vector of threat column names.
#'
#' @return A data frame with species as rows and
#'   Threat_Category combinations as columns.
#'
#' @export
calculate_category_exposure <- function(
    mapped_exposure,
    stressor_matrix,
    threat_cols
) {
  
  # ---- Validate inputs ----
  if (!"search_term" %in% names(mapped_exposure)) {
    stop("mapped_exposure must contain 'search_term'.")
  }
  
  if (!"birdcol_new" %in% names(stressor_matrix)) {
    stop("stressor_matrix must contain 'birdcol_new'.")
  }
  
  if (!"Category" %in% names(stressor_matrix)) {
    stop("stressor_matrix must contain 'Category'.")
  }
  
  if (!all(threat_cols %in% names(stressor_matrix))) {
    stop("Some threat_cols are not present in stressor_matrix.")
  }
  
  # ---- Reshape mapped exposure ----
  exposure_stressor_combined <- mapped_exposure %>%
    dplyr::select(-dplyr::any_of("common")) %>%
    tidyr::pivot_longer(
      cols = -search_term,
      names_to = "birdcol_new",
      values_to = "value"
    ) %>%
    tidyr::pivot_wider(
      names_from = search_term,
      values_from = value
    ) %>%
    dplyr::mutate(
      dplyr::across(where(is.numeric), ~ round(., 2))
    ) %>%
    dplyr::inner_join(stressor_matrix, by = "birdcol_new")
  
  # ---- Detect species columns (binomial pattern) ----
  species_cols <- grep(
    "^[A-Za-z]+\\s[A-Za-z]+$",
    names(exposure_stressor_combined),
    value = TRUE
  )
  
  if (length(species_cols) == 0) {
    stop("No species columns detected using binomial name pattern.")
  }
  
  # ---- Ensure numeric ----
  exposure_stressor_combined[, c(species_cols, threat_cols)] <-
    lapply(
      exposure_stressor_combined[, c(species_cols, threat_cols)],
      function(x) as.numeric(as.character(x))
    )
  
  # Replace NA in threats with 0
  exposure_stressor_combined[, threat_cols][
    is.na(exposure_stressor_combined[, threat_cols])
  ] <- 0
  
  # ---- Convert to long format and multiply ----
  df_long <- exposure_stressor_combined %>%
    dplyr::select(
      birdcol_new,
      Category,
      dplyr::all_of(threat_cols),
      dplyr::all_of(species_cols)
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(species_cols),
      names_to = "Species",
      values_to = "species_value"
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(threat_cols),
      names_to = "Threat",
      values_to = "threat_value"
    ) %>%
    dplyr::mutate(
      product = species_value * threat_value
    )
  
  # ---- Aggregate ----
  summed <- df_long %>%
    dplyr::group_by(Species, Threat, Category) %>%
    dplyr::summarise(
      value = sum(product, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      Threat_Category = paste0(Threat, "_", Category)
    )
  
  # ---- Final wide output ----
  result <- summed %>%
    dplyr::select(Species, Threat_Category, value) %>%
    tidyr::pivot_wider(
      names_from = Threat_Category,
      values_from = value,
      values_fill = 0
    ) %>%
    dplyr::mutate(
      dplyr::across(where(is.numeric), ~ round(., 2))
    ) %>% 
    dplyr::rename(search_term = Species)
  
  return(result)
}