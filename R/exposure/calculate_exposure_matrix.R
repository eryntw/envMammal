#' Calculate species × threat exposure matrix
#'
#' @description
#' Combines mapped exposure data with a stressor matrix and computes
#' a species-by-threat exposure matrix using matrix multiplication.
#'
#' Workflow:
#' 1. Reshape mapped_exposure from wide to long and back to wide.
#' 2. Round numeric exposure values.
#' 3. Join with stressor_matrix by "birdcol_new".
#' 4. Detect species columns (binomial names).
#' 5. Extract species and threat matrices.
#' 6. Replace NA in threat matrix with 0.
#' 7. Perform matrix multiplication:
#'        t(species_matrix) %*% threat_matrix
#'
#' @param mapped_exposure A data frame containing species exposure values.
#' @param stressor_matrix A data frame containing threat scores,
#'        joined by column "birdcol_new".
#' @param threat_cols Character vector of threat column names.
#'
#' @return A data frame with:
#' - Rows = species
#' - Columns = threats
#' - Values = summed exposure × stressor score
#'
#' @export
calculate_exposure_matrix <- function(mapped_exposure,
                                      stressor_matrix,
                                      threat_cols) {
  
  # ---- Validate inputs ----
  if (!"search_term" %in% names(mapped_exposure)) {
    stop("mapped_exposure must contain 'search_term'.")
  }
  
  if (!"birdcol_new" %in% names(stressor_matrix)) {
    stop("stressor_matrix must contain 'birdcol_new'.")
  }
  
  if (!all(threat_cols %in% names(stressor_matrix))) {
    stop("Some threat_cols are not present in stressor_matrix.")
  }
  
  # ---- Reshape mapped exposure ----
  exposure_stressor_combined <- mapped_exposure %>%
    dplyr::select(-common) %>%
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
  
  # ---- Detect species columns (binomial format) ----
  species_cols <- grep("^[A-Za-z]+\\s[A-Za-z]+$",
                       names(exposure_stressor_combined),
                       value = TRUE)
  
  if (length(species_cols) == 0) {
    stop("No species columns detected using binomial name pattern.")
  }
  
  # ---- Ensure numeric ----
  exposure_stressor_combined[, c(species_cols, threat_cols)] <-
    lapply(
      exposure_stressor_combined[, c(species_cols, threat_cols)],
      function(x) as.numeric(as.character(x))
    )
  
  # ---- Extract matrices ----
  species_mat <- as.matrix(
    exposure_stressor_combined[, species_cols]
  )
  
  threat_mat <- as.matrix(
    exposure_stressor_combined[, threat_cols]
  )
  
  # Replace NA in threat matrix
  threat_mat[is.na(threat_mat)] <- 0
  
  # ---- Matrix multiplication ----
  result <- t(species_mat) %*% threat_mat
  
  # ---- Format output ----
  result_df <- as.data.frame(result)
  result_df$search_term <- rownames(result_df)
  rownames(result_df) <- NULL
  result_df <- result_df[, c("search_term", threat_cols)]
  
  return(result_df)
}