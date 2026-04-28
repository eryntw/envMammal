#' Rescale threat scores within Category groups
#'
#' @description
#' Rescales specified numeric threat columns within each `Category` group
#' by dividing each value by the maximum value observed in that group.
#'
#' This ensures that the highest score within each Category becomes 1,
#' and all other values are scaled proportionally.
#'
#' The transformation is equivalent to:
#'   scaled_value = value / max(value within Category)
#'
#' Behaviour:
#' - If max = 3 → values become 1, 0.67, 0.33
#' - If max = 2 → values become 1, 0.5
#' - If max = 1 → values remain 1
#' - If max = 0 or all NA → values remain unchanged
#' - NA values are preserved
#'
#' @param df A data frame containing a `Category` column and threat columns.
#' @param threat_cols A character vector of column names to rescale.
#'
#' @return A data frame with the specified threat columns rescaled within
#' each `Category`.
#'
#' @details
#' The function performs grouped mutation using `dplyr`. It does not modify
#' non-specified columns. Grouping is removed before returning.
#'
#' @examples
#' threat_cols <- c("HabitatLoss", "Invasive_cat")
#' df_scaled <- rescale_exposure_matrix(df, threat_cols)
#'
#' @export
rescale_exposure_matrix <- function(df, threat_cols) {
  
  # ---- Basic input checks ----
  if (!"Category" %in% names(df)) {
    stop("Column 'Category' not found in data frame.")
  }
  
  if (!all(threat_cols %in% names(df))) {
    stop("Some threat_cols are not present in the data frame.")
  }
  
  # ---- Force numeric ----
  df <- df %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(threat_cols),
        ~ as.numeric(as.character(.))
      )
    )
  
  # ---- Rescaling ----
  df %>%
    dplyr::group_by(Category) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(threat_cols),
        ~ if (all(is.na(.))) {
          .
        } else {
          max_val <- max(., na.rm = TRUE)
          if (max_val > 0) . / max_val else .
        }
      )
    ) %>%
    dplyr::ungroup()
}