#' Get dominant indicator (max contributor) within a parent group
#'
#' This function identifies, for each row in a COIN object, which indicator
#' (within a specified parent group) has the maximum value. It returns both
#' the row-wise max values and the indicator responsible, along with a summary
#' frequency table.
#'
#' @param coin A COIN object containing `$Meta$Ind` and `$Data$Aggregated`
#' @param parent A character string specifying the Parent group (e.g., "Constraints")
#'
#' @return A list with:
#' \describe{
#'   \item{data}{A dataframe with max_value and max_col per row}
#'   \item{summary}{A frequency table of which indicators are most often maximal}
#' }
#'
#' @examples
#' result <- get_max_indicator_summary(coin_12, "Constraints")
#' result$summary
#'
get_max_indicator <- function(coin, parent) {
  
  # load required packages
  require(dplyr)
  
  # ---- 1. Get indicator codes for the specified parent ----
  ids <- coin$Meta$Ind %>%
    dplyr::filter(Parent == parent) %>%
    dplyr::pull(iCode)
  
  # ---- 2. Extract aggregated data ----
  df <- coin$Data$Aggregated
  
  # ---- 3. Compute row-wise max value and source column ----
  result_df <- df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      max_value = max(dplyr::c_across(dplyr::all_of(ids)), na.rm = TRUE),
      max_col = ids[which.max(c_across(all_of(ids)))]
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(max_value, max_col)
  
  # ---- 4. Summary table ----
  summary_tab <- table(result_df$max_col)
  
  # ---- 5. Return output ----
  return(list(
    data = result_df,
    summary = summary_tab
  ))
}