#' Extract max-indicator selection frequencies for a single COIN
#'
#' Computes the proportion of times each indicator is selected as the
#' maximum within its parent group.
#'
#' @param coin A COIN object
#' @param col A vector of parent indicator codes (Lv2+ iCode)
#'
#' @return A data frame with iCode, Parent, iName, and Percentage
#' @export
extract_coin_maxcol <- function(coin, col) {
  
  purrr::map(col, function(x) {
    res <- get_max_indicator(coin, x)$summary
    if (is.null(res)) return(NULL)
    res
  }) %>%
    purrr::compact() %>%  # drop NULL safely
    purrr::map_dfr(~ tibble::tibble(
      iCode = names(.x),
      Percentage = as.numeric(.x) / nrow(coin$Data$Raw)
    )) %>%
    dplyr::right_join(
      coin$Meta$Ind %>%
        dplyr::select(iCode, Parent, iName),
      by = "iCode"
    ) %>%
    dplyr::mutate(
      Percentage = dplyr::case_when(
        is.na(Percentage) & Parent %in% col ~ 0,
        is.na(Percentage) & !Parent %in% col ~ 1,
        TRUE ~ round(Percentage, 2)
      )
    )
}