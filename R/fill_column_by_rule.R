#' Fill a New Column Based on a Rule Applied Across Selected Columns
#'
#' Creates a new column in a data frame by applying one of three aggregation
#' rules (`"mean"`, `"median"`, or `"hierarchy"`) row-wise across a specified
#' set of existing columns.
#'
#' @param df A data frame or tibble.
#' @param new_col A string. The name of the new column to create.
#' @param cols A character vector of column names in `df` to apply the rule to.
#'   Columns must be numeric for `"mean"` and `"median"` rules. For
#'   `"hierarchy"`, columns can be any type; the first non-`NA` value in the
#'   order supplied is used.
#' @param rule A string. One of `"mean"`, `"median"`, or `"hierarchy"`:
#'   \describe{
#'     \item{`"mean"`}{Row-wise mean across `cols`, ignoring `NA` values. Returns
#'       `NA` if all values in a row are `NA`.}
#'     \item{`"median"`}{Row-wise median across `cols`, ignoring `NA` values.
#'       Returns `NA` if all values in a row are `NA`.}
#'     \item{`"hierarchy"`}{Returns the first non-`NA` value across `cols` in
#'       the order they are supplied. Returns `NA` if all values in a row are
#'       `NA`.}
#'   }
#'
#' @return A tibble identical to `df` with the new column `new_col` appended.
#'
#' @details
#' The function uses `dplyr::mutate()` combined with `purrr::pmap()` to apply
#' the rule row-wise, preserving the tidy workflow and compatibility with
#' grouped or ungrouped tibbles. Column selection is validated before
#' processing to provide informative errors early.
#'
#' For `"mean"` and `"median"`, columns are coerced to numeric inside the
#' row-wise function. A warning will be raised by R if coercion produces `NA`
#' (e.g. passing character columns).
#'
#' @examples
#' library(dplyr)
#'
#' df <- tibble(
#'   species = c("Sp_A", "Sp_B", "Sp_C", "Sp_D"),
#'   score_1 = c(3.2,  NA,  1.5, NA ),
#'   score_2 = c(4.1,  2.8, NA,  NA ),
#'   score_3 = c(NA,   3.0, 2.2, NA )
#' )
#'
#' # Mean rule
#' fill_column_by_rule(df, "mean_score",   c("score_1", "score_2", "score_3"), "mean")
#'
#' # Median rule
#' fill_column_by_rule(df, "median_score", c("score_1", "score_2", "score_3"), "median")
#'
#' # Hierarchy rule (score_1 takes priority, then score_2, then score_3)
#' fill_column_by_rule(df, "priority_score", c("score_1", "score_2", "score_3"), "hierarchy")
#'
#' @importFrom dplyr mutate
#' @importFrom purrr pmap_dbl pmap
#' @export
fill_column_by_rule <- function(df, new_col, cols, rule = c("mean", "median", "hierarchy")) {
  
  # --- Input validation -------------------------------------------------------
  
  rule <- match.arg(rule)
  
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame or tibble.")
  }
  
  if (!is.character(new_col) || length(new_col) != 1 || nchar(trimws(new_col)) == 0) {
    stop("`new_col` must be a single non-empty string.")
  }
  
  if (!is.character(cols) || length(cols) == 0) {
    stop("`cols` must be a non-empty character vector of column names.")
  }
  
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0) {
    stop("The following columns were not found in `df`: ",
         paste(missing_cols, collapse = ", "))
  }
  
  if (new_col %in% names(df)) {
    warning("`new_col` '", new_col, "' already exists in `df` and will be overwritten.")
  }
  
  # --- Row-wise rule functions ------------------------------------------------
  
  rule_fn <- switch(rule,
                    
                    mean = function(...) {
                      vals <- as.numeric(c(...))
                      if (all(is.na(vals))) NA_real_ else mean(vals, na.rm = TRUE)
                    },
                    
                    median = function(...) {
                      vals <- as.numeric(c(...))
                      if (all(is.na(vals))) NA_real_ else median(vals, na.rm = TRUE)
                    },
                    
                    hierarchy = function(...) {
                      vals <- c(...)
                      first_non_na <- vals[!is.na(vals)]
                      if (length(first_non_na) == 0) NA else first_non_na[[1]]
                    }
  )
  
  # --- Apply via mutate + pmap ------------------------------------------------
  
  # pmap_dbl enforces numeric output for mean/median (safer type stability)
  # pmap gives flexible output for hierarchy (preserves original column type)
  
  df <- dplyr::mutate(
    df,
    !!new_col := {
      if (rule %in% c("mean", "median")) {
        purrr::pmap_dbl(dplyr::pick(dplyr::all_of(cols)), rule_fn)
      } else {
        purrr::pmap(dplyr::pick(dplyr::all_of(cols)), rule_fn) |> unlist()
      }
    }
  )
  
  return(df)
}