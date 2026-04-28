#' Create a stock-style ranked GT table with shifted layout
#'
#' @description
#' Takes a wide dataset (e.g. vulntable) and produces a stock-style table where:
#' - each column is independently ranked
#' - rows represent rank positions
#' - differences are computed relative to a reference column
#' - differences are colour-coded (green/red/grey)
#' - biggest movers are highlighted
#'
#' @param data A data.frame in wide format
#' @param id_col Column containing entity names
#' @param value_cols Character vector of numeric columns to rank
#' @param ref_col Reference column for comparison (default = first)
#'
#' @return A gt table
#' @export
#'
#' @examples
#' rank_table_gt(vulntable, uName, c("w1","Max-Lv1","Max-Lv2","Agg-gmean"))
rank_table_gt <- function(data,
                          id_col,
                          value_cols,
                          ref_col = value_cols[1]) {
  
  id_col <- rlang::enquo(id_col)
  id_name <- rlang::as_name(id_col)
  
  # =========================================================
  # 1. Long format + ranking
  # =========================================================
  
  df_long <- data %>%
    dplyr::select(!!id_col, dplyr::all_of(value_cols)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(value_cols),
      names_to = "day",
      values_to = "value"
    )
  
  df_ranked <- df_long %>%
    dplyr::group_by(day) %>%
    dplyr::mutate(rank = rank(-value, ties.method = "first")) %>%
    dplyr::ungroup()
  
  # =========================================================
  # 2. Compute differences vs reference
  # =========================================================
  
  df_ref <- df_ranked %>%
    dplyr::filter(day == ref_col) %>%
    dplyr::select(!!id_col, ref_rank = rank)
  
  df_comp <- df_ranked %>%
    dplyr::left_join(df_ref, by = id_name) %>%
    dplyr::mutate(diff = ref_rank - rank)
  
  # Biggest movers per day
  df_comp <- df_comp %>%
    dplyr::group_by(day) %>%
    dplyr::mutate(
      max_move_day = max(abs(diff), na.rm = TRUE),
      is_biggest = abs(diff) == max_move_day & diff != 0
    ) %>%
    dplyr::ungroup()
  
  # =========================================================
  # 3. Create shifted layout (rank-position based)
  # =========================================================
  
  df_comp <- df_comp %>%
    dplyr::mutate(day = factor(day, levels = value_cols))
  
  day_tables <- df_comp %>%
    split(.$day) %>%
    purrr::map(~ .x %>%
                 dplyr::arrange(rank) %>%
                 dplyr::mutate(Row = dplyr::row_number()) %>%
                 dplyr::select(Row, !!id_col, diff, is_biggest))
  
  df_shifted <- purrr::reduce(day_tables, dplyr::full_join, by = "Row")
  
  colnames(df_shifted) <- c(
    "Rank",
    as.vector(rbind(value_cols,
                    paste0(value_cols, "_diff"),
                    paste0(value_cols, "_big")))
  )
  
  # =========================================================
  # 4. Format HTML labels
  # =========================================================
  
  format_diff_html <- function(stock, diff) {
    
    diff_str <- ifelse(diff > 0, paste0("+", diff),
                       ifelse(diff < 0, as.character(diff), "0"))
    
    color <- ifelse(diff > 0, "#00C853",
                    ifelse(diff < 0, "#D50000", "#9E9E9E"))
    
    paste0(
      stock,
      " <span style='color:", color, "; font-weight:600;'>",
      diff_str,
      "</span>"
    )
  }
  
  # Build label columns
  for (col in value_cols) {
    df_shifted[[col]] <- format_diff_html(
      df_shifted[[col]],
      df_shifted[[paste0(col, "_diff")]]
    )
  }
  
  # =========================================================
  # 5. Build GT table
  # =========================================================
  
  label_cols <- value_cols
  big_cols   <- paste0(value_cols, "_big")
  
  gt_tbl <- df_shifted %>%
    dplyr::select(Rank, dplyr::all_of(label_cols)) %>%
    gt::gt() %>%
    gt::fmt_markdown(dplyr::everything())
  
  # Highlight biggest movers
  for (i in seq_along(label_cols)) {
    
    gt_tbl <- gt_tbl %>%
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#FFF59D"),
          gt::cell_text(weight = "bold")
        ),
        locations = gt::cells_body(
          columns = label_cols[i],
          rows = df_shifted[[big_cols[i]]]
        )
      )
  }
  
  return(gt_tbl)
}