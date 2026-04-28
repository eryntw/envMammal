compare_rank <- 
  function(data, id_col, value_cols,
           ref_col = value_cols[1],
           rank_fun = function(x) rank(-x, ties.method = "first"),
           diff_style = c("plusminus", "arrow", "none")) {
    
    library(dplyr)
    library(tidyr)
    library(purrr)
    
    diff_style <- match.arg(diff_style)
    
    id_col <- rlang::enquo(id_col)
    id_name <- rlang::as_name(id_col)
    
    if (!ref_col %in% value_cols) {
      stop("ref_col must be one of value_cols")
    }
    
    # Long format
    df_long <- data %>%
      select(!!id_col, all_of(value_cols)) %>%
      pivot_longer(cols = all_of(value_cols),
                   names_to = "day",
                   values_to = "value")
    
    # Rank per day
    df_ranked <- df_long %>%
      group_by(day) %>%
      mutate(rank = rank_fun(value)) %>%
      ungroup()
    
    # Reference ranks
    df_ref <- df_ranked %>%
      filter(day == ref_col) %>%
      select(!!id_col, ref_rank = rank)
    
    # Join + diff
    df_comp <- df_ranked %>%
      left_join(df_ref, by = id_name) %>%
      mutate(diff = ref_rank - rank)
    
    # Formatting
    format_diff <- function(stock, diff) {
      if (diff_style == "plusminus") {
        ifelse(diff > 0, paste0(stock, " +", diff),
               ifelse(diff < 0, paste0(stock, " ", diff),
                      paste0(stock, " 0")))
      } else if (diff_style == "arrow") {
        ifelse(diff > 0, paste0(stock, " ↑", diff),
               ifelse(diff < 0, paste0(stock, " ↓", abs(diff)),
                      paste0(stock, " -")))
      } else {
        paste0(stock)
      }
    }
    
    df_comp <- df_comp %>%
      mutate(label = format_diff(!!id_col, diff))
    
    # Sort each day independently
    day_tables <- df_comp %>%
      group_split(day) %>%
      set_names(value_cols) %>%
      purrr::map(~ .x %>%
                   arrange(rank) %>%
                   mutate(Row = row_number()) %>%
                   select(Row, label))
    
    # Join by row index
    df_final <- purrr::reduce(day_tables, full_join, by = "Row")
    
    colnames(df_final) <- c("Rank", value_cols)
    
    return(df_final)
  }