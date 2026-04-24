# =========================================================
# COIN Log Extraction Utilities
# =========================================================
# This script provides helper functions to extract and 
# summarise processing steps from a COIN object into a 
# clean, human-readable table.
#
# Includes:
# 1. flatten_specs()  - Flatten nested specification lists
# 2. extract_weights() - Extract non-default weights
# 3. extract_coin_table() - Main summary table generator
# =========================================================


#' Flatten nested specification lists into readable strings
#'
#' Recursively flattens nested lists (e.g., global_specs,
#' indiv_specs) into a single character string. Supports:
#' - Custom defaults for NULL values
#' - Simplified formatting (for indiv_specs)
#' - Special formatting for level-specific values
#'
#' @param x A list, atomic vector, or NULL
#' @param default_global Value to return if x is NULL
#' @param simplify Logical; if TRUE, removes labels like
#'   "all levels" and collapses values directly
#'
#' @return A character string summarising the structure
#' @export
flatten_specs <- function(x, default_global = NULL, simplify = FALSE) {
  
  ## NULL ----
  if (is.null(x)) {
    if (!is.null(default_global)) return(default_global)
    return(NA_character_)
  }
  
  ## Atomic ----
  if (is.atomic(x)) {
    
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_character_)
    
    # simplified output (e.g., indiv_specs)
    if (simplify) {
      return(paste(x, collapse = ";\n"))
    }
    
    # default behaviour
    if (length(x) == 1) {
      return(paste0("all levels: ", x))
    }
    
    if (length(x) == 3) {
      return(paste0(
        "lv1: ", x[1], ";\n",
        "lv2: ", x[2], ";\n",
        "lv3: ", x[3]
      ))
    }
    
    return(paste(x, collapse = ";\n"))
  }
  
  ## List ----
  out <- c()
  
  for (nm in names(x)) {
    val <- x[[nm]]
    
    # drop function wrapper levels (e.g., f1, f2, f3)
    if (nm %in% c("f1", "f2", "f3")) {
      inner <- flatten_specs(val, simplify = simplify)
      out <- c(out, inner)
    } else {
      inner <- flatten_specs(val, simplify = simplify)
      out <- c(out, paste0(nm, ": ", inner))
    }
  }
  
  paste(out, collapse = ";\n")
}


#' Extract non-default aggregation weights from a COIN object
#'
#' Returns weights where Weight != 1 (i.e., non-uniform weights).
#'
#' @param coin A COIN object
#'
#' @return A character string listing weighted indicators, or "none"
#' @export
extract_weights <- function(coin) {
  
  w <- coin$Log$Aggregate$w
  
  if (is.null(w)) return("none")
  
  w.df <- coin$Meta$Weights[[coin$Log$Aggregate$w]]
  w_filtered <- w.df[w.df$Weight != 1, ]
  
  if (nrow(w_filtered) == 0) return("none")
  
  paste0(w_filtered$iCode, ": ", w_filtered$Weight, collapse = ";\n")
}


#' Extract COIN processing log into a summary table
#'
#' Creates a tidy table summarising the key steps in the
#' COIN workflow:
#' - Treat
#' - Normalise
#' - Aggregate
#'
#' Includes dataset transitions, specifications, and weights.
#'
#' @param coin A COIN object
#'
#' @return A tibble with components as rows and processing
#'   steps as columns
#' @export
#'
#' @examples
#' extract_coin_table(coin)
extract_log_table <- function(coin) {
  
  treat <- coin$Log$Treat
  norm  <- coin$Log$Normalise
  agg   <- coin$Log$Aggregate
  
  tibble::tibble(
    Component = c(
      "input dataset",
      "global method",
      "individual method",
      "weights (!=1)"
    ),
    
    Treat = c(
      treat$dset,
      flatten_specs(treat$global_specs),
      flatten_specs(treat$indiv_specs, simplify = TRUE),
      "Not Applicable"
    ),
    
    Normalise = c(
      norm$dset,
      flatten_specs(
        norm$global_specs,
        default_global = "n_minmax",
        simplify = TRUE
      ),
      flatten_specs(
        norm$indiv_specs,
        default_global = "none"
      ),
      "Not Applicable"
    ),
    
    Aggregate = c(
      agg$dset,
      flatten_specs(agg$f_ag),
      flatten_specs(agg$f_ag_para, default_global = "none"),
      extract_weights(coin)
    )
  )
}