# ============================================================
# map_traits.R
#
# Synchronise species trait data between two tables by copying
# all shared columns from A into B using a species key.
# ============================================================

#' Synchronise shared species trait columns between tables
#'
#' Copies all common columns from table A into table B based on a
#' shared species identifier column.
#'
#' @param A Source data.frame containing trait values.
#' @param B Target data.frame to receive mapped values.
#' @param x Character. Name of the key column shared by both tables
#'          (default = "scientific_name").
#' @param overwrite Logical.
#'        FALSE = only fill missing values in B (default)
#'        TRUE  = replace values in B whenever A has data
#' @param is_missing Values treated as missing in B.
#'
#' @return Updated B with synchronised columns.
#' @export
#'
map_traits <- function(A,
                       B,
                       x = "scientific_name",
                       overwrite = FALSE,
                       is_missing = c("", " ", "NA", "N/A", NA),
                       Atype = "long") {
  
  # -----------------------------
  # 1. Input checks
  # -----------------------------
  
  # Pivot A from long format
  if (!is.null(Atype) && Atype == "long") {
    A <- tidyr::pivot_wider(A, names_from = "trait", values_from = "value") %>% 
      readr::type_convert()
  }
  
  if (!x %in% names(A) || !x %in% names(B)) {
    stop("Key column not found in both tables: ", x, call. = FALSE)
  }
  
  # -----------------------------
  # 2. Automatically detect shared columns
  # -----------------------------
  map_cols <- intersect(names(A), names(B))
  map_cols <- setdiff(map_cols, x)
  
  if (length(map_cols) == 0) {
    stop("No shared columns between A and B to map.", call. = FALSE)
  }
  
  message("Mapping ", length(map_cols), " shared columns:")
  message(paste(map_cols, collapse = ", "))
  
  # -----------------------------
  # 3. Type harmonisation
  # Prevent numeric/character conflicts
  # -----------------------------
  for(col in map_cols){
    
    if(is.numeric(A[[col]])){
      B[[col]] <- suppressWarnings(as.numeric(B[[col]]))
    }
    
    if(is.character(A[[col]])){
      B[[col]] <- as.character(B[[col]])
    }
  }
  
  # -----------------------------
  # 4. Join A into B
  # -----------------------------
  A_sub <- A |>
    dplyr::select(dplyr::all_of(c(x, map_cols))) |>
    dplyr::distinct()
  
  joined <- B |>
    dplyr::left_join(A_sub, by = x, suffix = c("", ".new"))
  
  # -----------------------------
  # 5. Replacement logic
  # -----------------------------
  for(col in map_cols){
    
    newcol <- paste0(col, ".new")
    
    old <- joined[[col]]
    new <- joined[[newcol]]
    
    # Detect missing in B
    missing_old <- is.na(old)
    
    if(is.character(old)){
      missing_old <- missing_old | trimws(old) %in% is_missing
    }
    
    # Detect valid values in A
    valid_new <- !is.na(new)
    
    if(is.character(new)){
      valid_new <- valid_new & !(trimws(new) %in% is_missing)
    }
    
    if(overwrite){
      replace_idx <- valid_new
    } else {
      replace_idx <- missing_old & valid_new
    }
    
    replace_idx[is.na(replace_idx)] <- FALSE
    
    joined[[col]][replace_idx] <- new[replace_idx]
  }
  
  
  # -----------------------------
  # 6. Remove helper columns
  # -----------------------------
  joined <- joined |>
    dplyr::select(-dplyr::ends_with(".new"))
  
  return(joined)
}
