#' Replace Taxa Names Using a Lookup Table
#'
#' Standardises taxonomic names in either a data frame column or a character
#' vector/list using a predefined lookup table.
#'
#' @param x A data frame, character vector, or list containing taxa names.
#' @param taxa_col Character. Column name if `x` is a data frame.
#' @param direction Character. Lookup direction:
#'   "forward" = names → values -->,
#'   "reverse" = values → names <--.
#'
#' @return Updated object with replaced taxa names.
#'
#' @export
replace_taxa <- function(x, taxa_col = NULL, direction = "forward") {
  
  # ===============================
  # Taxa replacement lookup table
  # ===============================
  
  lookup <- c(
    "Ardea intermedia" = "Ardea plumifera"
  )
  
  # ===============================
  # Reverse lookup if requested
  # ===============================
  
  if (direction == "reverse") {
    lookup <- setNames(names(lookup), lookup)
  }
  
  # ===============================
  # Replacement function
  # ===============================
  
  replace_vec <- function(vec) {
    vec[vec %in% names(lookup)] <- lookup[vec[vec %in% names(lookup)]]
    vec
  }
  
  # ===============================
  # Apply to data frame
  # ===============================
  
  if (is.data.frame(x)) {
    
    if (is.null(taxa_col) || !(taxa_col %in% names(x))) {
      stop("taxa_col must be provided for data frames.")
    }
    
    x[[taxa_col]] <- replace_vec(x[[taxa_col]])
    return(x)
  }
  
  # ===============================
  # Apply to vector or list
  # ===============================
  
  if (is.character(x)) {
    return(replace_vec(x))
  }
  
  if (is.list(x)) {
    return(replace_vec(unlist(x)))
  }
  
  stop("Input must be a data frame, character vector, or list.")
}