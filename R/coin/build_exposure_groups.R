build_exposure_groups <- function(iData, expcols) {
  
  library(stringr)
  
  # ---- Get all column names ----
  cols <- names(iData)
  
  # ---- Remove identifier columns ----
  cols <- cols[!cols %in% c("uName", "uCode")]
  
  # ---- Keep only exposure indicators (must contain certain prefix) ----
  exposure_cols <- cols[
    stringr::str_detect(cols, paste0("^(", paste(expcols, collapse="|"), ")_"))
  ]
  
  # ---- Extract parent prefix (before first underscore) ----
  parents <- stringr::str_extract(exposure_cols, "^[^_]+")
  
  # ---- Split into named list by parent ----
  exposure_groups <- split(exposure_cols, parents)
  
  # ---- Sort indicators within each group ----
  exposure_groups <- lapply(exposure_groups, sort)
  
  return(exposure_groups)
}