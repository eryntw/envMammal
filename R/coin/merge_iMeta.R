# ============================================================
# merge_iMeta_list.R
# Merge multiple COINr iMeta tables under a new parent
# ============================================================

#' Merge Multiple iMeta Tables Under a New Parent Aggregate
#'
#' Merges two or more COINr-compatible \code{iMeta} tables that share
#' the same highest hierarchical level and nests them under a new
#' parent aggregate.
#'
#' The function:
#'
#' * Requires at least two \code{iMeta} tables
#' * Verifies identical maximum Level across inputs
#' * Binds all rows together
#' * Reassigns current top-level nodes to a new parent
#' * Adds a new aggregate one level above the previous maximum
#'
#' The new parent row is appended at the end and assigned:
#'
#' * \code{Weight = 1}
#' * \code{Direction = 1}
#'
#' Designed for modular composite index construction in \pkg{COINr}.
#'
#' @param iMeta_list A \code{list} of at least two COINr-compatible
#'   \code{iMeta} data frames.
#'
#' @param parent_name Character string specifying the name of the new
#'   highest-level aggregate.
#'
#' @return A merged \code{iMeta} \code{data.frame} with a new highest
#'   Level equal to \code{max(Level) + 1}.
#'
#' @details
#' All input tables must:
#'
#' * Contain columns:
#'   \code{iCode, iName, Direction, Level, Weight, Type, Parent}
#' * Share the same maximum Level
#' * Not contain duplicated \code{iCode} values across tables
#'
#' If duplicated \code{iCode} values are detected across inputs,
#' the function stops to prevent ambiguous hierarchy.
#'
#' @examples
#' \dontrun{
#' merged <- merge_iMeta_list(
#'   iMeta_list = list(
#'     sensitivity_iMeta,
#'     pressure_iMeta,
#'     exposure_iMeta
#'   ),
#'   parent_name = "Vulnerability Index"
#' )
#' }
#'
#' @export
merge_iMeta <- function(iMeta_list,
                        parent_name) {
  
  # -----------------------------
  # 1. Basic validation
  # -----------------------------
  
  if (!base::is.list(iMeta_list) ||
      base::length(iMeta_list) < 2) {
    base::stop("Provide a list of at least two iMeta tables.")
  }
  
  required_cols <- base::c(
    "iCode", "iName", "Direction",
    "Level", "Weight", "Type", "Parent"
  )
  
  # validate each table
  for (i in base::seq_along(iMeta_list)) {
    if (!base::all(required_cols %in% base::names(iMeta_list[[i]]))) {
      base::stop(
        base::paste("iMeta_list[[", i,
                    "]] does not contain required columns.", sep = "")
      )
    }
  }
  
  # -----------------------------
  # 2. Check highest Level match
  # -----------------------------
  
  max_levels <- base::sapply(
    iMeta_list,
    function(x) base::max(x$Level, na.rm = TRUE)
  )
  
  if (base::length(base::unique(max_levels)) != 1) {
    base::stop("All iMeta tables must share the same highest Level.")
  }
  
  current_max <- max_levels[1]
  new_level   <- current_max + 1
  
  # -----------------------------
  # 3. Check duplicate iCodes
  # -----------------------------
  
  all_codes <- base::unlist(
    base::lapply(iMeta_list, function(x) x$iCode),
    use.names = FALSE
  )
  
  dup_codes <- all_codes[base::duplicated(all_codes)]
  
  if (base::length(dup_codes) > 0) {
    base::stop(
      base::paste(
        "Duplicate iCode detected across iMeta tables:",
        base::paste(base::unique(dup_codes), collapse = ", ")
      )
    )
  }
  
  # -----------------------------
  # 4. Reassign top-level Parents
  # -----------------------------
  
  for (i in base::seq_along(iMeta_list)) {
    top_nodes <- iMeta_list[[i]]$iCode[
      iMeta_list[[i]]$Level == current_max
    ]
    
    iMeta_list[[i]]$Parent[
      iMeta_list[[i]]$iCode %in% top_nodes
    ] <- parent_name
  }
  
  # -----------------------------
  # 5. Bind all tables
  # -----------------------------
  
  merged <- dplyr::bind_rows(iMeta_list)
  
  # -----------------------------
  # 6. Add new highest-level row
  # -----------------------------
  
  new_row <- base::data.frame(
    iCode     = parent_name,
    iName     = parent_name,
    Direction = 1,
    Level     = new_level,
    Weight    = 1,
    Type      = "Aggregate",
    Parent    = NA,
    stringsAsFactors = FALSE
  )
  
  merged <- dplyr::bind_rows(merged, new_row)
  
  return(merged)
}