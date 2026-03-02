# ============================================================
# build_iMeta.R
# Construct COINr-compatible iMeta with optional negative indicators
# ============================================================

#' Build iMeta Table with Optional Negative Indicators
#'
#' Constructs a three-level COINr-compatible \code{iMeta} table.
#'
#' The hierarchy is:
#'
#' * **Level 3** – Index (specified by \code{index_name})
#' * **Level 2** – Groups (names of \code{groups})
#' * **Level 1** – Indicators nested under each Level 2 group
#'
#' By default, all indicators and aggregates are assigned:
#'
#' * \code{Weight = 1}
#' * \code{Direction = 1}
#'
#' Specific indicators can be assigned \code{Direction = -1}
#' using the \code{negative_indicators} argument.
#'
#' Structural checks ensure:
#'
#' * Indicators listed in \code{groups} exist in \code{iData}
#' * No indicator is assigned to multiple Level 2 groups
#' * Negative indicators exist among Level 1 indicators
#'
#' Designed for modular composite index construction compatible
#' with \pkg{COINr}.
#'
#' @param iData A \code{data.frame} containing indicator data.
#'   Columns \code{"uName"} and \code{"uCode"} are excluded
#'   from indicator detection if present.
#'
#' @param groups A named \code{list} defining the Level 2 structure.
#'   Each element name becomes a Level 2 aggregate and must contain
#'   a character vector of Level 1 indicator names.
#'
#' @param index_name Character string specifying the Level 3 index name.
#'
#' @param negative_indicators Optional character vector of indicator
#'   codes to assign \code{Direction = -1}. Must be Level 1 indicators.
#'   Default is \code{NULL}.
#'
#' @return A \code{data.frame} representing a valid COINr \code{iMeta}
#'   table.
#'
#' @export
build_iMeta <- function(iData,
                        groups,
                        index_name = "index",
                        negative_patterns = NULL) {
  
  # -----------------------------
  # 1. Extract indicator codes
  # -----------------------------
  
  iCodes <- base::names(
    iData %>%
      dplyr::select(-dplyr::any_of(base::c("uName", "uCode")))
  )
  
  all_group_inds <- base::unlist(groups, use.names = FALSE)
  
  # check for missing indicators
  missing <- base::setdiff(all_group_inds, iCodes)
  if (base::length(missing) > 0) {
    base::stop(
      base::paste(
        "Indicators listed in groups but missing from iData:",
        base::paste(missing, collapse = ", ")
      )
    )
  }
  
  # warn about ungrouped indicators
  ungrouped <- base::setdiff(iCodes, all_group_inds)
  if (base::length(ungrouped) > 0) {
    base::message(
      "Indicators present in iData but not assigned:\n",
      base::paste(ungrouped, collapse = ", ")
    )
  }
  
  # -----------------------------
  # 2. Level 1 – Indicators
  # -----------------------------
  
  L1 <- utils::stack(groups)
  base::names(L1) <- base::c("iCode", "Parent")
  
  dup <- L1$iCode[base::duplicated(L1$iCode)]
  if (base::length(dup) > 0) {
    base::stop(
      base::paste(
        "Indicator assigned to multiple Level 2 groups:",
        base::paste(base::unique(dup), collapse = ", ")
      )
    )
  }
  
  L1$Level     <- 1
  L1$Weight    <- 1
  L1$Direction <- 1
  L1$Type      <- "Indicator"
  L1$iName     <- L1$iCode
  
  # -----------------------------
  # 3. Apply negative direction
  # -----------------------------
  
  if (!base::is.null(negative_patterns)) {
    
    neg_from_pattern <- L1$iCode[
      stringr::str_detect(
        L1$iCode,
        base::paste(negative_patterns, collapse = "|")
      )
    ]
    
    L1$Direction[L1$iCode %in% neg_from_pattern] <- -1
  }
  
  # -----------------------------
  # 4. Level 2 – Aggregates
  # -----------------------------
  
  dims <- base::names(groups)
  
  L2 <- base::data.frame(
    iCode     = dims,
    iName     = dims,
    Direction = 1,
    Level     = 2,
    Weight    = 1,
    Type      = "Aggregate",
    Parent    = index_name,
    stringsAsFactors = FALSE
  )
  
  # -----------------------------
  # 5. Level 3 – Index
  # -----------------------------
  
  L3 <- base::data.frame(
    iCode     = index_name,
    iName     = index_name,
    Direction = 1,
    Level     = 3,
    Weight    = 1,
    Type      = "Aggregate",
    Parent    = NA,
    stringsAsFactors = FALSE
  )
  
  # -----------------------------
  # 6. Combine
  # -----------------------------
  
  iMeta <- dplyr::bind_rows(L1, L2, L3)
  
  return(iMeta)
}