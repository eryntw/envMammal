#' Summarise IUCN threat impacts
#'
#' Aggregates IUCN Level-2 threat scores into Level-1 categories and
#' computes species-level cumulative threat pressure.
#'
#' @param scored_threat A data.frame containing IUCN threat scoring output.
#' Must contain columns:
#' `scientific_name`, `common`, `threat_lv1`,
#' `severity_score`, `scope_score`, `timing_score`,
#' and `threat_impact_score`.
#'
#' @return A named list with two data.frames:
#' \describe{
#'   \item{Lv1_summary}{One row per species × Level-1 threat category}
#'   \item{species_summary}{One row per species}
#' }
#' @export
#'
#' @importFrom dplyr mutate group_by summarise n
#' @importFrom tidyr replace_na
summarise_iucn_threat <- function(scored_threat){
  
  required_cols <- c(
    "scientific_name",
    "common",
    "threat_lv1",
    "severity_score",
    "scope_score",
    "timing_score",
    "threat_impact_score"
  )
  
  if(!all(required_cols %in% names(scored_threat))){
    stop(
      "Input data must contain columns: ",
      paste(required_cols, collapse = ", ")
    )
  }
  
  # ---------- Level 1 threat summary ----------
  Lv1_summary <- scored_threat %>%
    dplyr::mutate(
      threat_impact_score = tidyr::replace_na(threat_impact_score, 0)
    ) %>%
    dplyr::group_by(scientific_name, common, threat_lv1) %>%
    dplyr::summarise(
      max_impact_score = max(threat_impact_score, na.rm = TRUE),
      total_impact_score = sum(
        severity_score + scope_score + timing_score,
        na.rm = TRUE
      ),
      n_threats = sum(threat_impact_score != 0, na.rm = TRUE),
      .groups = "drop"
    )
  
  # ---------- Species summary ----------
  species_summary <- Lv1_summary %>%
    dplyr::mutate(
      total_impact_score = tidyr::replace_na(total_impact_score, 0),
      threat_pressure_lv1 = max_impact_score / 9
    ) %>%
    dplyr::group_by(scientific_name, common) %>%
    dplyr::summarise(
      cumulative_threat_pressure = sum(threat_pressure_lv1, na.rm = TRUE),
      max_threat_pressure = max(threat_pressure_lv1, na.rm = TRUE),
      n_threat_lv1 = dplyr::n(),
      total_n_threats = sum(n_threats, na.rm = TRUE),
      total_score_lv1 = sum(total_impact_score, na.rm = TRUE),
      .groups = "drop"
    )
  
  # ---------- Return both ----------
  list(
    Lv1_summary = Lv1_summary,
    species_summary = species_summary
  )
}
