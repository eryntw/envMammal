# ============================================================
# assign_iNames.R
# Assign descriptive names to iMeta
# ============================================================

#' Assign Descriptive iNames to an iMeta Table
#'
#' Replaces \code{iName} values in a COINr-compatible \code{iMeta}
#' table using a named character vector lookup.
#'
#' @param iMeta A COINr-compatible \code{data.frame}.
#'
#' @param name_lookup Named character vector where:
#'   names = existing \code{iName} (or \code{iCode})
#'   values = descriptive labels.
#'
#' @return Modified \code{iMeta} with updated \code{iName}.
#'
#' @export
#' 
bird_name_lookup <- c(
  
  # -----------------------------------------------------------
  # Elevation & Range
  # -----------------------------------------------------------
  bb_ElevationalRange = "Elevational range",
  bb_NormMax = "Max elevation",
  bb_NormMin = "Min elevation",
  bl_RlEooSmallerOfBreedingAndNonBreedingEoo = "Min seasonal EOO",
  
  # -----------------------------------------------------------
  # Mean annual aridity (ADM)
  # -----------------------------------------------------------
  rec_ADM_q10 = "Aridity 10%",
  rec_ADM_q90 = "Aridity 90%",
  rec_ADM_range_90_10 = "Aridity range 90–10",
  
  # -----------------------------------------------------------
  # Elevation (DEM)
  # -----------------------------------------------------------
  `rec_dem-9s_q10` = "Elevation 10%",
  `rec_dem-9s_q90` = "Elevation 90%",
  `rec_dem-9s_range_90_10` = "Elevation range 90–10",
  
  # -----------------------------------------------------------
  # Geomorphic / Environmental diversity
  # -----------------------------------------------------------
  rec_geom_90M_s10e110_simpson = "Geomorphon simpson",
  rec_stern_dehoedt_2000_minor_simpson = "Climate simpson",
  
  # -----------------------------------------------------------
  # Precipitation (PTI, PTX, PTS1)
  # -----------------------------------------------------------
  rec_PTI_q10 = "Min monthly precip 10%",
  rec_PTI_q90 = "Min monthly precip 90%",
  rec_PTI_range_90_10 = "Min monthly precip range 90–10",
  
  rec_PTS1_q10 = "Seasonal precip 10%",
  rec_PTS1_q90 = "Seasonal precip 90%",
  rec_PTS1_range_90_10 = "Seasonal precip range 90–10",
  
  rec_PTX_q10 = "Max monthly precip 10%",
  rec_PTX_q90 = "Max monthly precip 90%",
  rec_PTX_range_90_10 = "Max monthly precip range 90–10",
  
  # -----------------------------------------------------------
  # Temperature (TXI, TXX)
  # -----------------------------------------------------------
  rec_TXI_q10 = "Min monthly temp 10%",
  rec_TXI_q90 = "Min monthly temp 90%",
  rec_TXI_range_90_10 = "Min monthly temp range 90–10",
  
  rec_TXX_q10 = "Max monthly temp 10%",
  rec_TXX_q90 = "Max monthly temp 90%",
  rec_TXX_range_90_10 = "Max monthly temp range 90–10",
  
  # -----------------------------------------------------------
  # Habitat breadth
  # -----------------------------------------------------------
  aub_BreedingHB = "Breeding habitat breadth",
  aub_FeedingHB = "Feeding habitat breadth",
  bb_Hb = "Habitat breadth",
  bl_HB_L1 = "Habitat breadth L1",
  bl_score_HB_L2 = "Habitat breadth L2",
  
  # -----------------------------------------------------------
  # Diet breadth
  # -----------------------------------------------------------
  bb_Db = "Diet breadth",
  bb_Db_simpson = "Diet Simpson",
  
  # -----------------------------------------------------------
  # Anthropogenic exposure
  # -----------------------------------------------------------
  aub_score_anthro_habitat = "Anthropogenic score",
  bl_prop_anthro_max = "Anthropogenic max",
  bl_prop_anthro_total = "Anthropogenic total",
  
  # -----------------------------------------------------------
  # Migration / Life history
  # -----------------------------------------------------------
  aub_obligate_migrant = "Obligate migrant",
  bb_obligate_migrant = "Obligate migrant",
  bl_obligate_migrant = "Obligate migrant",
  
  bb_Rr = "Reproductive rate",
  bl_GenerationLength = "Generation length",
  bl_is_raptor = "Raptor",
  
  # -----------------------------------------------------------
  # Threat metrics
  # -----------------------------------------------------------
  t_trend_score = "Trend score",
  t_TrendxStatus = "Trend × Status",
  t_status_score = "Status score",
  
  t_score_sum_sp = "Threat sum",
  t_score_max_sp = "Threat max",
  t_n_lv1 = "n Level 1 threats",
  t_n_lv2_total = "n Level 2 threats"
)

assign_iNames <- function(iMeta,
                          name_lookup = bird_name_lookup) {
  
  if (!"iName" %in% base::names(iMeta)) {
    base::stop("iMeta must contain column 'iName'.")
  }
  
  match_idx <- base::match(iMeta$iName,
                           base::names(name_lookup))
  
  replace <- !base::is.na(match_idx)
  
  iMeta$iName[replace] <- name_lookup[match_idx[replace]]
  
  return(iMeta)
}