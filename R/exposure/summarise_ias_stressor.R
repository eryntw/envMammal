#' Summarise IAS herbivore and predator stressor per species
#'
#' Aggregates (timing + scope + severity) per threat record,
#' then summarises per species using either sum or max.
#'
#' @param scored_threat Dataframe containing:
#'   scientific_name, threat_ias,
#'   timing_score, scope_score, severity_score
#'
#' @param agg_method Character:
#'   "sum" = cumulative IAS stressor per species (default)
#'   "max" = strongest single IAS threat per species
#'
#' @return Dataframe with:
#'   scientific_name
#'   ias_herb_sum
#'   ias_pred_sum
#'
#' @export
#' 
summarise_ias_stressor <- function(scored_threat,
                                   agg_method = c("sum", "max")) {
  
  agg_method <- match.arg(agg_method)
  
  ## Threat species list ------
  
  threat_species <- list(
    
    plants = list(
      native = c(
        # none in your list
      ),
      invasive = c(
        "Mimosa pigra",          # Giant sensitive plant
        "Salvinia molesta",      # Giant salvinia
        "Spartina alterniflora", # Smooth cordgrass
        "Spartina maritima",     # Small cordgrass
        "Unspecified Spartina",
        "Nassella trichotoma"    # Serrated tussock
      )
    ),
    
    predators = list(
      native = c(
        "Unspecified Varanus",         # Monitor lizards
        "Canis familiaris ssp. dingo"  # Dingo (native/long-established)
      ),
      invasive = c(
        "Vulpes vulpes",        # Red fox
        "Felis catus",          # Feral cat
        "Rattus rattus",        # Black rat
        "Canis familiaris",     # Feral dog
        "Sus scrofa",           # Feral pig (omnivore but major predator)
        "Clostridium botulinum" # Pathogen
      )
    ),
    
    herbivores = list(
      native = c(
        "Osphranter robustus",  # Common wallaroo
        "Unspecified Macropus",
        "Petaurus breviceps",   # Sugar glider
        "Manorina flavigula",   # Yellow-throated miner (nectarivore)
        "Neochmia temporalis"   # Red-browed finch (granivore)
      ),
      invasive = c(
        "Oryctolagus cuniculus", # European rabbit
        "Capra hircus",          # Goat
        "Ovis aries",            # Sheep
        "Bos taurus",            # Cattle
        "Equus caballus",        # Horse
        "Bubalus bubalis",       # Water buffalo
        "Dama dama",             # Fallow deer
        "Unspecified CERVIDAE",
        "Cyprinus carpio",       # Common carp
        "Anser caerulescens",    # Snow goose
        "Anser rossii"           # Ross's goose
      )
    )
  )
  
  ## Function ----
  
  herb_inv <- threat_species$herbivores$invasive
  pred_inv <- threat_species$predators$invasive
  
  ## Row-level sum ----
  
  scored_threat <- scored_threat %>%
    dplyr::mutate(
      threat_ias = trimws(threat_ias),
      row_sum = timing_score + scope_score + severity_score
    )
  
  ## Choose species-level aggregation function ----
  
  agg_fun <- switch(
    agg_method,
    sum = function(x) sum(x, na.rm = TRUE),
    max = function(x) max(x, na.rm = TRUE)
  )
  
  ## Aggregate per species ----
  
  out <- scored_threat %>%
    dplyr::filter(threat_ias %in% c(herb_inv, pred_inv)) %>%
    dplyr::mutate(
      group = dplyr::case_when(
        threat_ias %in% herb_inv ~ "herb",
        threat_ias %in% pred_inv ~ "pred"
      )
    ) %>%
    dplyr::group_by(scientific_name, group) %>%
    dplyr::summarise(score = agg_fun(row_sum),
                     .groups = "drop") %>%
    tidyr::pivot_wider(
      names_from = group,
      values_from = score,
      values_fill = 0
    )
    
    ## Dynamic renaming based on agg_method ----
  
  out <- out %>%
    dplyr::rename_with(~ paste0("ias_", .x, "_", agg_method),
                       .cols = c("herb", "pred"))
  
  return(out)
}