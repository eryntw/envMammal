map_exposure_cols <- function(df) {
  
  df %>%
    
    dplyr::mutate(
      
      ## Arid zone vegetation Breeding (proportion) ----
      dplyr::across(
        .cols = dplyr::matches("^aub_BreedingHabitat.*9$"),
        .fns  = ~ if_else(is.na(aub_BreedingHB) | aub_BreedingHB == 0, 0, .x / aub_BreedingHB)
      ),
      
      ## Nesting substrate proportions ------
      NestSbs_sum = rowSums(dplyr::across(dplyr::matches("^bbn_NestSbs|^bbn_NestTypeBu$")), na.rm = TRUE),
      
      dplyr::across(
        .cols = dplyr::matches("^bbn_NestSbs|^bbn_NestTypeBu$"),
        .fns  = ~ if_else(NestSbs_sum == 0, 0, .x / NestSbs_sum)
      ),
      
      ## Foraging strategy (divide by 100 if not 0) ------
      dplyr::across(
        .cols = dplyr::matches("^elt_ForStrat.*|^elt_PelagicSpecialist$"),
        .fns  = ~ if_else(is.na(.x) | .x == 0, 0, .x / 100)
      ),
      
      ## Nesting structure (binary) ------
      Open = if_else(rowSums(dplyr::across(dplyr::matches("bbn_NestType(Cp|Hc|No|O|Pl|Sa|Sc)"))) > 0, 1, 0),
      Cavity = if_else(rowSums(dplyr::across(dplyr::matches("bbn_NestType(Bu|Cr|Cv)"))) > 0, 1, 0),
      Enclosed = if_else(rowSums(dplyr::across(dplyr::matches("bbn_NestType(Dm|Pn|Sp|M)"))) > 0, 1, 0),
      NotNestingAustralia = if_else(aub_NonBreedingOnly4 == 1, 1, 0),
      
      ## Territoriality mapping ------
      Territoriality = dplyr::case_when(
        bhv_Territoriality == "strong" | bhv_Territoriality == "yes" ~ 1,
        bhv_Territoriality %in% c("weak", "none", "no") ~ 0,
        TRUE ~ NA_real_
      ),
      
      ## Body size categorisation ------
      BodySize = dplyr::case_when(
        bb_AverageMass < 60 ~ "Small",
        bb_AverageMass >= 60 & bb_AverageMass <= 300 ~ "Medium",
        bb_AverageMass > 300 & bb_AverageMass <= 3400 ~ "Large",
        bb_AverageMass > 3400 ~ "VeryLarge",
        TRUE ~ NA_character_
      ),
      
      ## Nectar diet proportion ------
      NectarProp = if_else(is.na(bb_Db) | bb_Db == 0, 0, bb_NeWt / bb_Db)
      
    ) %>%
    
    # remove temporary column
    dplyr::select(-NestSbs_sum)
}