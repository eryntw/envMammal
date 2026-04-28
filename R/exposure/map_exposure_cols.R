map_exposure_cols <- function(df) {
  
  df %>%
    
    dplyr::mutate(
      
      ## ---- Breeding vegetation (proportion) ----
      dplyr::across(
        .cols = dplyr::matches("^aub_BreedingHabitat.*9$"),
        .fns  = ~ if_else(is.na(aub_BreedingHB) | aub_BreedingHB == 0, 0, .x / aub_BreedingHB),
        .names = "exp_{.col}"
      ),
      exp_EucalyptusWoodland = exp_aub_BreedingHabitatTemperateDrySclerophyllForestAndWoodland9 +
        exp_aub_BreedingHabitatTropicalSavannaWoodland9,
      
      ## ---- Nesting substrate (proportion) ----
      NestSbs_sum = rowSums(dplyr::across(dplyr::matches("^bbn_NestSbs|^bbn_NestTypeBu$")), na.rm = TRUE),
      
      dplyr::across(
        .cols = dplyr::matches("^bbn_NestSbs(T|S|Z|G|R)$|^bbn_NestTypeBu$"),
        .fns  = ~ if_else(NestSbs_sum == 0, 0, .x / NestSbs_sum),
        .names = "exp_{.col}"
      ),
      
      ## ---- Foraging strategy (proportion) ----
      dplyr::across(
        .cols = dplyr::matches("^elt_ForStrat.*|^elt_PelagicSpecialist$"),
        .fns  = ~ if_else(is.na(.x) | .x == 0, 0, .x / 100),
        .names = "exp_{.col}"
      ),
      
      ## ---- Migratory ----
      exp_longdistmig = if_else(bb_Mig == 1, 1, 0),
      exp_partmig = if_else(bb_Mig == 2, 1, 0),
      exp_othermig = if_else(bb_Mig == 0, 1, 0),
      
      ## ---- Nesting structure ----
      exp_Open = if_else(rowSums(dplyr::across(dplyr::matches("bbn_NestType(Cp|Hc|No|O|Pl|Sa|Sc)"))) > 0, 1, 0),
      exp_Cavity = if_else(rowSums(dplyr::across(dplyr::matches("bbn_NestType(Bu|Cr|Cv)"))) > 0, 1, 0),
      exp_Enclosed = if_else(rowSums(dplyr::across(dplyr::matches("bbn_NestType(Dm|Pn|Sp|M)"))) > 0, 1, 0),
      exp_NotNestingAustralia = if_else(aub_NonBreedingOnly4 == 1, 1, 0),
      
      ## ---- Territoriality ----
      exp_Territorial_Strong = if_else(bhv_Territoriality == "strong", 1, 0),
      exp_Territorial_WeakNone = if_else(bhv_Territoriality %in% c("weak", "none"), 1, 0),
      
      ## ---- Body size ----
      exp_Bodysize_small = if_else(bb_AverageMass < 60, 1, 0),
      exp_Bodysize_medium = if_else(bb_AverageMass >= 60 & bb_AverageMass <= 300, 1, 0),
      exp_Bodysize_large = if_else(bb_AverageMass > 300 & bb_AverageMass <= 3400, 1, 0),
      exp_Bodysize_veryLarge = if_else(bb_AverageMass > 3400, 1, 0),
      
      ## ---- Threat classes ----
      dplyr::across(
        matches("^threat_.*_score$"),
        list(
          no_impact = ~ if_else(.x == 0, 1, 0),
          negligible = ~ if_else(.x > 0 & .x < 3, 1, 0),
          low = ~ if_else(.x >= 3 & .x < 6, 1, 0),
          medium = ~ if_else(.x >= 6 & .x < 8, 1, 0),
          high = ~ if_else(.x > 7, 1, 0)
        ),
        .names = "exp_{.col}_{.fn}"
      ),
      
      ## ---- HWI (ratio) ----
      exp_nHWI = 1 / avo_HandWingIndex,
      exp_HWI = avo_HandWingIndex / 100,
      
      ## ---- Activity pattern ----
      exp_Nocturnal = if_else(elt_Nocturnal == 1, 1, 0),
      exp_Diurnal = if_else(elt_Nocturnal != 1, 1, 0),
      exp_Crepuscular = if_else(elt_Nocturnal != 1, 1, 0),
      
      ## ---- Nectar diet (proportion) ----
      exp_NectarProp = if_else(bb_NeWt == 0, 0, bb_NeWt/10)
    ) %>%
    
    dplyr::select(-NestSbs_sum, 
                  -exp_aub_BreedingHabitatTemperateDrySclerophyllForestAndWoodland9,
                  -exp_aub_BreedingHabitatTropicalSavannaWoodland9
                  )
}