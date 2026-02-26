build_sensitivity_groups <- function(iData){
  
  library(stringr)
  
  # ---- all candidate columns (exclude name columns and threats) ----
  cols <- names(iData)
  
  cols <- cols[!cols %in% c("uName", "uCode")]
  cols <- cols[!startsWith(cols, "t_")]   # keep threats out
  
  # ---------- Climate ----------
  climate <- cols[
    stringr::str_detect(cols,
                        "NormMin|NormMax|ElevationalRange|RlEoo|rec_")
  ]
  
  # ---------- Habitat ----------
  habitat <- cols[
    stringr::str_detect(cols,
                        "HB|Hb")
  ]
  
  # ---------- Diet ----------
  diet <- cols[
    stringr::str_detect(cols,
                        "Db|DB")
  ]
  
  # ---------- Adaptability ----------
  adaptability <- cols[
    stringr::str_detect(cols,
                        "anthro_")
  ]
  
  # ---------- Constraints ----------
  constraints <- cols[
    stringr::str_detect(cols,
                        "raptor|obligate_migrant|GenerationLength|bb_Rr")
  ]
  
  # ---- safety: remove duplicates ----
  used <- unique(c(climate, habitat, diet, adaptability, constraints))
  leftover <- setdiff(cols, used)
  
  if(length(leftover) > 0){
    message("Unclassified sensitivity indicators detected:\n",
            paste(leftover, collapse = ", "))
  }
  
  
  list(
    Climate = sort(unique(climate)),
    Habitat = sort(unique(habitat)),
    Diet = sort(unique(diet)),
    Adaptability = sort(unique(adaptability)),
    Constraints = sort(unique(constraints))
  )
}
