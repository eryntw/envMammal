build_threat_groups <- function(iData){
  
  t_cols <- names(iData)[startsWith(names(iData), "t_")]
  
  list <- list(
    Trend  = t_cols[stringr::str_detect(t_cols, "trend|TrendxStatus")],
    Status = t_cols[stringr::str_detect(t_cols, "status")]
  )
  list$Threat <-  setdiff(t_cols,
                          c(list$Trend, list$Status))
  return(list)
}
