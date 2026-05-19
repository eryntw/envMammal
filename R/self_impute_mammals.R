self_impute_mammals <- function(df) {
  
  df <- df %>% 
    
    ## Constraints ------
    
    fill_column_by_rule("gestation_day_mean",
                        pilot_subset %>% select(contains("gestation")) %>% names(),
                        rule = "mean") %>% 
    
    fill_column_by_rule("weaning_day_mean",
                        pilot_subset %>%
                          select(matches("(?i)(wean).*(age|day|_d$)")) %>% 
                          names(),
                        rule = "mean") %>% 
    
    fill_column_by_rule("female_maturity_day_mean",
                        pilot_subset %>%
                          select(matches("(?i)(female).*(maturity).*(age|day|_d$)")) %>% 
                          names(),
                        rule = "mean") %>%
    
    fill_column_by_rule("male_maturity_day_mean",
                        pilot_subset %>%
                          select(matches("(?i)(male).*(maturity).*(age|day|_d$)")) %>% 
                          names(),
                        rule = "mean") %>%
    
    fill_column_by_rule("littersize_mean",
                        pilot_subset %>%
                          select(contains("litter")) %>%
                          select(-matches("(?i)(interval|per)")) %>% 
                          names(),
                        rule = "mean") %>% 
    
    fill_column_by_rule("litterperyear_mean",
                        pilot_subset %>%
                          select(matches("(?i)(litter).*(per|interval)")) %>%
                          names(),
                        rule = "mean") %>%
    
    fill_column_by_rule("maxlongevity_mean",
                        pilot_subset %>%
                          select(matches("(?i)(longevity).*(_y|Yr)"))%>%
                          names(),
                        rule = "mean") %>% 
    
    fill_column_by_rule("bodymass_mean",
                        pilot_subset %>%
                          select(matches("(?i)(adult).*(mass)")) %>%
                          names(),
                        rule = "mean")
  
  return(df)
  
}
