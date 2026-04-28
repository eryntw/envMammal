add_common <- function(df) {
  
  lookup <- c(
    "Amytornis whitei" = "Pilbara grasswren"
  )
  
  
  df %>%
    dplyr::mutate(
      common = dplyr::recode(search_term, !!!lookup, .default = common)
    )
}