to_upper_camel <- function(col) {
  col %>%
    stringr::str_trim() %>%
    stringr::str_squish() %>%
    gsub("[^[:alnum:] ]", "", .) %>%
    stringr::str_to_title() %>%
    gsub(" ", "", .)
}