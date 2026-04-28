#' Generate bird trait attributes
#'
#' Adds binary trait scores for migratory status and raptor status.
#'
#' @param df A data.frame containing columns `bl_Family` and `bl_MigratoryStatus`
#'
#' @return A data.frame with added columns:
#' \itemize{
#'   \item obligate_migrant (0/1)
#'   \item raptor (0/1)
#' }
#' @export

get_birdlife_attr <- function(df) {
  
  required_cols <- c("Family", "MigratoryStatus")
  
  if (!all(required_cols %in% names(df))) {
    stop("Input data must contain columns: ", 
         paste(required_cols, collapse = ", "))
  }
  
  raptor_families <- c(
    "Barn-owls",
    "Typical Owls",
    "Hawks, Eagles",
    "Kites",
    "Falcons, Caracaras"
  )
  
  df %>%
    dplyr::mutate(
      
      # Obligatory migrant (1 = full migrant)
      obligate_migrant = dplyr::if_else(
        MigratoryStatus == "Full migrant",
        1L, 0L
      ),
      
      # Raptor family
      is_raptor = as.integer(Family %in% raptor_families)
      
    )
}
