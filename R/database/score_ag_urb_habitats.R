#' Score breeding and feeding in agricultural and urban landscapes
#'
#' Assigns a score based on combinations of breeding and feeding in agricultural and urban habitats.
#'
#' Scoring rules:
#' 0 - Breed in both ag & urban AND feed in at least one, OR feed in both AND breed in at least one  
#' 1 - Breed in either ag or urban (but not both)  
#' 2 - Feed in either ag or urban, with no breeding in either  
#' 3 - Do not breed or feed in ag or urban
#'
#' @param df A data frame containing the four habitat columns.
#' @param breed_ag Column name for breeding in agricultural landscapes (default: "Breeding_habitat_Agricultural_lands_9")
#' @param breed_urb Column name for breeding in urban landscapes (default: "Breeding_habitat_Urban_9")
#' @param feed_ag Column name for feeding in agricultural landscapes (default: "Feeding_habitat_Agricultural_landscapes_9")
#' @param feed_urb Column name for feeding in urban landscapes (default: "Feeding_habitat_Urban_landscapes_9")
#'
#' @return Data frame with a new column `ag_urb_score` containing the scores 0–3.
#' @export
#' Score agricultural/urban habitat adaptation
#'
#' Calculates an adaptation score based on breeding and feeding
#' use of agricultural and urban habitats.
#'
#' Score:
#' 3 = strong adaptation (both feeding and breeding use high)
#' 2 = breeding use only
#' 1 = feeding use only
#' 0 = no anthropogenic habitat use
#'
#' @param df Data frame containing habitat columns
#' @param breed_ag Column name for breeding agricultural habitat
#' @param breed_urb Column name for breeding urban habitat
#' @param feed_ag Column name for feeding agricultural habitat
#' @param feed_urb Column name for feeding urban habitat
#'
#' @return Numeric vector of adaptation scores
#' @export

score_ag_urb_habitats <- function(
    df,
    breed_ag = "BreedingHabitatAgriculturalLands9",
    breed_urb = "BreedingHabitatUrban9",
    feed_ag = "FeedingHabitatAgriculturalLandscapes9",
    feed_urb = "FeedingHabitatUrbanLandscapes9"
) {
  
  # check columns
  all_cols <- c(breed_ag, breed_urb, feed_ag, feed_urb)
  missing_cols <- setdiff(all_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing columns in df: ", paste(missing_cols, collapse = ", "))
  }
  
  # pull vectors (VERY important: vectorised)
  breed_ag_v <- dplyr::coalesce(df[[breed_ag]], 0)
  breed_urb_v <- dplyr::coalesce(df[[breed_urb]], 0)
  feed_ag_v  <- dplyr::coalesce(df[[feed_ag]], 0)
  feed_urb_v <- dplyr::coalesce(df[[feed_urb]], 0)
  
  # intermediate sums
  breedadapt <- breed_ag_v + breed_urb_v
  feedadapt  <- feed_ag_v  + feed_urb_v
  
  # scoring
  score <- dplyr::case_when(
    (breedadapt + feedadapt) > 2 ~ 3,
    breedadapt > 0               ~ 2,
    feedadapt > 0                ~ 1,
    TRUE                         ~ 0
  )
  
  return(score)
}

