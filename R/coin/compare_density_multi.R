#' Compare density distributions across multiple data frames (ggplot list)
#'
#' @param df_list Named list of data frames
#' @param cols Character vector of columns to plot
#' @param colours Optional vector of colours
#'
#' @return A named list of ggplot objects
#' @export

compare_density_multi <- function(df_list, cols, colours = NULL) {
  
  library(dplyr)
  library(ggplot2)
  library(purrr)
  
  n_df <- length(df_list)
  if (n_df < 2) stop("Provide at least two data frames.")
  
  # ---- dataset names ----
  df_names <- names(df_list)
  if (is.null(df_names)) df_names <- paste0("df", seq_len(n_df))
  
  # ---- colours ----
  if (is.null(colours)) {
    colours <- c(
      "darkslategrey", "lightpink", "royalblue3",
      "tan", "olivedrab", "gold", "red4"
    )[seq_len(n_df)]
  }
  
  linetypes <- seq_len(n_df)+1
  
  # ---- build plots ----
  plots <- purrr::map(cols, function(col) {
    
    # combine data for this variable only
    df_long <- purrr::imap_dfr(df_list, function(df, nm) {
      tibble::tibble(
        value = df[[col]],
        dataset = nm
      )
    }) %>%
      dplyr::filter(!is.na(value))
    
    # compute shared limits
    dens_list <- split(df_long$value, df_long$dataset) %>%
      lapply(density, na.rm = TRUE)
    
    ymax <- max(sapply(dens_list, function(d) max(d$y)))
    xmin <- min(sapply(dens_list, function(d) min(d$x)))
    xmax <- max(sapply(dens_list, function(d) max(d$x)))
    
    # plot
    ggplot(df_long, aes(x = value, 
                        colour = dataset, 
                        fill = dataset, 
                        linetype = dataset)) +
      geom_density(alpha = 0.1, linewidth = 1) +
      scale_colour_manual(values = colours) +
      scale_fill_manual(values = colours) +
      scale_linetype_manual(values = linetypes) +
      coord_cartesian(xlim = c(0, 100), ylim = c(0, ymax)) +
      labs(
        title = col,
        x = col,
        y = "Density"
      ) +
      theme_minimal() +
      theme(
        legend.position = "top",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank()
      )
  })
  
  names(plots) <- cols
  
  return(plots)
}
