#' Compare density distributions across multiple data frames
#'
#' This function overlays density plots for selected columns across multiple
#' data frames. It ensures consistent y-axis scaling so no curves are truncated.
#'
#' @param df_list A named list of data frames (e.g., list(df1 = df1, df2 = df2))
#' @param cols A character vector of column names to plot
#' @param colours Optional vector of colours (length must match df_list)
#'
#' @return Produces density plots (one per column)
#'
#' @examples
#' plot_density_compare_multi(
#'   df_list = list(df_results = df_results, base_results = base_results),
#'   cols = group$iCode
#' )
#'
compare_density_multi <- function(df_list, cols, colours = NULL) {
  
  # ---- checks ----
  n_df <- length(df_list)
  if (n_df < 2) stop("Provide at least two data frames.")
  if (n_df > 5) warning("More than 5 data frames provided—plot may be cluttered.")
  
  # default colours
  if (is.null(colours)) {
    colours <- c("blue", "red", "darkgreen", "purple", "orange")[seq_len(n_df)]
  }
  
  # names for legend
  df_names <- names(df_list)
  if (is.null(df_names)) df_names <- paste0("df", seq_len(n_df))
  
  # ---- loop over columns ----
  for (col in cols) {
    
    # compute all densities first
    dens_list <- lapply(df_list, function(df) {
      density(df[[col]], na.rm = TRUE)
    })
    
    # shared y-axis
    ymax <- max(sapply(dens_list, function(d) max(d$y)))
    
    # shared x-axis (optional but recommended)
    xmin <- min(sapply(dens_list, function(d) min(d$x)))
    xmax <- max(sapply(dens_list, function(d) max(d$x)))
    
    # ---- plot first density ----
    plot(dens_list[[1]],
         main = col,
         col = colours[1],
         ylim = c(0, ymax),
         xlim = c(xmin, xmax))
    
    # ---- add remaining densities ----
    if (n_df > 1) {
      for (i in 2:n_df) {
        lines(dens_list[[i]], col = colours[i])
      }
    }
    
    # ---- legend ----
    legend("topright",
           legend = df_names,
           col = colours,
           lty = 1,
           cex = 0.8)
  }
}