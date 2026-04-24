#' Plot Indicator Bar Chart with Maximum Reference Line
#'
#' Creates a horizontal bar plot for key indicators (e.g. Sensitivity,
#' Exposure, Pressure) with values plotted directly (stat = "identity").
#' A vertical dashed line indicates the maximum possible score.
#'
#' @param df A data.frame containing a single observation (e.g. one species)
#'   with indicator columns.
#' @param indicators Character vector of column names to plot.
#'   Default: c("Sensitivity", "Exposure", "Pressure").
#' @param max_value Numeric. The maximum possible value for each indicator.
#'   Default: 100.
#' @param title Optional plot title. If NULL, no title is shown.
#' @param label_digits Integer. Number of decimal places for value labels.
#'
#' @return A ggplot object showing a horizontal bar chart with value labels
#'   and a vertical reference line at the specified maximum.
#'
#' @details
#' Colours are mapped as follows:
#' - Exposure: darkslategrey
#' - Pressure: lightpink
#' - Sensitivity: royalblue3
#'
#' Values are displayed at the end of each bar, and the x-axis is extended
#' slightly beyond the maximum to avoid clipping labels.
#'
#' @examples
#' plot_indicator_bars(df)
#' plot_indicator_bars(df, title = "Species A")
#'
plot_indicator_bar <- function(df,
                               indicators = c("Sensitivity", "Exposure", "Pressure"),
                               max_value = 100,
                               label_digits = 1,
                               colour_mode = c("multi", "single"),
                               single_colour = "steelblue") {
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  
  colour_mode <- match.arg(colour_mode)
  
  if (length(indicators) > 7) {
    stop("Maximum of 7 indicators supported.")
  }
  
  # reshape data
  df_long <- df %>%
    select(all_of(indicators)) %>%
    pivot_longer(cols = everything(),
                 names_to = "indicator",
                 values_to = "value") %>%
    mutate(
      indicator = factor(indicator, levels = indicators),
      indicator = reorder(indicator, value)
    )
  
  # colour logic
  if (colour_mode == "multi") {
    
    base_cols <- c(
      "Sensitivity" = "royalblue3",
      "Exposure"    = "darkslategrey",
      "Pressure"    = "lightpink"
    )
    
    extra_needed <- setdiff(indicators, names(base_cols))
    
    extra_cols <- if (length(extra_needed) > 0) {
      setNames(hue_pal()(length(extra_needed)), extra_needed)
    } else {
      NULL
    }
    
    indicator_cols <- c(base_cols, extra_cols)
    indicator_cols <- indicator_cols[indicators]
    
    fill_scale <- scale_fill_manual(values = indicator_cols, guide = "none")
    
    fill_mapping <- aes(fill = indicator)
    
  } else {
    
    fill_scale <- NULL
    
    fill_mapping <- aes(fill = NULL)
  }
  
  # plot
  p <- ggplot(df_long, aes(x = value, y = indicator, fill = indicator)) +
    geom_col(width = 0.6) +
    geom_vline(xintercept = max_value,
               linewidth = 1.2,
               colour = "black") +
    geom_text(aes(label = round(value, label_digits)),
              hjust = 1.2,
              colour = "white",
              size = 4) +
    coord_cartesian(xlim = c(0, max_value * 1.1), clip = "off") +
    scale_y_discrete(expand = expansion(mult = c(0.02, 0.02))) +
    theme_minimal() +
    theme(aspect.ratio = 0.7,
          axis.text.y = element_text(size = 12),
          plot.margin = margin(0, 5, 5, 5)) +
    labs(x = NULL, y = NULL) +
    {
      if (colour_mode == "multi") {
        scale_fill_manual(values = indicator_cols, guide = "none")
      } else {
        scale_fill_manual(values = rep(single_colour, length(indicators)), guide = "none")
      }
    }
  
  if (!is.null(fill_scale)) {
    p <- p + fill_scale
  }
  
  return(p)
}