# =========================================================
# Plot COIN bar charts (static or interactive)
# =========================================================

#' Plot COIN bar charts (parent indicator or children)
#'
#' Wrapper around plot_bar() to visualise either:
#' 1. A parent aggregate indicator (e.g. Vulnerability, Exposure)
#' 2. All child indicators under a parent
#'
#' Supports both static ggplot output and interactive plotly output.
#'
#' @param coin A COIN object
#' @param iCode Indicator code (e.g. "Exposure", "Vulnerability")
#' @param mode Either "parent" or "children"
#' @param colours Optional vector of bar colours
#' @param ncol Number of columns for children layout (static only)
#' @param text.size Axis text size
#' @param interactive Logical; if TRUE returns plotly object
#'
#' @return A ggplot, patchwork, or plotly object
#' @export

plot_coin_bar <- function(
    coin,
    iCode,
    mode = c("parent", "children"),
    colours = NULL,
    ncol = 2,
    text.size = 4,
    interactive = FALSE
) {
  
  mode <- match.arg(mode)
  
  default_palette <- c(
    "darkslategrey", "lightpink", "royalblue3",
    "tan", "olivedrab", "gold", "red4"
  )
  
  # =====================================================
  # ---- PARENT ----
  # =====================================================
  
  if (mode == "parent") {
    
    if (is.null(colours)) {
      n <- length(na.omit(coin$Meta$Ind$iCode[coin$Meta$Ind$Parent == iCode]))
      if (n == 0) n <- 1
      colours_use <- default_palette[seq_len(min(n, length(default_palette)))]
    } else {
      colours_use <- colours
    }
    
    # ---- STATIC ----
    if (!interactive) {
      return(
        COINr::plot_bar(
          coin,
          dset = "Aggregated",
          iCode = iCode,
          uLabel = "uName",
          stack_children = TRUE,
          bar_colours = colours_use
        ) +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(
              angle = 90, vjust = 0.5, hjust = 1, size = text.size
            )
          )
      )
    }
    
    # ---- INTERACTIVE ----
    p_raw <- COINr::plot_bar(
      coin,
      dset = "Aggregated",
      iCode = iCode,
      uLabel = "uName",
      stack_children = TRUE,
      bar_colours = colours_use
    )
    
    df <- p_raw$data %>%
      dplyr::group_by(plbs) %>%
      dplyr::mutate(total_value = sum(.data[[iCode]], na.rm = TRUE)) %>%
      dplyr::ungroup()
    
    df$plbs <- reorder(df$plbs, -df$total_value)
    
    df$tooltip <- paste0(
      "Species: ", df$plbs,
      "<br>Component: ", df$Component,
      "<br>Value: ", round(df[[iCode]], 2)
    )
    
    p_clean <- ggplot2::ggplot(
      df,
      ggplot2::aes(
        x = plbs,
        y = .data[[iCode]],
        fill = Component,
        text = tooltip
      )
    ) +
      ggplot2::geom_col() +
      ggplot2::scale_fill_manual(values = colours_use) +
      ggplot2::theme_void() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          angle = 90, vjust = 0.5, hjust = 1, size = text.size
        )
      )
    
    return(plotly::ggplotly(p_clean, tooltip = "text"))
  }
  
  # =====================================================
  # ---- CHILDREN ----
  # =====================================================
  
  if (mode == "children") {
    
    iMeta <- coin$Meta$Ind
    child_codes <- na.omit(iMeta$iCode[iMeta$Parent == iCode])
    
    tbl <- coin$Data$Aggregated %>%
      dplyr::left_join(coin$Meta$Unit, by = "uCode") %>% 
      dplyr::select(dplyr::all_of(child_codes), "uName") %>%
      tidyr::pivot_longer(
        cols = child_codes,
        names_to = "code",
        values_to = "value"
      )
    
    # palette
    if (is.null(colours)) {
      colours_use <- default_palette[seq_len(min(length(child_codes), length(default_palette)))]
    } else {
      colours_use <- colours
    }
    
    # interactive add on
    
    tbl$tooltip <- paste0(
      "Species: ", tbl$uName,
      "<br>Indicator: ", tbl$code,
      "<br>Value: ", round(tbl$value, 2)
    )
    
    p <- ggplot(
      tbl,
      aes(
        x = tidytext::reorder_within(uName, value, code),
        y = value,
        fill = code,
        text = tooltip
      )
    ) +
      geom_col() +
      facet_wrap(~ code, ncol = ncol, scales = "free_x") +
      tidytext::scale_x_reordered() +
      scale_fill_manual(values = colours_use) +
      scale_y_continuous(
        limits = c(0, 100),
        expand = expansion(mult = c(0, 0.05))
      ) +
      theme_classic() +
      theme(
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none",
        aspect.ratio = 0.5,
        panel.spacing.y = unit(1, "lines") # spacing between faceted rows
      )
    
    pi <- plotly::ggplotly(p, tooltip = "text", width = 600, height = 300)
    
    # ---- STATIC ----
    if (!interactive) {
      return(p)
    } else{
      return(pi)
    }
  }
}