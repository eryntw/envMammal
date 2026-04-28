#' Plot COIN indicator hierarchy as a dendrogram
#'
#' This function extracts the indicator hierarchy from a COIN object
#' (via coin$Meta$Ind) and visualises it as a dendrogram using ggraph.
#'
#' @param df A dataframe contains iCode, Parent, iName, and Percentage columns.
#' @param circular Logical; whether to plot circular dendrogram (default = FALSE).
#' @param leaf_angle Angle for leaf node labels (default = 90).
#' @param leaf_size Font size for leaf labels (default = 2.5).
#' @param internal_size Font size for internal node labels (default = 3).
#' @param root_size Font size for root node label (default = 4).
#'
#' @return A ggraph plot object
#' @export
#'
#' @examples
#' plot_coin_hierarchy(coin)
plot_coin_hierarchy <- function(
    df,
    circular = FALSE,
    leaf_size = 5,
    internal_size = 6,
    root_size = 6
) {
  
  # ---- Load required packages ----
  require(dplyr)
  require(igraph)
  require(ggraph)
  require(tidygraph)
  
  # ---- Extract hierarchy ----
  df <- df %>% 
    dplyr::select(Parent, iName, Percentage) %>% 
    stats::na.omit()
  
  # ---- Create graph ----
  mygraph <- igraph::graph_from_data_frame(df)
  
  # ---- Plot ----
  p <- ggraph::ggraph(mygraph, 
                      layout = "dendrogram", 
                      circular = circular) + 
    
    # edges
    ggraph::geom_edge_elbow(aes(width = Percentage, color = Percentage), 
                            strength = 1) +
    ggraph::scale_edge_width(range = c(0.1, 2)) +
    
    # nodes
    ggraph::geom_node_point() +
    
    # ---- leaf nodes ----
  ggraph::geom_node_text(
    ggplot2::aes(label = name, filter = leaf),
    hjust = 1,
    nudge_y = -0.05,
    size = leaf_size
  ) +
    
    # ---- internal nodes ----
  ggraph::geom_node_text(
    ggplot2::aes(label = name, filter = !leaf & !tidygraph::node_is_root()),
    hjust = 0.5,
    vjust = -0.5,
    nudge_y = 0.4,
    size = internal_size
  ) +
    
    # ---- root node ----
  ggraph::geom_node_text(
    ggplot2::aes(label = name, filter = tidygraph::node_is_root()),
    size = root_size,
    vjust = -1,
    fontface = "bold"
  ) +
    
    # spacing
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.2, 0.2))) +
    
    #flip
    
    ggplot2::coord_flip(clip = "off")+
    
    # theme
    ggraph::theme_graph(background = "white") +
    
    # legend
    ggplot2::guides(edge_colour = guide_legend(), edge_width = guide_legend()) +
    ggplot2::theme(
      legend.position = c(0.9, 0.1),
      legend.justification = c(1, 0),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 14)
    )
  
  return(p)
}
