#' Function to view top terms associated with a categorical variable.
#'
#' Counts the top terms that appear in posts with a grouping variable.
#' Individual instances of grouping variable (e.g. Nike, Adidas) who have common words
#' in their top `n_terms` will be placed by graph-drawing algorithm closer to those they have none in common with.
#' Terms common to all or multiple grouping variables will be drawn towards the middle of the network.
#' Terms that are uniquely associated with a specific grouping variable will be draw outwards towards the periphery of the network.
#'
#' @param data Your data frame - should have text variable and a grouping variable coded in.
#' @param group_var The variable you wish to compare, should be a categorical or factor e.g. brand, entity, sentiment, country
#' @param text_var The text variable
#' @param n_terms Number of terms to select for each
#' @param with_ties Whether to allow for > `n_terms` if terms have equal frequency in `group_var`'s count
#'
#' @return A network visualisation of terms associated with a grouping variable
#' @export
#'
#' @examples
#' \dontrun{
#' Example 1:
#' plot_group_term_network(data, group_var = brand, text_var = text, n_terms = 15)
#'
#' Example 2:
#' plot_group_term_network(my_data_frame, group_var = sentiment, text_var = message, n_terms = 15)
#' }
plot_group_term_network <- function(data, group_var = brand, text_var = message, n_terms = 20,
                                    with_ties = FALSE){

  #Some tidy evaluate variables
  group_sym <- rlang::ensym(group_var)
  text_sym <- rlang::ensym(text_var)

  #Get tokens, count by group then slice n_terms tokens per group
  group_tokens <- data %>%
    tidytext::unnest_tokens(words, {{text_var}}) %>%
    dplyr::count({{group_var}}, words) %>%
    dplyr::group_by({{group_var}}) %>%
    dplyr::slice_max(order_by = n, n = n_terms, with_ties = with_ties) %>%
    dplyr::ungroup()

  #Make edges
  edges <- group_tokens %>% dplyr::rename(from = {{group_var}},
                                          to = 2)

  groups <- unique(edges$from)

  #Make a group - group set of rows, wherein the size is going to be large.
  group_frame <- tibble::tibble(from = groups, to = groups) %>%
    dplyr::mutate(size = 50)

  edges <- edges %>% dplyr::bind_rows(group_frame)

  nodes <- edges %>%
    dplyr::select(from, to) %>%
    tidyr::pivot_longer(cols = everything(), values_to = "node_name") %>%
    dplyr::distinct(node_name) %>%
    dplyr::mutate(size = ifelse(node_name %in% groups, 50, 1))

  colour_join <- edges %>% dplyr::select(-c(from, size))

  edges <- edges %>%
    dplyr::mutate(size = ifelse(to %in% groups, size, 0))

  nodes <- nodes %>%
    dplyr::left_join(colour_join, by = c("node_name" = "to")) %>%
    dplyr::group_by(node_name) %>%
    dplyr::mutate(n = mean(n, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(my_colour = ifelse(node_name %in% groups, "GROUP", "NOTGROUP")) %>%
    dplyr::distinct(node_name, .keep_all = TRUE) %>%
    dplyr::mutate(n = ifelse(is.na(n), 1, n),
                  n = ifelse(node_name %in% groups, max(n), n))

  tidygraph::tbl_graph(nodes = nodes, edges = edges) %>%
    ggraph::ggraph(layout = "nicely") +
    ggraph::geom_edge_link(edge_alpha = .5, edge_color = "grey70")+
    # ggraph::geom_edge_link(edge_alpha = .5, edge_color = "grey70",
    #                        aes(edge_width = n))+
    ggraph::geom_node_point(aes(size = size, colour = my_colour)) +
    shadowtext::geom_shadowtext(ggplot2::aes(label = node_name, x ,y),
                                size = 3, colour = "black", repel = TRUE,
                                bg.colour = "white", fontface = "bold",
                                vjust = 0, bg.r = 0.25)+
    ggplot2::theme_void() +
    ggplot2::scale_colour_manual(values = c("GROUP" =  "#fde725",
                                            "NOTGROUP" = "#21918c"), guide = "none") +
    # ggplot2::scale_colour_viridis_c(guide = "none")+
    ggraph::scale_edge_colour_viridis(guide = "none") +
    ggplot2::scale_size_continuous(aes(size = size), guide = "none", range = c(0, 10)) #+
  # ggraph::scale_edge_width(aes(size = n),range = c(0.5, 4), guide = "none")

}
