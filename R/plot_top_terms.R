#' Make faceted lollipop charts for top terms from a cluster
#'
#' @param top_terms_table the output of make_top_terms_table()
#' @param words_var the name of the variable in which word tokens have been saved
#' @param group_var cluster / topic / grouping variable
#' @param top_n number of terms per plot
#' @param nrow number of rows to display plots on
#'
#' @return faceted lollipops
#' @export
#'
top_terms_plot <- function(top_terms_table, words_var = words, group_var = cluster_name, top_n = 15, nrow = 2){

  words_quo <- rlang::enquo(words_var)
  words_sym <- rlang::ensym(words_var)

  group_quo <- rlang::enquo(group_var)
  group_sym <- rlang::ensym(group_var)

  top_terms_table %>%
    dplyr::group_by({{group_var}}) %>%
    dplyr::slice_max(order_by = n, n = top_n, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(!! words_quo := tidytext::reorder_within(!! words_sym, n, !!group_sym)) %>%
    ggplot2::ggplot(aes(x= {{words_var}} , y = n, fill = !! group_sym,color = !! group_sym)) +
    # geom_col(position = "dodge", show.legend = FALSE)+
    ggplot2::geom_segment(aes(x = {{words_var}}, xend = {{words_var}},
                              y = 0, yend = n),
                          show.legend = FALSE) +
    ggplot2::geom_point(size = 3,
                        shape = 21,
                        show.legend = FALSE) +
    ggplot2::coord_flip() +
    HelpR::theme_microsoft_discrete() +
    ggplot2::facet_wrap(ggplot2::vars(!!group_sym),
                        scales = "free",
                        nrow = nrow)+
    ggplot2::theme_minimal()+
    tidytext::scale_x_reordered("Term") +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 12))+
    ggplot2::labs(x = NULL,
                  y = "Term Frequency")
}
