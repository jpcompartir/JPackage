#' Quick re-work of function ahead of going into work packages
#'
#' @param df Data Frame or Tibble object
#' @param topic_var The Grouping variable
#' @param text_var The text variable
#' @param top_n How many terms per group to plot, arranged by weighted-log odds ratio
#' @param nrow How many rows to show the plot on
#' @param top_terms_cutoff How many of the top-frequency terms to select top_n from
#'
#' @return list with viz and view objects
#' @export
#'
my_wlos_slice <- function(df, topic_var, text_var = Message, top_n = 30, nrow = 4,
                          top_terms_cutoff = 1500){

  text_var <- rlang::enquo(text_var)
  topic_var <- rlang::enquo(topic_var)

  wlos <- df %>%
    tidytext::unnest_tokens(word, !! text_var) %>%
    dplyr::group_by(!! topic_var) %>%
    dplyr::count(word, sort = TRUE) %>%
    dplyr::ungroup() %>%
    tidylo::bind_log_odds(set = !! topic_var, feature = word, n = n)

  viz <- wlos %>%
    dplyr::slice_max(order_by = n, n = top_terms_cutoff) %>%
    dplyr::group_by(!! topic_var) %>%
    dplyr::top_n(n = top_n, wt = log_odds_weighted) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = n, y = log_odds_weighted, label = word)) +
    ggplot2::geom_hline(yintercept = 0, lty = 2,
                        color = "gray50", alpha = 0.5, size = 1.2) +
    ggrepel::geom_text_repel(size = 3, segment.size = 0.5, color = 'black') +
    ggplot2::geom_point(size = .4, show.legend = F) +
    ggplot2::facet_wrap(c(topic_var), nrow = nrow, scales = "free") +
    ggplot2::scale_x_log10() +
    ggplot2::labs(x = "Word frequency",
                  y = "Log odds ratio, weighted by uninformative Dirichlet prior") +
    ggplot2::theme(strip.background =element_rect(fill="gray"))+
    ggplot2::theme_minimal() +
    ggplot2::theme(strip.background = element_rect(fill = "gray"))

  list("viz" = viz, "view" = wlos)

}
