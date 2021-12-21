#' Plot intra-topic coherence bar charts
#'
#' @param lda Latent Dirichlet Allocation object
#' @param selected_k User-determined value of k (# of topics)
#' @param ... Additional arguments which are passed down at every level
#'
#' @return ggplot2 object
#' @export

plot_bars_coherence <- function(lda, selected_k, ...){

  lda %>%
    dplyr::filter(k == selected_k, ...)%>%
    tidyr::unnest(coherence, ...)%>%
    #extract the number of topics to later order the plots by, so topic_1 > topic_2 not topic_1 > topic_10 when k >= 10
    dplyr::mutate(number = as.numeric(stringr::str_extract(topic, "\\d+")))%>%
    ggplot2::ggplot(ggplot2::aes(x = reorder(topic, number), y = coherence, fill = topic, ...))+
    ggplot2::geom_col(...)+
    ggplot2::theme_light(...)+
    ggplot2::scale_fill_viridis_d(...)+
    ggplot2::theme(legend.position = "none",
                   axis.title = ggplot2::element_text(size = 14), ...)+
    ggplot2::labs(x = NULL, ...)

}

lda %>%
  tidyr::unnest(coherence) %>%
  dplyr::mutate(number = as.numeric(stringr::str_extract(topic, "\\d+")))%>%
  dplyr::select(number)
