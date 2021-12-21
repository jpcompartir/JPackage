#' Scree plot for topic coherence analysis
#'
#' @param lda Latent Dirichlet Allocation object
#'
#' @return ggplot2 object
#' @export
#'
plot_k_coherence <- function(lda){
  coherence <- lda %>%
    tidyr::unnest(coherence) %>%
    dplyr::group_by(k) %>%
    dplyr::summarise(coherence = mean(coherence))
  coherence %>%
    ggplot2::ggplot(ggplot2::aes(x = k, y = coherence))+
    ggplot2::geom_line()+
    ggplot2::geom_point()+
    ggplot2::theme_light()+
    ggplot2::scale_x_continuous(breaks = unique(coherence$k))
}

