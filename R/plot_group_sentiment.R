#' Quickly plot group sentiment distributions as a perentage
#'
#' @param df data frame
#' @param group_var Grouping variable, e.g. country, topic, cluster
#' @param sentiment_var Sentiment variable (categorical)
#'
#' @return Ggplot stacked bar chart with x and y coords flipped
#' @export
#'
plot_group_sentiment <- function(df, group_var = topic, sentiment_var = sentiment){

  df %>%
    dplyr::count({{group_var}}, {{sentiment_var}}) %>%
    dplyr::add_count({{group_var}}, wt = n) %>%
    dplyr::mutate(percent = n/nn * 100) %>%
    ggplot2::ggplot(aes(y = {{group_var}}, x = percent, fill = {{sentiment_var}}))+
    ggplot2::geom_col() +
    HelpR::theme_microsoft_discrete() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(fill = NULL, x = NULL, y = "% of Posts")

}
