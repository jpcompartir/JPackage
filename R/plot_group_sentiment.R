#' Quickly plot group sentiment distributions as a perentage
#'
#' @param df data frame
#' @param group_var Grouping variable, e.g. country, topic, cluster
#' @param sentiment_var Sentiment variable (categorical)
#' @param type Whether the plot should be of volume or py percentage. Acccepts "percent" or "volume"
#' @param title The title of the plot, entered as a string.
#'
#' @return Ggplot stacked bar chart with x and y coords flipped
#' @export
#' @examples
#' \dontrun{
#' Example 1:
#' plot_group_sentiment(df, group_var = platform, sentiment_var = sentiment, type = "percent")
#'
#' Example 2:
#' plot_group_sentiment(df, group_var = topic, sentiment_var = sentiment, type = "volume")
#' }
plot_group_sentiment <- function(df, group_var = topic, sentiment_var = sentiment, type = "percent",
                                 title = "Grouped Sentiment Chart"){

  df <- df %>%
    dplyr::count({{group_var}}, {{sentiment_var}}) %>%
    dplyr::add_count({{group_var}}, wt = n, name = ".total") %>%
    dplyr::mutate(percent = n/.total * 100)
  if(type == "percent"){
     plot <- df %>%
      ggplot2::ggplot(ggplot2::aes(y = {{group_var}}, x = percent, fill = {{sentiment_var}}))+
      ggplot2::geom_col() +
      # ggplot2::geom_text(ggplot2::aes(label = paste0(round(percent, 1), "%")), vjust = 0.5, hjust = 0.5, size = 3, show.legend = FALSE)+
      HelpR::theme_microsoft_discrete() +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::labs(fill = NULL, y = NULL, x = "% of Posts",
                    title = title)
  } else if(type == "volume"){
    plot <- df %>%
      ggplot2::ggplot(ggplot2::aes(y = {{group_var}}, x = n, fill = {{sentiment_var}}))+
      ggplot2::geom_col() +
      # ggplot2::geom_text(ggplot2::aes(label = paste0(n),size = 3, show.legend = FALSE))+
      HelpR::theme_microsoft_discrete() +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::labs(fill = NULL, y = NULL, x = "Number of Posts",
                    title = title)
  }
  return(plot)


}

library(tidyverse)
ParseR::sprinklr_export %>%
  janitor::clean_names() %>%
  select(social_network, sentiment) %>%
  plot_group_sentiment(group_var = social_network, sentiment_var = sentiment, type = "volume")

ParseR::sprinklr_export %>%
  janitor::clean_names() %>%
  select(social_network, sentiment) %>%
  plot_group_sentiment(group_var = social_network, sentiment_var = sentiment, type = "percent")
