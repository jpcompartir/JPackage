#' Quickly plot group sentiment distributions as a perentage
#'
#' @param df data frame
#' @param group_var Grouping variable, e.g. country, topic, cluster
#' @param sentiment_var Sentiment variable (categorical)
#' @param type Whether the plot should be of volume or py percentage. Accepts "percent" or "volume"
#' @param title The title of the plot, entered as a string.
#' @param bar_labels Whether to add the raw volume, percentage or neither to the bars
#'
#' @return Ggplot stacked bar chart with x and y coords flipped
#' @export
#' @examples
#' \dontrun{
#' # Example 1:
#' plot_group_sentiment(df, group_var = platform, sentiment_var = sentiment, type = "percent")
#'
#' # Example 2:
#' plot_group_sentiment(df, group_var = topic, sentiment_var = sentiment, type = "volume")
#' }
plot_group_sentiment <- function(df,
                                 group_var = topic,
                                 sentiment_var = sentiment,
                                 type = c("percent", "volume"),
                                 title = "Grouped Sentiment Chart", bar_labels = c("none", "percent", "volume")) {
  group_sym <- rlang::ensym(group_var)
  bar_labels <- match.arg(if (missing(bar_labels)) "volume" else bar_labels, c("none", "percent", "volume"))
  type <- match.arg(if (missing(type)) "percent" else type, c("percent", "volume"))

  df <- df %>%
    dplyr::count({{ group_var }}, {{ sentiment_var }}) %>%
    dplyr::add_count({{ group_var }}, wt = n, name = ".total") %>%
    dplyr::mutate(
      percent = n / .total * 100,
      percent_character = paste0(round(percent, digits = 1), "%")
    )

  if (type == "percent") {
    plot <- df %>%
      ggplot2::ggplot(ggplot2::aes(x = reorder(!!group_sym, n), y = percent, fill = {{ sentiment_var }})) +
      ggplot2::geom_col() +
      ggplot2::labs(
        fill = NULL, y = NULL, x = "% of Posts",
        title = title
      )
  } else if (type == "volume") {
    plot <- df %>%
      ggplot2::ggplot(ggplot2::aes(x = reorder(!!group_sym, n), y = n, fill = {{ sentiment_var }})) +
      ggplot2::geom_col() +
      ggplot2::labs(
        fill = NULL, y = NULL, x = "Number of Posts",
        title = title
      )
  }

  plot <- plot +
    HelpR::theme_microsoft_discrete() +
    ggplot2::coord_flip() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    ggplot2::scale_y_discrete(expand = c(0, 0))

  if (bar_labels == "percent") {
    plot <- plot +
      ggplot2::geom_text(ggplot2::aes(label = percent_character),
        colour = "white",
        position = ggplot2::position_stack(0.5),
        check_overlap = TRUE
      )
  }
  if (bar_labels == "volume") {
    plot <- plot +
      ggplot2::geom_text(ggplot2::aes(label = scales::comma(n)),
        colour = "white",
        position = ggplot2::position_stack(0.5),
        check_overlap = TRUE
      )
  }
  return(plot)
}
