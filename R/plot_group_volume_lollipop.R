#' Quick plot for a topic (or any group) volume lollipop chart
#'
#' @param df your data frame or tibble object
#' @param group_var the variable you wish to produce grouped volume for, e.g. topic, or name (if pivot_longer and not renaming)
#' @param project_name a string which will be pasted into the title, e.g. "project_name = Internal Project" gives "Internal Project - Topic Volume" as title.
#'
#' @return a ggplot object
#' @export
#'
plot_group_volume_lollipop <- function(df, group_var = name, project_name = "project"){

  df %>%
    dplyr::count({{group_var}}) %>%
    ggplot2::ggplot(ggplot2::aes(x = {{group_var}}, y = n, colour = {{group_var}}, fill = {{group_var}})) +
    ggplot2::geom_segment(ggplot2::aes(x = {{group_var}}, xend = {{group_var}}, y = 0, yend = n)) +
    ggplot2::geom_point(size = 3, shape = 21, show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = n, vjust = -1)) +
    HelpR::theme_microsoft_discrete() +
    ggplot2::labs(x = NULL, y = NULL, title = paste0(project_name, " - Topic Volume")) +
    ggplot2::theme(legend.position = "none",
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank())
}
