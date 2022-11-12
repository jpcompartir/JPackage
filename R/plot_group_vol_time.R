#' Quickly plot faceted volume of groups over time
#'
#' @param df Data frame or tibble
#' @param group_var grouping variable e.g. country, cluster, topic etc.
#' @param date_var Variable which contains date information (can be datetime too I think)
#' @param unit A single unit of time fed into lubridate::floor_date  "week", "day", "month","quarter", "year"
#' @param nrow How many rows the plot should be shown in
#'
#' @return ggplot object of facetted bar charts
#' @export
plot_group_vol_time <- function(df, group_var = group, date_var = date, unit = c("day", "week", "month", "quarter", "year"), nrow = 2){

  unit <- match.arg(unit)

  date_sym <- rlang::ensym(date_var)
  group_sym <- rlang::ensym(group_var)

  df <- df %>% dplyr::mutate(plot_date = lubridate::floor_date(!!date_sym, unit = unit),
                             facet_var = !!group_sym)

  df %>%
    dplyr::count(plot_date, facet_var) %>%
    ggplot2::ggplot(ggplot2::aes(x = plot_date, y = n, fill = facet_var)) +
    ggplot2::geom_col() +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_date(date_breaks = "1 months", date_labels = "%d-%b") +
    ggplot2::theme(legend.position = "none",
                   axis.text.x = element_text(angle = 90)) +
    ggplot2::labs(title = "Topic Volume over Time", x = NULL, y = "Number of Posts") +
    ggplot2::facet_wrap(~facet_var, nrow = nrow)


}

