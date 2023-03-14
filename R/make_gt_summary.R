#' Create a summary table, one step of the gt summary process
#'
#' @param data
#' @param group_var
#' @param sentiment_var
#'
#' @return
#' @export
#'
#' @examples
make_gt_summary_table <- function(data,
                                  group_var,
                                  sentiment_var){
  group_sym <- rlang::ensym(group_var)
  sentiment_sym <- rlang::ensym(sentiment_var)


  summary_table <- topic_tech %>%
    dplyr::count({{group_var}},
                 {{sentiment_var}}) %>%
    dplyr::mutate(volume = sum(n),
                  percent = n / sum(n) * 100,
                  .by = {{group_var}}) %>%
    dplyr::mutate({{sentiment_var}} := stringr::str_to_title({{sentiment_var}})) %>%
    dplyr::select(-n) %>%
    tidyr::pivot_wider(names_from = {{sentiment_var}},
                       values_from = percent)

  return(summary_table)
}


#' Title
#'
#' @param data Your data frame or tibble object
#' @param ...
#' @param max_colours
#'
#' @return
#' @export
#'
#' @examples
make_gt_grad_pal <- function(data, ..., max_colours){

  tmp_data <- data %>%
    select(...)

  my_cols <- colnames(tmp_data)
  my_col_names <- sort(my_cols)

  col_list <- tmp_data %>%
    tidyr::pivot_longer(everything(),
                        names_to = "my_name") %>%
    group_split(my_name)

  names(col_list) <- my_col_names

  col_list <- map(col_list, ~ .x %>%
                    summarise(max = max(value),
                              min = min(value)))

  col_list <- map2_df(col_list, names(col_list), ~ .x %>%
                        mutate(variable = .y))

  colour_lookup <- tibble::tibble(variable = names(max_colours),
                                  colour_value = max_colours)

  col_list <- col_list %>%
    left_join(colour_lookup)
}

#' Create a gt_summary table fit for use in client briefs
#'
#' Function calls various helper functions, @seealso `make_gt_gradient_palette`,
#' `make_gt_sumamry_table2`
#'
#' @param data
#' @param sentiment_var
#' @param group_var
#' @param max_colours
#'
#' @return
#' @export
#'
#' @examples
disp_gt_summary <- function(data, sentiment_var, group_var, max_colours){

}

#' quickly add re-usable theme elements for gt plot funcs
#'
#' @return list of ggplot boiler plate theme options
#' @export
#'
#' @examples
disp_gt_theme <- function(){

  ggplot_theme <- ggplot2::theme(
    plot.title = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    leegend.position = "null",
    axis.title = ggplot2::element_blank(),
    strip.text = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(size = 30),
    axis.text.y = ggplot2::element_text(angle = 45,
                                        hjust = 1),
    plot.margin = ggplot2::margin(30, 0, 0, 0)
  )
}

# summary <- topic_tec %>%
#   make_gt_summary_table(group_var = name,
#                         sentiment_var = sentiment)
#
# colours <- summary %>%
#   make_gt_grad_pal(Positive, Negative, Neutral,
#                            max_colours = c("Positive" = "#1b7837",
#                                            "Negative" = "#762a83",
#                                            "Neutral" = "yellow"))
# map(colours, .f = ~ .x %>%
#       col_numeric(palette = c("#f7f7f7", .$colour_value),
#                   domain = c(.$min, .$max),
#                   alpha = 0.75))
#
# map2(..1 = min, ..2 = max, ..3 = colour_value, ~ colours %>%
#        col_numeric(palette = c("#f7f7f7", ..3))
# )
