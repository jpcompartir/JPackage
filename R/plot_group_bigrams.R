#' Make a bigram network for each element of a grouping variable
#'
#' @param data Data frame or tibble object
#' @param group_var  The categorical variable to create plots for
#' @param text_var  Name of the text variable
#' @param ... Additional arguments for make_bigram_viz i.e. n = , top_n =, min =
#'
#' @return list of ggplot2 objects
#' @export
#'
plot_group_bigrams <- function(data, group_var, text_var = mention_content, ...){
  data %>%
    dplyr::group_split({{group_var}})%>%
    purrr::map(make_bigram_viz, {{text_var}}, ...)
}
