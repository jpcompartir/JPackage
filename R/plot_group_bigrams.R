#' Make a bigram network for each level of a categorical variable
#'
#' @param data Data frame or tibble object
#' @param cat_var  The categorical variable to create plots for
#' @param text_var  Name of the text variable
#' @param ... Additional arguments for make_bigram_viz i.e. n = , top_n =, min =
#'
#' @return list of ggplot2 objects
#' @export
#'
plot_group_bigrams <- function(data, cat_var, text_var = mention_content, ...){
  data %>%
    dplyr::group_split(cat_var)%>%
    purrr::map(make_bigram_viz, {{text_var}}, ...)
}
