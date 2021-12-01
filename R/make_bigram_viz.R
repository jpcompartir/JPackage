#' Plot a bigram network
#'
#' @param data Data frame or tibble object
#' @param top_n Number of n-grams to plot
#' @param min Min frequency of n-gram
#' @param text_var Name of the text variable
#'
#' @return A ggplot2 network viz
#' @export

make_bigram_viz <- function(data, text_var = mention_content, top_n = 50, min = 10){
  data %>%
    ParseR::count_ngram(text_var = {{text_var }}, top_n = top_n, min_freq = min)%>%
    purrr::pluck("viz")%>%
    ParseR::viz_ngram()
}
