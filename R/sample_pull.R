#' Quickly view a sample of any variable
#'
#' @param df Data Frame or Tibble object
#' @param variable The variable you want to sample
#' @param n Number of samples you want to print
#'
#' @return Printed samples of your variable
#' @export
#'
sample_pull <- function(df = df, variable = mention_content, n = 10 ){
  df %>%
    sample_n(n)%>%
    pull({{variable}})
}
