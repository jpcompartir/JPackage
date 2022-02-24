#' Count a factor and then add a column which shows percentage
#'
#' @param df Data frame or tibble object
#' @param var The variable to count
#'
#' @return A summary table with the factor, count and percentage
#' @export
#'
percent_summ <- function(df, var){

  counts <- dplyr::count(df, {{var}})

  dplyr::mutate(counts, percent = 100 * n / sum(n))
}


