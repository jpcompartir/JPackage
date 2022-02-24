#' Count multiple factors
#'
#' @param df Data frame and tibble object
#' @param ... The variables you want to create individual tables for - unquoted.
#'
#' @return Summary frames which separately count inputted variables and add a percent column
#' @export
#'
count_multiple <- function(df, ...){
  df %>%
    dplyr::select(...)%>%
    tidyr::pivot_longer(cols = everything())%>%
    dplyr::group_by(name)%>%
    tidyr::nest()%>%
    dplyr::ungroup()%>%
    dplyr::group_split(name)%>%
    purrr::map(unnest, cols = data)%>%
    purrr::map(count, value, sort = TRUE)%>%
    purrr::map(mutate, percent = 100 * n / sum(n))

}

