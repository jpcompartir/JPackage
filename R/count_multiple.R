#' Count multiple variables of the same data type
#'
#' @param df Data frame and tibble object
#' @param ... The variables you want to create individual tables for - unquoted and should be of the same data type.
#'
#' @return Summary frames which separately count inputted variables and add a percent column
#' @export
#'
count_multiple <- function(df, ...){

  df <- df %>%
    dplyr::select(...)%>%
    tidyr::pivot_longer(cols = everything())

  list_names <- df %>% dplyr::distinct(name) %>% dplyr::pull(1)
  list_names <- sort(list_names)

  list_output <- df %>%
    dplyr::group_by(name)%>%
    tidyr::nest()%>%
    dplyr::ungroup()%>%
    dplyr::group_split(name)%>%
    purrr::map(., ~ .x %>% tidyr::unnest(cols = data)%>%
                 dplyr::count(value, sort = TRUE) %>%
                 dplyr::mutate(percent = 100 * n / sum(n)))

  names(list_output) <- list_names

  return(list_output)

}

