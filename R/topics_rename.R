#' Rename a list of topics to a new list and set the variable as a factor with new names as levels
#'
#' @param df Data Frame or Tibble object
#' @param topic_var The current variable the topics are stored in
#' @param old_names The old names of the topics
#' @param new_names New names of the topics
#'
#' @return Data Frame or Tibble with topic variable changed in place
#' @export
#'
#' @examples
#' \dontrun{
#' Example 1:
#' old_names <- paste0("topic_", 1:2)
#' new_names <- c("New Topic 1", "New Topic 2")
#' df %>% topics_rename(topic_var = name, old_names = old_names, new_names = new_names)
#'
#' }
topics_rename <- function(df, topic_var, old_names, new_names){

  topic_sym <- rlang::ensym(topic_var)

  df <- purrr::map2_dfr(.x = old_names, .y = new_names, .f = ~ df %>%
                          dplyr::mutate({{topic_var}} := dplyr::case_when({{topic_var}} == .x ~ .y))) %>%
    dplyr::filter(!is.na(!!topic_sym)) %>%
    dplyr::mutate({{topic_var}} := factor(!!topic_sym, levels = new_names))

  return(df)
}
