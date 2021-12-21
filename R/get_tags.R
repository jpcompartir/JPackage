#' Retrieve hashtags and at_tags from text
#'
#' Returns a list of two tibbles - hashtags and at_tags.
#' Index into the list with either $ syntax or []
#'
#' @param data Data frame or tibble object
#' @param text_var Name of the text variable
#'
#' @return A list of two tibbles
#' @export
#'
get_tags <- function(data, text_var = mention_content){

  tags <- data %>%
    dplyr::transmute(hashtags =stringr::str_extract_all({{text_var}}, "#\\w*"),
                     at_tags = stringr::str_extract_all({{text_var}}, "@\\w*"))

  hashtags <- tibble::tibble(hashtags = unlist(tags$hashtags))
  at_tags <- tibble::tibble(at_tags = unlist(tags$at_tags))

  list(hashtags, at_tags)
}
