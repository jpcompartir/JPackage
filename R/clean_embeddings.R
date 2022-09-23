#' Quickly reformat a sentence transformers embedding output
#'
#' @param embeds an n-dimensional data frame with an index column
#' @param ... columns that should be dropped, i.e. document or index column
#'
#' @return data frame with column names cleaned and index column removed
#' @export
#'
clean_embeds <- function(embeds, ...){
  embeds <- embeds %>%
    dplyr::select(...) %>%
    janitor::clean_names()

  names(embeds) <- stringr::str_replace_all(names(embeds), "x", "dim_")

  return(embeds)
}
