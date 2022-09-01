#' Quickly reformat a sentence transformers embedding output
#'
#' @param embeds an n-dimensional data frame with an index column (Pandas output without index = False)
#'
#' @return data frame with column names cleaned and index column removed
#' @export
#'
clean_embeds <- function(embeds){
  embeds <- embeds %>%
    dplyr::select(-document) %>%
    janitor::clean_names()

  names(embeds) <- stringr::str_replace_all(names(embeds), "x", "dim_")

  return(embeds)
}
