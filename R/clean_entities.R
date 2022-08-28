#' Quickly extract columns of interest from Hugging Face entity recognisers
#'
#' @param df data frame with entities extracted in JSON format
#'
#' @return tidier data frame for analysis.
#' @export
#'
clean_entities <- function(df) {
  df %>%
    janitor::clean_names()%>%
    dplyr::rename(document = x1)%>%
    tidyr::pivot_longer(dplyr::contains('x'))%>%
    dplyr::filter(!is.na(value)) %>%
    tidyr::separate(value, into = c("entity_type", "entity_score","entity",  "index"), sep = ",")%>%
    dplyr::mutate(entity_type = stringr::str_remove(entity_type, "\\{'entity_group': '"),
                  entity_type = stringr::str_remove(entity_type, "'"),
                  entity_score = stringr::str_remove(entity_score, " 'score': "),
                  entity_score = stringr::str_remove(entity_score, " '"),
                  entity = stringr::str_remove(entity, " 'word': '"),
                  entity = stringr::str_remove(entity, "'"))%>%
    dplyr::mutate(entity_score = round(as.numeric(entity_score), 3))%>%
    dplyr::select(-name, -index)

}
