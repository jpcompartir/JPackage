#' Run some fast text normalisation steps for output to Huggingface
#'
#' @param df Data Frame or Tibble object
#' @param text_var Name of the text variable
#'
#' @return Tibble with two columns
#' @export
#'
huggingface_quick_clean <- function(df, text_var){

  x <- dplyr::select(df, {{text_var}})

  x <- dplyr::mutate(x, doc = dplyr::row_number())

  x %>%
    LimpiaR::limpiar_url(text_var = {{text_var}})%>%
    LimpiaR::limpiar_emojis(text_var = {{text_var}}, with_emoji_tags = FALSE)%>%
    LimpiaR::limpiar_spaces(text_var = {{text_var}}) %>%
    LimpiaR::limpiar_duplicates(text_var = {{text_var}})%>%
    LimpiaR::limpiar_retweets(text_var = {{text_var}})

}
