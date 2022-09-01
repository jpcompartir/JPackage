#' Quickly make a top terms table from UMAP/Sentence Transformers/kMeans clusters
#'
#' @param df Data frame object with clusters/topics
#' @param group_var cluster/topic variable
#' @param text_var the text variable
#'
#' @return a data frame with tokens counted
#' @export
#'
make_top_terms_table <- function(df, group_var = cluster_name, text_var = mention_content){

  text_quo <- rlang::enquo(text_var)
  text_sym <- rlang::ensym(text_var)

  group_quo <- rlang::enquo(group_var)
  group_sym <- rlang::ensym(group_var)


  df %>%
    dplyr::group_split({{group_var}}) %>%
    purrr::map(~.x %>%
                 dplyr::mutate(!!text_quo := !! text_sym) %>%
                 dplyr::mutate(!!text_quo := tm::removeNumbers(!! text_sym)) %>%
                 LimpiaR::limpiar_url(!!text_quo) %>%
                 LimpiaR::limpiar_spaces(!!text_quo) %>%
                 tidytext::unnest_tokens(words, !! text_sym)) %>%
    purrr::reduce(bind_rows) %>%
    dplyr::count({{group_var}}, words, sort = TRUE)
}
