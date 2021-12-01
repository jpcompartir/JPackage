#' Remove posts with suspicious n-grams
#'
#' Function identifies posts which contain suspicious-looking n-gram patterns.
#' Posts can then be removed, the pattern inspected, and the posts that were removed too.
#' You can re-assign your current data frame to the 'clean' data frame through the third element of the list.
#'
#' @param data Data frame or tibble object
#' @param text_var Name of the text variable
#' @param n_gram  Number of words in the n-gram i.e. n = 2 = bigram
#' @param top_n  Number of n-grams to keep
#' @param min_freq Minimum number of
#'
#' @return A list with the suspicious-looking ngrams, removed posts, data & regex pattern
#' @export
#'

spam_grams <- function(data, text_var, n_gram = 4, top_n = 250, min_freq = 20){

  grams <- data %>%
    tidytext::unnest_tokens(ngram, {{text_var}}, token = "ngrams",
                            n = n_gram, format = "text")%>%
    dplyr::count(ngram, name = "count")%>%
    dplyr::slice_max(n = top_n, order_by = count) %>%
    dplyr::filter(count >= min_freq)

  regex <- paste0(grams$ngram, collapse = "|")

  removed <- data %>% dplyr::filter(stringr::str_detect({{text_var}}, regex))%>%
    dplyr::select({{text_var}})

  data <- data %>% dplyr::filter(!stringr::str_detect({{text_var}}, regex))

  list(grams, removed, data, regex)
}
