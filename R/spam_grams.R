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
#' @param in_parallel Whether to run the function with parallel processing
#'
#' @return A list with the suspicious-looking ngrams, removed posts, data & regex pattern
#' @export
#'

spam_grams <- function(data, text_var, n_gram = 8, top_n = 1000, min_freq = 5, in_parallel = TRUE){

  if(!in_parallel){
    grams <- data %>%
      tidytext::unnest_tokens(ngram, {{text_var}}, token = "ngrams",
                              n = n_gram, format = "text")%>%
      dplyr::count(ngram, name = "count")%>%
      dplyr::slice_max(n = top_n, order_by = count) %>%
      dplyr::filter(count >= min_freq)

    message("Unnesting ngrams finished")

    regex <- paste0(grams$ngram, collapse = "|")

    message("Removing spam regex from data")

    removed <- data %>% dplyr::filter(stringr::str_detect({{text_var}}, regex))%>%
      dplyr::select({{text_var}})

    data <- data %>% dplyr::filter(!stringr::str_detect({{text_var}}, regex))

    return(list(grams, removed, data, regex))
  }

  options(future.rng.onMisuse = "ignore")
  future::plan(future::multisession(workers = future::availableCores() -1))

  text_sym <- rlang::ensym(text_var)

  data <- data %>%
    dplyr::mutate(.row_id = dplyr::row_number(),
                  cuts = cut(.row_id, future::availableCores() - 1))

  #Note to self - no use parallelising this as it's so fast - actually slows us down.
  grams <- data %>%
    tidytext::unnest_tokens(ngram, {{text_var}}, token = "ngrams",
                            n = n_gram, format = "text")%>%
    dplyr::count(ngram, name = "count")%>%
    dplyr::slice_max(n = top_n, order_by = count) %>%
    dplyr::filter(count >= min_freq)

  message("Unnesting ngrams finished")

  regex <- grams$ngram
  regex <- paste0(regex, collapse = "|")

  message("Removing spam regex from data")
  removed <- data %>%
    dplyr::group_split(cuts) %>%
    furrr::future_map_dfr(~ .x %>%
                            dplyr::filter(stringr::str_detect(!!text_sym, regex)) %>%
                            dplyr::select(!!text_sym, .row_id))

  future::plan(future::sequential())
  message("Ending parallel session")

  data <- data %>% dplyr::filter(!.row_id %in% removed$.row_id) %>%
    dplyr::select(-c(.row_id, cuts))

  removed <- dplyr::select(removed, -.row_id)

  list(grams,removed,data,regex)
}


