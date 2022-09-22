#' Run the `make_DTMs` function logic but with parallel processing!
#'
#' Function does not implictly clean the text variable like `make_DTMs`
#'
#' @param df A data frame or tibble where each row is a separate post.
#' @param text_var The variable containing the text which you want to explore.
#' @param min_freq The minimum number of times a term must be observed to be considered.
#' @param hashtags Should hashtags be included?
#' @param mentions Should mentions be included?
#'
#' @return A nested tibble in which each row contains a document-term matrix.
#'
#' @usage make_DTMs_parallel(df, text_var, min_freq = 10, hashtags = FALSE, mentions = FALSE)
#' @examples make_DTMs_parallel(
#'  df = sprinklr_export,
#'  text_var = Message,
#'  min_freq = 25,
#'  hashtags = FALSE,
#'  mentions = FALSE
#' )
#' @export
#'
make_DTMs_parallel <- function(df,
                               text_var,
                               min_freq = 10,
                               hashtags = FALSE,
                               mentions = FALSE) {
  future::plan(future::multisession(workers = availableCores() -1))
  # Clean the text - this part was edited by Jack to avoid mismatching document IDs (Mar 28th 2022)
  clean_df <- df %>%
    tibble::rowid_to_column(var = "message_id") %>%
    dplyr::mutate(row_id = dplyr::row_number(),
                  cuts = cut(row_id, 7)) %>%
    dplyr::mutate(message = {{text_var}}) %>%
    dplyr::select(message, message_id, cuts) %>%
    dplyr::filter(!is.na(message))%>%
    dplyr::mutate(message_id = as.character(message_id))
  ## Create dtms ----
  # 'Tuning parameters' for dtm creation
  dtm_tuning <- tidyr::expand_grid(freq_cutoff = min_freq)
  # Add freq cut-offs for each query-source combo
  dtm_setup <- clean_df %>%
    tidyr::nest(data = tidyr::everything()) %>%
    dplyr::mutate(dtm_tuning = list(dtm_tuning)) %>%
    tidyr::unnest(cols = dtm_tuning)
  # Define function for creating dtm from tibble
  create_dtm <- function(data, term_n) {
    stopwords <- SegmentR::stopwords
    term_counts <- data %>%
      dplyr::group_split(cuts) %>%
      furrr::future_map_dfr(~ .x %>%
                              # Split posts into individual words
                              tidytext::unnest_tokens(input = message,
                                                      output = word,
                                                      token = "tweets") %>%
                              # Remove boring words
                              dplyr::filter(!word %in% stopwords) %>%
                              # Remove words which don't appear frequently
                              dplyr::count(message_id, word, name = "term_freq")
      ) %>%
      dplyr::group_by(word) %>%
      dplyr::filter(sum(term_freq) > term_n) %>%
      dplyr::ungroup()
    # If no terms exceed required threshold
    if (nrow(term_counts) == 0) {
      return(NA)
    } # This stops error but don't know why, seem to be no NULL entries in output
    # Create dtm using calculated frequencies
    term_counts %>%
      tidytext::cast_dtm(document = message_id, term = word, value = term_freq)
  }

  future::plan(future::sequential())

  # Create dtms using different freq cutoffs
  dtm_setup %>%
    dplyr::mutate(dtm = purrr::map2(.x = data, .y = freq_cutoff, create_dtm),
                  n_terms = purrr::map_dbl(dtm, ncol),
                  n_docs = purrr::map_dbl(dtm, nrow))


}
