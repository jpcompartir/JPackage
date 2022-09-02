#' Parallelised fit_LDAs function + coherence included.
#'
#' Fits multiple LDA models (one for each combination of the supplied parameters)
#' This is generally faster than fit_LDAs, the larger the dataset, the larger the benefit.
#'
#' @param dtms A nested tibble where each row is a dtm
#' @param k_opts vector of the different values of k to fit an LDA model with (k = number of topics)
#' @param iter_opts NA vector of the different values of iter to fit an LDA model with
#' @param coherence_n Number of words to calculate coherence from
#' @return A nested tibble in which each row contains an LDA model.
#' @usage old_par_fit_LDAs(dtms, k_opts = 2:3, iter_opts = 2000, coherence_n = 10)
#' @export
old_par_fit_LDAs <- function(dtms,
                         k_opts = 2:3,
                         iter_opts = 2000,
                         coherence_n = 10){

  #Set up multicore session. Leave 1 core out of everything:
  future::plan(future::multisession(workers = availableCores()-1))

  # Set up tuning grid for LDA model
  lda_tuning <- tidyr::expand_grid(k = k_opts,
                                   alpha = 1,
                                   delta = 1,
                                   iter = iter_opts) %>%
    dplyr::mutate(alpha = 1/k, delta = 0.1/k)

  # Add tuning parameters for each query-source combo
  lda_setup <- dtms %>%
    dplyr::group_by(freq_cutoff) %>%
    dplyr::mutate(lda_tuning = list(lda_tuning)) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(cols = lda_tuning)

  # Define function to run LDA for different values of k
  run_lda <- function(dtm, k, alpha, delta, iter) {
    topicmodels::LDA(x = dtm,
                     method = "Gibbs",
                     k = k,
                     control = list(seed = 1,
                                    iter = iter,
                                    alpha = alpha,
                                    delta = delta))
  }
  options(future.rng.onMisuse = "ignore")
  # Fit
  lda <-lda_setup %>%
    dplyr::mutate(lda = furrr::future_pmap(.l = list(dtm = dtm,
                                                     k = k,
                                                     alpha = alpha,
                                                     delta = delta,
                                                     iter = iter),
                                           .f = run_lda), seed = TRUE) %>%
    dplyr::select(data, dtm, freq_cutoff, n_terms, n_docs, k, alpha, delta, iter, lda)



  #Add coherence function to LDAs
  .get_coherence <- function(dtm, lda, M = 10) {

    dtm_matrix <- Matrix::sparseMatrix(i=dtm$i,
                                       j=dtm$j,
                                       x=dtm$v,
                                       dims=c(dtm$nrow, dtm$ncol),
                                       dimnames = dtm$dimnames)

    phi <-  lda %>%
      generics::tidy(matrix = "beta") %>%
      tidyr::pivot_wider(names_from = "term", values_from = "beta",
                         names_repair = "minimal") %>%
      dplyr::select(-topic) %>%
      as.matrix()

    coherence <- textmineR::CalcProbCoherence(phi,
                                              dtm_matrix,
                                              M = coherence_n)

    topic <- paste("topic_", as.character((1:length(coherence))), sep = "")

    #return
    tibble::tibble(topic, coherence)
  }
  future:::ClusterRegistry("stop")
  #No need to use furrr::future_map2() here as it's unstable and calc is quick.
  lda %>%
    dplyr::mutate(coherence = purrr::map2(.x = dtm,
                                          .y = lda,
                                          .f = .get_coherence,
                                          M = M))
}
