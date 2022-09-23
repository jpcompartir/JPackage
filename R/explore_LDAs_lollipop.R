#' explore_LDAs but with lollipops!
#'
#' Produces exploratory visualisations from LDA models.
#'
#' @param ldas A nested tibble where each row contains and LDA model.
#' @param top_terms Should bar charts of the top terms for each topic be produced?
#' @param top_n How many terms should be included in the top terms bar chart for each topic?
#' @param nrow How many rows should the facetted plot go across?
#' @param diff_terms Should bar charts of distinguishing terms for each topic be produced?
#' @param diff_n How many terms should be included in the distinguishing terms bar chart for each topic?
#' @param bigrams Should a bigram network be produced for each topic?
#' @param bigram_n How many bigrams should be included in the network?
#' @param bigram_threshold What proportion of a post should be assigned to a topic for it to be considered for inclusion in the bigram network?
#' @param exemplars Should a tibble of exemplar posts be produced?
#' @param exemplars_n How many exemplars should be provided for each topic?
#' @param probabilities Should a tibble of document-topic probabilities be produced?
#' @param coherence Should a tibble of topic coherence measurements be produced?
#' @param coherence_n Number of words to be used in coherence calculation.
#'
#' @return A nested tibble containing columns with the requested objects.
#' @usage explore_LDAs(
#'  ldas,
#'  top_terms = TRUE,
#'  top_n = 15,
#'  nrow = 2,
#'  diff_terms = TRUE,
#'  diff_n = 10,
#'  bigrams = TRUE,
#'  bigram_n = 50,
#'  bigram_threshold = 0.5,
#'  exemplars = TRUE,
#'  exemplars_n = 50,
#'  probabilities = TRUE,
#'  coherence = TRUE,
#'  coherence_n = 10
#' )
#' @export
explore_LDAs_lollipops <- function(ldas,
                            top_terms = TRUE,
                            top_n = 20,
                            nrow = 2,
                            diff_terms = TRUE,
                            diff_n = 10,
                            bigrams = TRUE,
                            bigram_n = 50,
                            bigram_threshold = 0.5,
                            exemplars = TRUE,
                            exemplars_n = 50,
                            probabilities = TRUE,
                            coherence = TRUE,
                            coherence_n = 10) {

  explore <- ldas

  if(top_terms == TRUE){
    explore <- explore %>%
      dplyr::mutate(top_terms = purrr::map(.x = lda, .top_beta, top_n = top_n,
                                           nrow = nrow))
  }

  if(diff_terms == TRUE){
    explore <- explore %>%
      dplyr::mutate(diff_terms = purrr::map(.x = lda, .diff_beta, top_n = diff_n))
  }

  if(bigrams == TRUE){

    # Check if ParseR is installed
    if (require("ParseR", character.only = TRUE, quietly = T) == FALSE) {
      stop("You need to install ParseR to produce bigram networks.")
    }

    explore <- explore %>%
      dplyr::mutate(bigrams = purrr::map2(.x = lda,
                                          .y = data,
                                          .within_topic_bigram,
                                          gamma_threshold = bigram_threshold,
                                          top_n = bigram_n))
  }

  if(exemplars == TRUE){
    explore <- explore %>%
      dplyr::mutate(exemplars = purrr::map2(.x = lda,
                                            .y = data,
                                            .get_exemplars,
                                            n_posts = exemplars_n))
  }

  if (probabilities == TRUE){
    explore <- explore %>%
      dplyr::mutate(probabilities = purrr::map2(.x = lda,
                                                .y = data,
                                                .get_probabilities))

  }

  if (coherence == TRUE){
    explore <- explore %>%
      dplyr::mutate(coherence = purrr::map2(.x = dtm,
                                            .y = lda,
                                            .f = .get_coherence,
                                            M = coherence_n))
  }

  explore %>%
    # Remove for neatness
    dplyr::select(-data, -lda)

}

## Helper functions ----

# Microsoft colour palette function
.ms_palette <- function(n) {

  # Define 8 microsoft colours
  ms_colours <- c("#D83B01", # Orange
                  "#0078D4", # Blue
                  "#107C10", # Green
                  "#8661C5", # Purple
                  "#008575", # Teal
                  "#FFB900", # Yellow
                  "#50E6FF", # Light Blue
                  "#FF9349", # Light Orange
                  "#D83B08", # Orange
                  "#007808") # Blue

  # Choose n colours
  ms_colours[1:n]

}

# Define function for finding highest beta terms within each topic
.top_beta <- function(lda, top_n = 10, nrow = 2) {

  ptpw <- broom::tidy(lda, matrix = "beta")

  # Which terms have highest beta within each topic?
  top_terms <- ptpw %>%
    dplyr::group_by(topic) %>%
    dplyr::top_n(top_n, beta) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(topic, -beta)

  # Facetted by topic (will duplicate common terms)
  facetted <- top_terms %>%
    dplyr::mutate(term = stringr::str_wrap(term),
                  term = tidytext::reorder_within(term, beta, topic)) %>%
    ggplot2::ggplot(ggplot2::aes(term, beta, fill = factor(topic),
                                 colour = factor(topic))) +
    # ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::geom_segment(aes(x = term, xend = term,
                              y = 0, yend = beta),
                          show.legend = FALSE
    ) +
    ggplot2::geom_point(size = 3,
                        shape = 21,
                        show.legend = FALSE) +
    ggplot2::facet_wrap(ggplot2::vars(topic),
                        scales = "free",
                        labeller = ggplot2::label_both,
                        nrow = nrow) +
    ggplot2::coord_flip() +
    tidytext::scale_x_reordered("Term") +
    ggplot2::scale_y_continuous("Probability term generated by topic (\u03b2)") +
    # ggplot2::scale_fill_manual(palette = .ms_palette) +
    ggplot2::scale_fill_manual(values = as.vector(.ms_palette(10))) +
    ggplot2::scale_colour_manual(values = as.vector(.ms_palette(10))) +
    ggplot2::theme_minimal() +
    ggplot2::theme(title = ggplot2::element_text(size = 16),
                   text = ggplot2::element_text(size = 8),
                   strip.text = ggplot2::element_text(size = 14),
                   axis.title.y = ggplot2::element_text(angle = 0, vjust = 0.5),
                   axis.title.x = ggplot2::element_text(size = 14))


  # Which topics are terms most likely to belong to?
  most_likely_topics <- ptpw %>%
    dplyr::group_by(term) %>%
    dplyr::filter(beta == max(beta)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(topic) %>%
    dplyr::top_n(top_n, beta) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(topic, -beta) %>%
    dplyr::mutate(term = tidytext::reorder_within(term, beta, topic)) %>%
    ggplot2::ggplot(ggplot2::aes(term, beta, fill = factor(topic),
                                 colour = factor(topic))) +
    # ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::geom_segment(aes(x = term, xend = term,
                              y = 0, yend = beta),
                          show.legend = FALSE
    ) +
    ggplot2::geom_point(size = 3,
                        shape = 21,
                        show.legend = FALSE) +
    ggplot2::facet_wrap(ggplot2::vars(topic),
                        scales = "free",
                        labeller = ggplot2::label_both,
                        nrow = nrow) +
    ggplot2::coord_flip() +
    tidytext::scale_x_reordered("Term") +
    ggplot2::scale_y_continuous("Probability term belongs to topic (\u03b2)") +
    # ggplot2::scale_fill_manual(palette = .ms_palette) +
    ggplot2::scale_fill_manual(values = as.vector(.ms_palette(10))) +
    ggplot2::scale_colour_manual(values = as.vector(.ms_palette(10))) +
    ggplot2::theme_minimal() +
    ggplot2::theme(title = ggplot2::element_text(size = 16),
                   text = ggplot2::element_text(size = 8),
                   strip.text = ggplot2::element_text(size = 14),
                   axis.title.y = ggplot2::element_text(angle = 0, vjust = 0.5),
                   axis.title.x = ggplot2::element_text(size = 14))

  # Output
  list("all_terms" = facetted,
       "max_only" = most_likely_topics)

}

#Jamie's function for making bigrams cleaner
.jh_viz_ngram <- function(tbl_graph, emphasis = TRUE) {

  {if (emphasis == TRUE) {

    # Get scale breaks
    node_freq <- tbl_graph %>%
      tidygraph::activate(nodes) %>%
      tidygraph::as_tibble() %>%
      dplyr::pull(word_freq)

    node_breaks <- (c(max(node_freq), min(node_freq))) %>%
      stats::quantile(probs = c(seq(0, 1, 0.25))) %>%
      round()
    names(node_breaks) <- NULL

    edge_freq <- tbl_graph %>%
      tidygraph::activate(edges) %>%
      tidygraph::as_tibble() %>%
      dplyr::pull(ngram_freq)

    edge_breaks <- (c(max(edge_freq), min(edge_freq))) %>%
      stats::quantile(probs = c(seq(0, 1, 0.25))) %>%
      round()
    names(edge_breaks) <- NULL

    # Make network
    ngram_network <- ggraph::ggraph(tbl_graph, layout = "nicely") +
      ggraph::geom_node_point(ggplot2::aes(size = word_freq,
                                           alpha = word_freq,
                                           color = word_freq),
                              stroke = 0) +
      ggraph::geom_edge_fan(ggplot2::aes(edge_alpha = ngram_freq,
                                         edge_colour = ngram_freq),
                            arrow = grid::arrow(type = "closed",
                                                length = grid::unit(.1, "inches")),
                            end_cap = ggraph::circle(.05, "inches")) +
      ggraph::geom_node_text(ggplot2::aes(label = word),
                             size = 4,
                             vjust = 0.5,
                             hjust = 1,
                             colour = "black",
                             bg.colour = "white",
                             repel = TRUE) +
      ggplot2::scale_color_viridis_c("Term\nFrequency",
                                     breaks = node_breaks,
                                     labels = scales::comma,
                                     guide = "legend") +
      ggplot2::scale_size_continuous("Term\nFrequency",
                                     breaks = node_breaks,
                                     labels = scales::comma) +
      ggplot2::scale_alpha_continuous("Term\nFrequency",
                                      breaks = node_breaks,
                                      range = c(0.5, 1),
                                      labels = scales::comma) +
      ggraph::scale_edge_color_viridis("N-gram\nFrequency",
                                       breaks = edge_breaks,
                                       labels = scales::comma,
                                       guide = "legend") +
      ggraph::scale_edge_alpha_continuous("N-gram\nFrequency",
                                          breaks = edge_breaks,
                                          range = c(0.5, 1),
                                          labels = scales::comma) +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "right",
                     title = ggplot2::element_text(size = 12),
                     text = ggplot2::element_text(size = 12))
  } else{
    ngram_network <- ggraph::ggraph(tbl_graph, layout = "nicely") +
      ggraph::geom_node_point() +
      ggraph::geom_node_text(ggplot2::aes(label = word),
                             size = 3.5,
                             vjust = 1.1,
                             hjust = 1) +
      ggraph::geom_edge_fan(ggplot2::aes(edge_alpha = ngram_freq),
                            arrow = grid::arrow(type = "closed",
                                                length = grid::unit(.1, "inches")),
                            end_cap = ggraph::circle(.05, "inches"),
                            show.legend = FALSE) +
      ggplot2::theme_void()
  }}

  # Output

  ngram_network

}

# Define function for finding terms with greatest difference in between topics
.diff_beta <- function(lda, top_n = 10) {

  # Get beta values
  beta_wide <- lda %>%
    broom::tidy(matrix = "beta") %>%
    dplyr::mutate(topic = paste0("topic", topic)) %>%
    tidyr::pivot_wider(names_from = topic, values_from = beta)

  # Get number of topics in the model
  k <- lda@k

  # How many plots will there be?
  n_plots <- choose(n = k, k = 2)
  spread_list <- vector("list", length = n_plots)

  # What plots will there be?
  plot_combos <- combn(x = 1:k, m = 2) # Each column is a combo
  combo_names <- paste0("topic", plot_combos) # Each pair is a combo

  # Assign each topic a colour
  my_colours <- .ms_palette(n = k)
  combo_colours <- my_colours[c(plot_combos)]

  # Empty vector for names
  plot_names <- vector("character", length = n_plots)

  for (i in seq(1, length(combo_names), 2)) {

    plot_df <- beta_wide %>%
      dplyr::select_at(.vars = tidyselect::all_of(c("term",
                                                    combo_names[i],
                                                    combo_names[i + 1]))) %>%
      dplyr::rename("topic_x" = combo_names[i], "topic_y" = combo_names[i + 1])

    spread_plot <- plot_df %>%
      dplyr::mutate(log2_ratio = log2(topic_x / topic_y)) %>%
      dplyr::group_by(direction = log2_ratio > 0) %>%
      dplyr::top_n(top_n, abs(log2_ratio)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(term = stats::reorder(term, log2_ratio)) %>%
      ggplot2::ggplot(ggplot2::aes(x = term, y = log2_ratio, fill = direction,
                                   colour = direction)) +
      # ggplot2::geom_col() +
      ggplot2::geom_segment(aes(x = term, xend = term,
                                y = 0, yend = log2_ratio),
                            show.legend = FALSE
      ) +
      ggplot2::geom_point(size = 4,
                          shape = 21,
                          show.legend = FALSE) +
      ggplot2::scale_x_discrete("Term", labels = scales::wrap_format(15)) +
      ggplot2::scale_y_continuous(paste0("Log2(",
                                         combo_names[i],
                                         " \u03b2 / ",
                                         combo_names[i + 1],
                                         " \u03b2)")) +
      ggplot2::scale_fill_manual("Topic",
                                 values = c("TRUE" = combo_colours[i],
                                            "FALSE" = combo_colours[i + 1]),
                                 labels = c("TRUE" = combo_names[i],
                                            "FALSE" = combo_names[i + 1])) +
      ggplot2::scale_colour_manual("Topic",
                                   values = c("TRUE" = combo_colours[i],
                                              "FALSE" = combo_colours[i + 1]),
                                   labels = c("TRUE" = combo_names[i],
                                              "FALSE" = combo_names[i + 1])) +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal() +
      ggplot2::theme(title = ggplot2::element_text(size = 16),
                     text = ggplot2::element_text(size = 8),
                     axis.title.y = ggplot2::element_text(angle = 0, vjust = 0.5))

    spread_list[[(i + 1)/2]] <- spread_plot

    plot_names[(i + 1)/2] <- paste0(combo_names[i],
                                    "_vs_",
                                    combo_names[i + 1])

  }

  # Add names
  names(spread_list) <- plot_names

  # Output
  spread_list

}

.within_topic_bigram <- function(lda,
                                 orig_data,
                                 gamma_threshold = 0.75,
                                 top_n = 30) {

  # Get per-document-per-topic probabilities
  pdpt <- broom::tidy(lda, matrix = "gamma")

  # Get number of topics in the model
  k <- lda@k

  # Initialise list of required length
  bigram_list <- vector("list", length = k)

  most_likely_topics <- pdpt %>%
    dplyr::filter(gamma >= gamma_threshold) %>%
    dplyr::group_by(document) %>%
    dplyr::filter(gamma == max(gamma)) %>%
    dplyr::ungroup()

  # Generate bigram for each of k topics in the model
  for (i in 1:k) {
    topic_counts <- most_likely_topics %>%
      dplyr::filter(topic == i) %>%
      dplyr::inner_join(., orig_data, by = c("document" = "message_id")) %>%
      ParseR::count_ngram(., text_var = message, top_n = top_n)

    bigram_list[[i]] <- .jh_viz_ngram(topic_counts$viz) +
      ggplot2::labs(caption = paste0("Topic ", i))
  }

  names(bigram_list) <- paste0("topic_", 1:k, "_bigram")

  bigram_list

}

# Define function for getting example posts for each topic
.get_exemplars <- function(lda, orig_data, n_posts = 10) {

  # Get per-document-per-topic probabilities
  pdpt <- broom::tidy(lda, matrix = "gamma")

  # Get number of topics in the model
  k <- lda@k

  # Get top n posts for each topic
  pdpt %>%
    dplyr::group_by(topic) %>%
    dplyr::top_n(n_posts, wt = gamma) %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(., orig_data, by = c("document" = "message_id")) %>%
    dplyr::arrange(topic, dplyr::desc(gamma))

}

# Function for getting all probabilities
.get_probabilities <- function(lda, orig_data){

  pdpt <- broom::tidy(lda, matrix = "gamma")

  pdpt %>%
    dplyr::mutate(topic = as.character(paste("topic_", topic, sep = ""))) %>%
    tidyr::spread(key = topic, value = gamma) %>%
    dplyr::mutate(document = as.numeric(document)) %>%
    dplyr::arrange(document)

}

# Calculates the probablistic coherence for each of the topics
.get_coherence <- function(dtm, lda, M = 10) {

  dtm_matrix <- Matrix::sparseMatrix(i=dtm$i,
                                     j=dtm$j,
                                     x=dtm$v,
                                     dims=c(dtm$nrow, dtm$ncol),
                                     dimnames = dtm$dimnames)

  phi = lda %>%
    generics::tidy(matrix = "beta") %>%
    tidyr::pivot_wider(names_from = "term", values_from = "beta",
                       names_repair = "minimal") %>%
    dplyr::select(-topic) %>%
    as.matrix()

  coherence <- textmineR::CalcProbCoherence(phi,
                                            dtm_matrix,
                                            M = M)

  topic <- paste("topic_", as.character((1:length(coherence))), sep = "")

  tibble::tibble(topic, coherence)

}
