#' Tidy a zero-shot classifier output and join with original text variable
#'
#' @param text_df The text variable we fed into Huggingface
#' @param model_output Huggingface's model output with sequence, label, score
#' @param num_labels The number of labels given to the zero-shot classifier
#'
#' @return tibble
#' @export
#'
tidy_huggingface_zeroshot <- function(text_df, model_output, num_labels){
  labels <- tidyr::crossing(doc = 1:nrow(text_df),
                            sub_id = 1:num_labels)%>%
    dplyr::mutate(label = model_output$labels,
                  scores = model_output$scores) %>%
    tidyr::pivot_wider(names_from = label, values_from = scores)

  text_df <- text_df %>% dplyr::mutate(doc = dplyr::row_number())


  joined_df <- labels %>%
    dplyr::left_join(text_df, by = "doc")%>%
    dplyr::select(-sub_id)%>%
    dplyr::relocate(doc, text)

  joined_df %>%
    dplyr::group_by(doc, text)%>%
    dplyr::summarise_all(~ last(na.omit(.)))%>%
    dplyr::ungroup()

}
