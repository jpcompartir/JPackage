#' Retrieve the 1-5 word boundaries before and 1-10 after a pattern.
#'
#' Boundaries are broken up by spaces (super basic)
#'
#' @param data  Data frame or tibble object
#' @param text_var Name of the text variable
#' @param pattern Pattern to search for - should be a character vector
#' @param range_before Number of words before
#' @param range_after Number of words after
#' @param show_pattern Whether to print the pattern as well as the context tibble
#'
#' @return a tibble of the extracted contexts
#' @export
#'

context_grabber <- function(data, text_var = mention_content, pattern,
                          range_before = 5, range_after = 10, show_pattern = FALSE){

  text_var <- data %>% dplyr::pull({{text_var}})

if(range_before <= 0){before <- FALSE}else{before <- TRUE}
if(range_after <= 0){after <-  FALSE}else{after <- TRUE}

  #Conditional chain - sets the range and whether to extract before, after or both
  if(before & !after){
    pattern <- paste0("(\\w+\\s+){1,", range_before, "}\\s*", pattern, collapse = "")
  }else if (after & !before){
    pattern <- paste0(paste0(pattern, " (\\w+\\s+){1,", range_after, "}", collapse = ""))
  }else{pattern <- paste0("(\\w+\\s+){1,", range_before,"}\\s*", pattern, " (\\w+\\s+){1,", range_after,"}", collapse = "")}

  #if user wants to see the pattern generated too:
  if(show_pattern){
    list(pattern, tibble::tibble(contexts =  unlist(stringr::str_extract_all(text_var, pattern))))
  }else{tibble::tibble(contexts =  unlist(stringr::str_extract_all(text_var, pattern)))}


}


