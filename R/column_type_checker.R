#' Quick function for checking if a column is of the right type using data-masking
#'
#' @param data Data Frame or Tibble object
#' @param column Column you want to check
#' @param type `column`'s expected type
#'
#' @return a character vector
#' @export
#'
#' @examples
#' \dontrun{
#' check_text <- df %>% column_type_checker(text_var, "character")
#'
#' if(check_text == "no") stop("Wrong type")
#'
#' }
column_type_checker <- function(data,
                                column,
                                type){
  if(!all(class(data %>% dplyr::pull({{column}})) == type)){
    return("no")
  } else {
    return("yes")
  }

}
