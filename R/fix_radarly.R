#' Quickly wrangle a couple of annoying columns from Radarly outputs
#'
#' @param df Name of Data Frame / Tibble object
#'
#' @return Edited Data Frame
#' @export
#'
#' @usage
#'
#' fix_radarly(df)
fix_radarly <- function(df){
  df %>%
    janitor::clean_names()%>%
    dplyr::rename(mention_content = text,
                  mention_url = permalink,
                  sentiment = tone)%>%
    dplyr::mutate(date = lubridate::dmy_hm(date),
                  date = as.Date(date),
                  screen_name = dplyr::case_when(is.na(screen_name)~ "NA",
                                          TRUE ~ screen_name))%>%
    LimpiaR::limpiar_na_cols(0.01)
}

