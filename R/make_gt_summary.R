#' Create a summary table, one step of the gt summary process
#'
#' @param data
#' @param group_var
#' @param sentiment_var
#'
#' @return
#' @export
#'
#' @examples
make_gt_summary_table <- function(data,
                                  group_var,
                                  sentiment_var){
  #Tidy evaluate supplied variables
  group_sym <- rlang::ensym(group_var)
  sentiment_sym <- rlang::ensym(sentiment_var)


  summary_table <- topic_tech %>%
    dplyr::count({{group_var}},
                 {{sentiment_var}}) %>%
    dplyr::mutate(volume = sum(n),
                  percent = n / sum(n) * 100,
                  .by = {{group_var}}) %>%
    dplyr::mutate({{sentiment_var}} := tolower({{sentiment_var}})) %>% #convert to lower case, can convert back later
    dplyr::select(-n) %>%
    tidyr::pivot_wider(names_from = {{sentiment_var}},
                       values_from = percent)

  return(summary_table)
}



#' Title
#'
#' @param data Your data frame or tibble object
#' @param ...
#' @param max_colours
#'
#' @return
#' @export
#'
#' @examples
make_gt_grad_pal <- function(data, ..., max_colours){

  tmp_data <- data %>%
    select(...)

  my_cols <- colnames(tmp_data)
  my_col_names <- sort(my_cols)

  col_list <- tmp_data %>%
    tidyr::pivot_longer(everything(),
                        names_to = "my_name") %>%
    group_split(my_name)

  names(col_list) <- my_col_names

  col_list <- map(col_list, ~ .x %>%
                    summarise(max = max(value),
                              min = min(value)))

  col_list <- map2_df(col_list, names(col_list), ~ .x %>%
                        mutate(variable = .y))

  colour_lookup <- tibble::tibble(variable = names(max_colours),
                                  colour_value = max_colours)

  colours_df <- col_list %>%
    left_join(colour_lookup) %>%
    tidyr::unnest(colour_value) %>%
    dplyr::mutate(base_colour = "#f7f7f7") %>% #Add base colour for both
    tidyr::pivot_longer(cols = c(max, min)) %>% #Reshape data for pamp
    tidyr::pivot_wider(names_from = name, values_from = value) #Finish reshape data for pam
}



#' Create a gt_summary table fit for use in client briefs
#'
#' Function calls various helper functions, @seealso `make_gt_gradient_palette`,
#' `make_gt_sumamry_table2`
#'
#' @param data
#' @param sentiment_var
#' @param group_var Will usually be the name of the topic variable (name or topic)
#' @param sentiment_max_colours positive and sentiment classes in lower case
#'
#' @return
#' @export
#'
#' @examples
disp_gt_summary <- function(data,
                            sentiment_var,
                            group_var,
                            date_var,
                            time_unit = c("month", "day", "week", "year", "quarter"),
                            sentiment_max_colours = list("positive" = "#1b7837", "negative" = "#762a83"),
                            icons = NULL,
                            table_title = "Test",
                            source_note = "source_note ="){

  time_unit <- match.arg(time_unit)

  date_sym <- rlang::ensym(date_var)
  group_sym <- rlang::ensym(group_var)
  sentiment_sym <- rlang::ensym(sentiment_var)

  summary <- data %>%
    make_gt_summary_table(group_var = !!group_sym,
                          sentiment_var = !!sentiment_sym)

  #Create the palettes for Negative and Positive
  palettes_df <- summary %>%
    make_gt_grad_pal(negative, positive, max_colours = sentiment_max_colours)

  joined_df <- .disp_join_summ_pal(summary, palettes_df)

  #Make palettes
  base <- unique(joined_df$base_colour)

  max_pos <- unique(joined_df$pos_max)
  min_pos <- unique(joined_df$pos_min)
  pos_colour <- unique(joined_df$pos_colour_value)
  pos_palette <- scales::col_numeric(
    c(base, pos_colour),
    domain = c(min_pos, max_pos),
    alpha = 0,75
  )

  max_neg <- unique(joined_df$neg_max)
  min_neg <- unique(joined_df$neg_min)
  neg_colour <- unique(joined_df$neg_colour_value)

  neg_palette <- scales::col_numeric(
    c(base, neg_colour),
    domain = c(min_neg, max_neg),
    alpha = 0.75
  )



  joined_df <- joined_df %>%
    dplyr::select({{group_var}}, Volume = volume, Positive = positive, Neutral = neutral, Negative = negative) %>%
    dplyr::mutate(`Volume x Time` = "",
                  `Sentiment x Time` = "")

  # Space for adding icons here
  if(!is.null(icons)){
    dplyr::mutate(joined_df, icons = icons, .before = 1)
  }

  table <- joined_df %>%
    gt::gt()

  #There is a chance that group_splitting this way doesn't work if the factors are re-ordered, I think?
  splits <-  dplyr::group_split(data, {{group_var}})
  vol_plot_list <-purrr::map(splits, ~.x %>%
                 disp_gt_vot(date_var = {{date_var}}, time_unit = time_unit))

  sent_plot_list <- purrr::map(splits, ~ .x %>%
                                 disp_gt_sent_time(date_var = {{date_var}}, sentiment_var = {{sentiment_var}}, time_unit = time_unit))

  table <- table %>%
    gt::data_color(columns = Positive,
                   colors = pos_palette) %>%
    gt::data_color(columns = Negative,
                   colors = neg_palette) %>%
    gt::summary_rows(columns = c(Volume),
                     fns = list(Total = "sum"),
                     decimals = 0,
                     missing_text = "") %>%
    gt::text_transform(
      locations =
        gt::cells_body(columns = `Volume x Time`),
      fn = function(x){
        vol_plot_list %>%
          gt::ggplot_image(height = px(80),
                                aspect_ratio = 2)
      }
      ) %>%
    gt::text_transform(
      locations = gt::cells_body(columns = `Sentiment x Time`),
      fn = function(x){
        sent_plot_list %>%
          gt::ggplot_image(height = px(80),
                           aspect_ratio = 2)
      }
    ) %>%
    gt::tab_header(title = table_title) %>%
    gt::tab_source_note(source_note = source_note) %>%
    gt::fmt_number(columns = Volume, sep_mark = ",", decimals = 0) %>%
    gt::fmt_percent(columns = c(Positive, Negative, Neutral), decimals = 1, scale_values = FALSE) %>%
    gt::opt_table_font("Segoe UI") %>%
    gt::cols_align(align = "center") %>%
    gt::tab_options(
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      #Remove border around table
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      #Adjust font sizes and alignment
      source_notes.font.size = 12,
      heading.align = "left") %>%
    gt::tab_style(style = gt::cell_text(weight = "600"),
              locations = gt::cells_title(groups = "title")) %>%
    gt::tab_style(style = gt::cell_text(color = "grey50",
                                transform = "capitalize"),
              locations = gt::cells_column_labels(tidyselect::everything())) %>%
    tab_style(style = gt::cell_text(style = "italic"),
              locations = gt::cells_stub_grand_summary()) %>%
    tab_style(style = gt::cell_text(style = "italic"),
              locations = gt::cells_source_notes()) %>%
    tab_style(style = gt::cell_text(weight = "600"),
              locations = gt::cells_grand_summary())


  return(table)
}


.disp_join_summ_pal <- function(summary, palettes_df){
  #THis is hanging by an absolute thread, to be refactored later

  joined_df <- summary %>%
    tidyr::pivot_longer(c(negative, positive),
                        names_to = "variable") %>%
    dplyr::left_join(palettes_df)

  names_splits <- sort(unique(joined_df$variable))
  splits <- dplyr::group_split(joined_df, variable)
  names(splits) <- names_splits

  splits <- purrr::map(splits, ~ .x %>% tidyr::pivot_wider(names_from = variable, values_from = value))

  splits$positive <- dplyr::rename(splits$positive,  pos_colour_value = colour_value, pos_min = min, pos_max = max)
  splits$negative <- dplyr::rename(splits$negative,
                                   neg_colour_value =  colour_value, neg_min = min, neg_max = max)

  joined_df <- splits$negative %>%
    dplyr::select(name, neg_colour_value, neg_max, neg_min, negative) %>%
    dplyr::left_join(splits$positive, by = "name")

  return(joined_df)
}


#' quickly add re-usable theme elements for gt plot funcs
#'
#' @return list of ggplot boiler plate theme options
#' @export
#'
#' @examples
disp_gt_theme <- function(){

  ggplot_theme <- ggplot2::theme(
    plot.title = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    legend.position = "null",
    axis.title = ggplot2::element_blank(),
    strip.text = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(size = 30),
    axis.text.y = ggplot2::element_text(angle = 45,
                                        hjust = 1),
    plot.margin = ggplot2::margin(30, 0, 0, 0)
  )
}



#' Title
#'
#' @param df
#' @param date_var
#' @param time_unit
#' @param bar_colour
#' @param date_breaks
#' @param date_labels
#'
#' @return
#' @export
#'
#' @examples
disp_gt_vot <- function(df, date_var,  time_unit = c("day", "week", "month", "quarter", "year"), bar_colour =  "#628EFD", date_breaks = "4 months",date_labels = "%b"){

  unit <- match.arg(time_unit)
  date_sym <- rlang::ensym(date_var)

  df <- df %>%
    dplyr::mutate(plot_date = as.Date(!!date_sym),
                  plot_date = lubridate::floor_date(plot_date, unit = unit))

  plot <- df %>%
    dplyr::count(plot_date) %>%
    ggplot2::ggplot(ggplot2::aes(x = plot_date, y = n)) +
    ggplot2::geom_col(fill = bar_colour) +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_date(date_breaks = date_breaks, date_labels = date_labels) +
    disp_gt_theme()

  return(plot)
}



#' Title
#'
#' @param df
#' @param sentiment_var
#' @param date_var
#' @param chart_type
#' @param time_unit
#' @param date_breaks
#' @param date_labels
#'
#' @return
#' @export
#'
#' @examples
disp_gt_sent_time <- function(df,
                         sentiment_var = sentiment,
                         date_var = date,
                         chart_type = c("lines", "bars"),
                         time_unit = c("week", "day","month", "quarter", "year"),
                         date_breaks = "4 months",
                         date_labels = "%b"
){

  unit <- match.arg(time_unit)
  chart_type <- match.arg(chart_type)

  sent_sym <- rlang::ensym(sentiment_var)
  date_sym <- rlang::ensym(date_var)

  sent_string <- rlang::as_string(sent_sym)
  date_string <- rlang::as_string(date_sym)

  if(!sent_string %in% colnames(df)){
    stop(paste0("Cannot find '", sent_string, "' in the data frame, did you mean `sentiment_var = sentiment`?"))
  }
  if(!date_string %in% colnames(df)){
    stop(paste0("Cannot find '", date_string, "' in the data frame, did you mean `date_var = date`?"))
  }

  df <- df %>% dplyr:: mutate(
    plot_date = as.Date(!!date_sym),
    plot_date = lubridate::floor_date(plot_date, unit = unit),
    !!sent_sym := tolower(!!sent_sym))

  plot <- df %>%
    dplyr::count(plot_date,!!sent_sym) %>%
    ggplot2::ggplot(ggplot2::aes(x = plot_date, y = n, fill = !!sent_sym, colour = !!sent_sym))

  if(chart_type == "lines"){
    plot <- plot +
      ggplot2::geom_line()
  } else { plot <- plot +
    ggplot2::geom_col()
  }
  plot <- plot +
    ggplot2::scale_x_date(date_breaks = date_breaks, date_labels = date_labels) +
    ggplot2::scale_fill_manual(aesthetics = c("colour", "fill"),
                               values = c("positive" = "#1b7837",
                                          "negative" = "#762a83",
                                          "neutral" = "black")) +
    disp_gt_theme()

  return(plot)
}
