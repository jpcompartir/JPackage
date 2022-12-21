#' Save a list of plots to the current working directory
#'
#' Set your working directory to the folder you want to save your plots to with `setwd("~/foo/bar")` then call the function on your list of plots.
#'
#' @param plot_list Should be a named list of plots
#' @param plot_type For saving prefix that gets pasted with name and .png = e.g. "bigram_microsoft_topic_1.png"
#' @param dpi Resolution of saved image - higher number = higher quality
#' @param width Plot width in specified units
#' @param height Plot width in specified units
#' @param units the appropriate unit of measurement for plot dimensions
#' @param dir the directory to save plots in (defaults to current working directory)
#'
#' @return does not return anything - saves plots to working directory
#' @export
#' @examples
#' \dontrun{
#' df <- ParseR::sprinklr_export %>% janitor::clean_names()
#' bigrams_list <- df %>% group_split(social_network) %>% map(make_bigram_viz, message, .progress = TRUE)
#' names(bigrams_list) <- c("1", "2", "3")
#' dir.create("test_dir")
#' save_plot_lists(plot_list = bigrams_list, dir = "test_dir")
#' }
save_plot_list <- function(plot_list, dir = NULL, plot_type = "bigram", dpi = 700, width = 9, height = 6, units = c("in", "cm", "mm", "px")){


  #Make the directory the current working directory if not set
  if(is.null(dir)){
    dir <- paste0(getwd(), "/")
  } else{
    if(!is.character(dir)){ #Check that directory is a character as it should be
      stop("dir argument should be left blank or be input as a character vector")
    }
  }
  #If the dirextory does not end with a /, add one
  if(!endsWith(dir, "/")){dir <- paste0(dir, "/")}

  #Set default as "in" and if not match with input of units
  units <- match.arg(if (missing(units)) "in" else units, c("in", "cm", "mm", "px"))

  lapply(names(plot_list),function(x) ggplot2::ggsave(filename = paste(dir, plot_type, "_", x, ".png", sep = ""), width = width, height = height, units = units, dpi = dpi, bg = "white", plot = plot_list[[x]]))
}

