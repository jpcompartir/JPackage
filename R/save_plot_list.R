#' Save a list of plots to the current working directory
#'
#' Set your working directory to the folder you want to save your plots to with `setwd("~/foo/bar")` then call the function on your list of plots.
#'
#' @param plot_list Should be a named list of plots
#' @param plot_type For saving prefix that gets pasted with name and .png = e.g. "bigram_microsoft_topic_1.png"
#' @param dpi Resolution of saved image - higher number = higher quality
#'
#' @return does not return anything - saves plots to working directory
#' @export
#'
save_plot_list <- function(plot_list, plot_type = "bigram", dpi = 700){
  lapply(names(plot_list),function(x) ggplot2::ggsave(filename = paste(plot_type, "_", x, ".png", sep = ""), dpi = dpi, bg = "white", plot = plot_list[[x]]))
}
