#' Save a list of plots
#'
#' @param plot_list Should be a named list of plots
#' @param plot_type For saving prefix that gets pasted with name and .png = e.g. "bigram_microsoft_topic_1.png"
#'
#' @return does not return anything - saves plots to working directory
#' @export
#'
save_plot_list <- function(plot_list, plot_type = "bigram"){
  lapply(names(plot_list),function(x) ggplot2::ggsave(filename = paste(plot_type, "_", x, ".png", sep = ""), bg = "white", plot = bigrams_list[[x]]))
}
