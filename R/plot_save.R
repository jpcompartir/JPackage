
#' Save plots using ggsave but with some default arguments sorted
#
#' @param plot Which plot to save? Either an object or default to the `last_plot()`
#' @param dir What should the plot be saved as, and where?
#' @param height Plot's height (default in inches)
#' @param width Plot's width (default in inches)
#' @param dpi Resolution
#' @param units Measurement unit e.g. "in" = inches
#'
#' @return Saves a plot in the appropriate directory
#' @export
#'
#' @examples
#' \dontrun{
#' mtcars %>%
#'   ggplot(aes(x= cyl, y = mpg)) +
#'   geom_point()
#'
#' plot_save("viz/imaginary_plot.png", dpi = 700)
#' }
plot_save <- function(plot = ggplot2::last_plot(), dir, height = 6, width = 8, dpi = 320, units = c("in", "cm", "mm", "px")){

  #Set default as "in" and if not match with input of units
  units <- match.arg(if (missing(units)) "in" else units, c("in", "cm", "mm", "px"))

  plot %>%
    ggplot2::ggsave(filename = dir, dpi = dpi, bg = "white", height = height, width = width, units = units)
}
