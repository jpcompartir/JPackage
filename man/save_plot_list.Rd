% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save_plot_list.R
\name{save_plot_list}
\alias{save_plot_list}
\title{Save a list of plots to the current working directory}
\usage{
save_plot_list(
  plot_list,
  dir = NULL,
  plot_type = "bigram",
  dpi = 700,
  width = 9,
  height = 6,
  units = c("in", "cm", "mm", "px")
)
}
\arguments{
\item{plot_list}{Should be a named list of plots}

\item{dir}{the directory to save plots in (defaults to current working directory)}

\item{plot_type}{For saving prefix that gets pasted with name and .png = e.g. "bigram_microsoft_topic_1.png"}

\item{dpi}{Resolution of saved image - higher number = higher quality}

\item{width}{Plot width in specified units}

\item{height}{Plot width in specified units}

\item{units}{the appropriate unit of measurement for plot dimensions}
}
\value{
does not return anything - saves plots to working directory
}
\description{
Set your working directory to the folder you want to save your plots to with \code{setwd("~/foo/bar")} then call the function on your list of plots.
}
\examples{
\dontrun{
df <- ParseR::sprinklr_export \%>\% janitor::clean_names()
bigrams_list <- df \%>\% group_split(social_network) \%>\% map(make_bigram_viz, message, .progress = TRUE)
names(bigrams_list) <- c("1", "2", "3")
dir.create("test_dir")
save_plot_lists(plot_list = bigrams_list, dir = "test_dir")
}
}
