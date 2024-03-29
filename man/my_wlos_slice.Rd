% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wlo_new_slicer.R
\name{my_wlos_slice}
\alias{my_wlos_slice}
\title{Quick re-work of function ahead of going into work packages}
\usage{
my_wlos_slice(
  df,
  topic_var,
  text_var,
  top_n = 30,
  nrow = 4,
  top_terms_cutoff = 1500
)
}
\arguments{
\item{df}{Data Frame or Tibble object}

\item{topic_var}{The Grouping variable}

\item{text_var}{The text variable}

\item{top_n}{How many terms per group to plot, arranged by weighted-log odds ratio}

\item{nrow}{How many rows to show the plot on}

\item{top_terms_cutoff}{How many of the top-frequency terms to select top_n from}
}
\value{
list with viz and view objects
}
\description{
Quick re-work of function ahead of going into work packages
}
