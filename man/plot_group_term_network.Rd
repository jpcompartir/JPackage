% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_group_term_network.R
\name{plot_group_term_network}
\alias{plot_group_term_network}
\title{Function to view top terms associated with a categorical variable.}
\usage{
plot_group_term_network(
  data,
  group_var = brand,
  text_var = message,
  n_terms = 20,
  with_ties = FALSE
)
}
\arguments{
\item{data}{Your data frame - should have text variable and a grouping variable coded in.}

\item{group_var}{The variable you wish to compare, should be a categorical or factor e.g. brand, entity, sentiment, country}

\item{text_var}{The text variable}

\item{n_terms}{Number of terms to select for each}

\item{with_ties}{Whether to allow for > \code{n_terms} if terms have equal frequency in \code{group_var}'s count}
}
\value{
A network visualisation of terms associated with a grouping variable
}
\description{
Counts the top terms that appear in posts with a grouping variable.
Individual instances of grouping variable (e.g. Nike, Adidas) who have common words
in their top \code{n_terms} will be placed by graph-drawing algorithm closer to those they have none in common with.
Terms common to all or multiple grouping variables will be drawn towards the middle of the network.
Terms that are uniquely associated with a specific grouping variable will be draw outwards towards the periphery of the network.
}
\examples{
\dontrun{
Example 1:
plot_group_term_network(data, group_var = brand, text_var = text, n_terms = 15)

Example 2:
plot_group_term_network(my_data_frame, group_var = sentiment, text_var = message, n_terms = 15)
}
}
