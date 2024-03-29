% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_huggingface_zero_shot.R
\name{tidy_huggingface_zeroshot}
\alias{tidy_huggingface_zeroshot}
\title{Tidy a zero-shot classifier output and join with original text variable}
\usage{
tidy_huggingface_zeroshot(text_df, model_output, num_labels)
}
\arguments{
\item{text_df}{The text variable we fed into Huggingface}

\item{model_output}{Huggingface's model output with sequence, label, score}

\item{num_labels}{The number of labels given to the zero-shot classifier}
}
\value{
tibble
}
\description{
Tidy a zero-shot classifier output and join with original text variable
}
