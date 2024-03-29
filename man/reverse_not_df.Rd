% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy-dist-mat.R
\name{reverse_not_df}
\alias{reverse_not_df}
\alias{reverse_not_df.not_df}
\title{removes the \code{not}() operation (that makes a df a \code{not_df})}
\usage{
reverse_not_df(x)

\method{reverse_not_df}{not_df}(x)
}
\arguments{
\item{x}{\code{not_df} class object}
}
\value{
x without the \code{not_df} class associated with it
}
\description{
removes the \code{not}() operation (that makes a df a \code{not_df})
}
