\name{source_function}
\alias{source_function}
\title{A function source.}
\usage{
  source_function(func, name = NULL)
}
\arguments{
  \item{data}{A function that returns a data frame.}
}
\description{
  A function source is evaluated when the plot is created.
}
\examples{
mt <- function() mtcars
s <- source_function(mt)
sluice(s, props())
}

