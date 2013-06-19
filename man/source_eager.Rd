\name{source_eager}
\alias{source_eager}
\title{An eager source.}
\usage{
  source_eager(data, name = NULL)
}
\arguments{
  \item{data}{a data frame}

  \item{name}{the name of the data frame (used in error
  messages etc.)}
}
\description{
  An eager source captures the data frame immediately when
  it is called. This is useful when you are constructing
  plot components inside a function because they will be
  self-contained, but creates heavy weight gigvis objects.
  See \code{\link{source_lazy}} for a light-weight (but not
  self- contained) alternative
}
\examples{
source_eager(mtcars)
}

