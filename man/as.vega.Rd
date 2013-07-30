\name{as.vega}
\alias{as.vega}
\alias{as.vega.gigvis}
\title{Coerce an gigvis object to a vega list.}
\usage{
  as.vega(x, ...)

  \method{as.vega}{gigvis} (x, width = 600, height = 400,
    padding = NULL)
}
\arguments{
  \item{x}{an object to convert to vega}

  \item{width,height}{width and height of plot, in pixels}

  \item{padding}{padding, as described by
  \code{\link{padding}}}
}
\value{
  a list. When converted to JSON, will be the type of
  structure that vega expects.
}
\description{
  This generic function powers the coercion of gigvis
  objects to vega compatible data structures.
}

