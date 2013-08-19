\name{guide_legend}
\alias{guide_legend}
\title{Generate a vega legend specification}
\usage{
  guide_legend(size = NULL, shape = NULL, fill = NULL,
    stroke = NULL, orient = "right", title = NULL,
    format = NULL, values = NULL, properties = NULL)
}
\arguments{
  \item{size,shape,fill,stroke}{The name of the scale that
  determines the legends size, shape, fill and stroke.}

  \item{orient}{The orientation of the legend. One of
  "left" or "right". This determines how the legend is
  positioned within the scene. The default is "right".}

  \item{title}{The title for the legend (none by default).}

  \item{format}{The formatting pattern for axis labels.
  Vega uses D3's format pattern:
  \url{https://github.com/mbostock/d3/wiki/Formatting}}

  \item{values}{Explicitly set the visible legend values.}

  \item{properties}{Optional mark property definitions for
  custom legend styling. Should be a named list (title,
  label, symbols, gradient, legend) of
  \code{\link{props}}.}
}
\description{
  Generate a vega legend specification
}
\examples{
guide_legend(size = "size")
}

