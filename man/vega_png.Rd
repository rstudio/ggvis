\name{vega_png}
\alias{vega_png}
\title{Generate an PNG file from a ggvis object}
\usage{
  vega_png(gv, envir = parent.frame(), file = NULL)
}
\arguments{
  \item{gv}{A ggvis object.}

  \item{envir}{The environment in which to evaluate
  \code{gv}.}

  \item{file}{Output file name. If NULL, defaults to
  "plot.png".}
}
\description{
  This requires that the program \code{vg2png} is
  installed. This is part of \code{vega} node.js module.
}
\seealso{
  \url{https://github.com/trifacta/vega} for information on
  installing \code{vg2png}.
}

