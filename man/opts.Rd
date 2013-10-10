\name{opts}
\alias{opts}
\title{Define size of plot.}
\usage{
  opts(width = NULL, height = NULL, auto_size = NULL,
    resizable = NULL, padding = NULL, duration = NULL)
}
\arguments{
  \item{width,height}{Width and height of plot, in pixels.}

  \item{auto_size}{What do do with the width and height
  values, in relation to the window size. If
  \code{"fixed"}, the plot size will be set to
  \code{height} and \code{width}, regardless of window
  size. If \code{"fit"}, the plot size will be sized to the
  window or to \code{width} and \code{height}, whichever is
  smaller.}

  \item{resizable}{If TRUE, allow the user to resize the
  plot.}

  \item{padding}{A padding object specifying padding on the
  top, right, left, and bottom. See \code{\link{padding}}.}

  \item{duration}{Duration of transitions, in
  milliseconds.}
}
\description{
  Define size of plot.
}
\examples{
ggvis(mtcars, props(x = ~wt, y = ~mpg), mark_symbol(),
  opts(width = 300, height = 200, padding = padding(10, 10, 10, 10)))
}

