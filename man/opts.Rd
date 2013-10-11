\name{opts}
\alias{opts}
\title{Define size of plot.}
\usage{
  opts(width = NULL, height = NULL, auto_size = NULL,
    keep_aspect = TRUE, resizable = NULL, padding = NULL,
    duration = NULL)
}
\arguments{
  \item{width,height}{Width and height of plot, in pixels.}

  \item{auto_size}{Should the plot be resized according to
  the window size? If FALSE, the plot size will be set to
  \code{height} and \code{width}, regardless of window
  size. If TRUE, the plot size will be sized to the window
  or to \code{width} and \code{height}, whichever is
  smaller.}

  \item{keep_aspect}{If the plot is auto-sized, should the
  aspect ratio be preserved? (Only useful when
  \code{auto_size} is \code{TRUE}.)}

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

