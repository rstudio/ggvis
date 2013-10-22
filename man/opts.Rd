\name{opts}
\alias{opts}
\title{Set options for a ggvis plot}
\usage{
  opts(width = NULL, height = NULL, smart_size = NULL,
    keep_aspect = NULL, resizable = NULL, padding = NULL,
    duration = NULL, renderer = NULL)
}
\arguments{
  \item{width,height}{Width and height of plot, in pixels.
  Default is 600x400.}

  \item{smart_size}{If \code{TRUE}, ggvis will try to
  auto-detect if it is running in a small viewer window,
  and if so, will automatically size the plot to fill the
  window. The default value is \code{TRUE}, or the value of
  \code{getOption("ggvis.smart_size")}, if it is set.}

  \item{keep_aspect}{Should the aspect ratio be preserved?
  The default value is \code{FALSE}, or the value of
  \code{getOption("ggvis.keep_aspect")}, if it is set.}

  \item{resizable}{If TRUE, allow the user to resize the
  plot.  The default value is \code{TRUE}, or the value of
  \code{getOption("ggvis.resizable")}, if it is set.}

  \item{padding}{A padding object specifying padding on the
  top, right, left, and bottom. See \code{\link{padding}}.}

  \item{duration}{Duration of transitions, in
  milliseconds.}

  \item{renderer}{The renderer to use in the browser. Can
  be \code{"canvas"} (the default) or \code{"svg"}.}
}
\description{
  Set options for a ggvis plot
}
\examples{
ggvis(mtcars, props(x = ~wt, y = ~mpg), mark_symbol(),
  opts(width = 300, height = 200, padding = padding(10, 10, 10, 10)))
}
\seealso{
  \code{link{getOption}} and \code{link{options}}, for
  getting and setting global options.
}

