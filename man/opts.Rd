\name{opts}
\alias{opts}
\title{Set options for a ggvis plot}
\usage{
  opts(width = NULL, height = NULL, auto_size = NULL,
    keep_aspect = NULL, resizable = NULL, padding = NULL,
    duration = NULL)
}
\arguments{
  \item{width,height}{Width and height of plot, in pixels.
  Default is 400x400.}

  \item{auto_size}{Should the plot be resized according to
  the window size? If FALSE, the plot size will be set to
  \code{height} and \code{width}, regardless of window
  size. If TRUE, the plot size will be sized to the window
  or to \code{width} and \code{height}, whichever is
  smaller.  The default value is \code{TRUE}, or the value
  of \code{getOption("ggvis.auto_size")}, if it is set.}

  \item{keep_aspect}{If the plot is auto-sized, should the
  aspect ratio be preserved? (This is only useful when
  \code{auto_size} is \code{TRUE}.)  The default value is
  \code{FALSE}, or the value of
  \code{getOption("ggvis.keep_aspect")}, if it is set.}

  \item{resizable}{If TRUE, allow the user to resize the
  plot.  The default value is \code{TRUE}, or the value of
  \code{getOption("ggvis.resizable")}, if it is set.}

  \item{padding}{A padding object specifying padding on the
  top, right, left, and bottom. See \code{\link{padding}}.}

  \item{duration}{Duration of transitions, in
  milliseconds.}
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

