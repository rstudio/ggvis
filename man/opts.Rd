\name{opts}
\alias{opts}
\title{Set options for a ggvis plot}
\usage{
opts(width = NULL, height = NULL, keep_aspect = NULL, resizable = NULL,
  padding = NULL, duration = NULL, renderer = NULL,
  hover_duration = NULL, enter_duration = NULL, exit_duration = NULL,
  brush_policy = NULL, brush_delay = NULL)
}
\arguments{
  \item{width,height}{Width and height of plot, in pixels.
  Default is 600x400.}

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

  \item{hover_duration}{The amount of time for hover
  transitions, in milliseconds.}

  \item{enter_duration}{The amount of time for enter
  transitions, in milliseconds.}

  \item{exit_duration}{The amount of time for exit
  transitions, in milliseconds.}

  \item{brush_policy}{The policy for limiting the rate that
  brush update events are reported by the client to the
  server. Can be \code{"debounce"} (the default) or
  \code{"throttle"}. When debouncing, event information
  will be sent to the server <brush_delay> milliseconds
  after the last change. When throttling, event information
  will be sent at a rate no faster than once per
  <brush_delay> milliseconds.}

  \item{brush_delay}{The number of milliseconds to use with
  \code{brush_policy}.}
}
\description{
  Set options for a ggvis plot
}
\examples{
ggvis(mtcars, props(x = ~wt, y = ~mpg), mark_symbol(),
  opts(width = 300, height = 200, padding = padding(10, 10, 10, 10)))

# Display the default options
str(default_opts())
}
\seealso{
  \code{link{getOption}} and \code{link{options}}, for
  getting and setting global options.

  \code{link{default_opts}} to see the default options.
}

