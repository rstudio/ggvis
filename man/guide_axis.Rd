\name{guide_axis}
\alias{guide_axis}
\title{Generate a vega axis specification}
\usage{
  guide_axis(type, scale = type, orient = NULL,
    title = NULL, title_offset = 0, format = NULL,
    ticks = NULL, values = NULL, subdivide = NULL,
    tick_padding = NULL, tick_size_major = NULL,
    tick_size_minor = tick_size_major,
    tick_size_end = tick_size_major, offset = NULL,
    layer = "back", grid = TRUE, properties = list())
}
\arguments{
  \item{type}{The type of axis. Either x or y.}

  \item{scale}{The name of the scale backing the axis
  component. Defaults to the scale type - you will need to
  specify if you want (e.g.) a scale for a secondary
  y-axis.}

  \item{orient}{The orientation of the axis. One of top,
  bottom, left or right.  The orientation can be used to
  further specialize the axis type (e.g., a y axis oriented
  for the right edge of the chart) - defaults to bottom for
  x axes, and left for y axes.}

  \item{title}{A title for the axis (none by default).}

  \item{title_offset}{The offset (in pixels) from the axis
  at which to place the title.}

  \item{format}{The formatting pattern for axis labels.
  Vega uses D3's format pattern:
  \url{https://github.com/mbostock/d3/wiki/Formatting}}

  \item{ticks}{A desired number of ticks. The resulting
  number may be different so that values are "nice"
  (multiples of 2, 5, 10) and lie within the underlying
  scale's range.}

  \item{values}{Explicitly set the visible axis tick
  values.}

  \item{subdivide}{If provided, sets the number of minor
  ticks between major ticks (the value 9 results in decimal
  subdivision).}

  \item{tick_padding}{The padding, in pixels, between ticks
  and text labels.}

  \item{tick_size_major,tick_size_minor,tick_size_end}{The
  size, in pixels, of major, minor and end ticks.}

  \item{offset}{The offset, in pixels, by which to displace
  the axis from the edge of the enclosing group or data
  rectangle.}

  \item{layer}{A string indicating if the axis (and any
  gridlines) should be placed above or below the data
  marks. One of "front" or "back" (default).}

  \item{grid}{A flag indicating if gridlines should be
  created in addition to ticks.}

  \item{properties}{Optional mark property definitions for
  custom axis styling.  Should be a named list (ticks,
  majorTicks, minorTicks, labels and axis) of
  \code{\link{props}}.}
}
\description{
  Generate a vega axis specification
}
\examples{
guide_axis("x")
guide_axis("x", properties = list(ticks = props(stroke = "red")))
}

