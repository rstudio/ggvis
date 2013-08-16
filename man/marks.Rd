\name{marks}
\alias{mark_arc}
\alias{mark_area}
\alias{mark_image}
\alias{mark_line}
\alias{mark_rect}
\alias{mark_symbol}
\alias{mark_text}
\alias{marks}
\title{Vega marks.}
\usage{
  mark_symbol(props = NULL, data = NULL)

  mark_image(props = NULL, data = NULL)

  mark_arc(props = NULL, data = NULL)

  mark_area(props = NULL, data = NULL)

  mark_line(props = NULL, data = NULL)

  mark_rect(props = NULL, data = NULL)

  mark_text(props = NULL, data = NULL)
}
\arguments{
  \item{props}{A \code{\link{props}} object, named
  according to the properties listed below.}

  \item{data}{An optional dataset, if you want to override
  the usual data inheritance for this mark.}
}
\description{
  These functions create mark objects, corresponding to
  vega marks. Marks are leaves in the plot tree, and
  control the details of the final rendering. Marks are
  equivalent to the basic geoms in ggplot2 (e.g. point,
  line, polygon), where ggvis branches correspond to
  combinations of geoms and statistical transforms.
}
\section{Properties}{
  You can set the following mark properties:

  \itemize{ \item x The first (typically left-most)
  x-coordinate. \item x2 The second (typically right-most)
  x-coordinate. \item width The width of the mark (if
  supported). \item y The first (typically top-most)
  y-coordinate. \item y2 The second (typically bottom-most)
  y-coordinate. \item height The height of the mark (if
  supported). \item opacity The overall opacity. \item fill
  The fill color. \item fillOpacity The fill opacity \item
  stroke The stroke color. \item strokeWidth The stroke
  width, in pixels. \item strokeOpacity The stroke opacity.
  \item size [symbol] The pixel area of the symbol. For
  example in the case of circles, the radius is determined
  in part by the square root of the size value. \item shape
  [symbol] The symbol shape to use. One of circle
  (default), square, cross, diamond, triangle-up, or
  triangle-down (symbol only) \item innerRadius [arc] The
  inner radius of the arc, in pixels. \item outerRadius
  [arc] The outer radius of the arc, in pixels. \item
  startAngle [arc] The start angle of the arc, in radians.
  \item endAngle [arc] The end angle of the arc, in
  radians. \item interpolate [area, line] The line
  interpolation method to use. One of linear, step-before,
  step-after, basis, basis-open, cardinal, cardinal-open,
  monotone. \item tension [area, line] Depending on the
  interpolation type, sets the tension parameter. \item url
  [image] The URL from which to retrieve the image. \item
  align [image, text] The horizontal alignment of the
  object. One of left, right, center. \item baseline
  [image, text] The vertical alignment of the object. One
  of top, middle, bottom. \item text [text] The text to
  display. \item dx [text] The horizontal margin, in
  pixels, between the text label and its anchor point. The
  value is ignored if the align property is center. \item
  dy [text] The vertical margin, in pixels, between the
  text label and its anchor point. The value is ignored if
  the baseline property is middle. \item angle [text] The
  rotation angle of the text, in degrees. \item font [text]
  The typeface to set the text in (e.g., Helvetica Neue).
  \item fontSize [text] The font size, in pixels. \item
  fontWeight [text] The font weight (e.g., bold). \item
  fontStyle [text] The font style (e.g., italic). }

  To each property, you can assign any property object
  (\code{\link{prop}}) either locally (i.e. in the mark),
  or in a parent \code{\link{node}}.
}

