\name{props}
\alias{is.ggvis_props}
\alias{props}
\title{Manage a list of properties.}
\usage{
  props(..., inherit = TRUE)

  is.ggvis_props(x)
}
\arguments{
  \item{...}{A set of name-value pairs. The name should be
  a valid vega property.}

  \item{inherit}{If \code{TRUE}, the defaults, will inherit
  from properties from the parent node. If \code{FALSE}, it
  will start from nothing.}

  \item{x}{an object to test for props-ness.}
}
\description{
  \code{props()} provides a tool for concise creation of
  \code{prop} objects using a set of conventions designed
  to capture the most common use cases. If you need
  something less common, you'll need to use
  \code{\link{prop}} to access all possible options. See
  the \link{marks} documentation to see what properties the
  marks understand.
}
\section{Heuristics}{
  If the values are not already objects of class
  \code{prop}, \code{props} uses the following heuristics
  to when creating the prop:

  \itemize{ \item atomic vectors, e.g. \code{x = 1}: scaled
  = FALSE \item an interative input, e.g. \code{x =
  input_slider}: scaled = FALSE \item a formula containing
  a single value, e.g. \code{x ~ 1}: scaled = TRUE \item a
  formula containing a name or expression, \code{x ~ mpg}:
  scaled = TRUE }
}

\section{Non-standard evaluation}{
  \code{props} uses non-standard evaluation in a slightly
  unusual way: if you provide a formula input, the LHS of
  the formula will provide the name of the component. In
  otherwise, \code{props(x = y ~ 1)} is the same as
  \code{props(y ~ 1)}.

  You can combine variables from the dataset and variables
  defined in the local environment: expressions will be
  evaluated in the environment which the formula was
  defined.

  If you have the name of a variable in a string, see the
  props vignette for how to create the needed property
  mapping.
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
\examples{
# Set to constant values
props(x = 1, y = 2)
# Map to variables in the dataset
props(x ~ mpg, y ~ cyl)
# Set to a constant value in the data space
props(x ~ 1, y ~ 1)
# Use an interactive slider
props(opacity = input_slider(0, 1))
# Use any other prop settings
props(x = prop("old", scale = TRUE, offset = -1))
}

