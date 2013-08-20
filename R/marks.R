#' Vega marks.
#'
#' These functions create mark objects, corresponding to vega marks. Marks
#' are leaves in the plot tree, and control the details of the final rendering.
#' Marks are equivalent to the basic geoms in ggplot2 (e.g. point, line,
#' polygon), where ggvis branches correspond to combinations of geoms and
#' statistical transforms.
#'
#' @section Properties:
#'
#' You can set the following mark properties:
#'
#' \itemize{
#' \item  x  The first (typically left-most) x-coordinate.
#' \item  x2 The second (typically right-most) x-coordinate.
#' \item  width The width of the mark (if supported).
#' \item  y The first (typically top-most) y-coordinate.
#' \item  y2 The second (typically bottom-most) y-coordinate.
#' \item  height The height of the mark (if supported).
#' \item  opacity The overall opacity.
#' \item  fill The fill color.
#' \item  fillOpacity The fill opacity
#' \item  stroke The stroke color.
#' \item  strokeWidth The stroke width, in pixels.
#' \item  strokeOpacity The stroke opacity.
#' \item  size [symbol] The pixel area of the symbol. For example in the case
#'   of circles, the radius is determined in part by the square root of the size
#'   value.
#' \item  shape [symbol] The symbol shape to use. One of circle (default),
#'   square, cross, diamond, triangle-up, or triangle-down (symbol only)
#' \item  innerRadius [arc] The inner radius of the arc, in pixels.
#' \item  outerRadius [arc] The outer radius of the arc, in pixels.
#' \item  startAngle [arc] The start angle of the arc, in radians.
#' \item  endAngle [arc] The end angle of the arc, in radians.
#' \item  interpolate [area, line] The line interpolation method to use. One
#'   of linear, step-before, step-after, basis, basis-open, cardinal,
#'   cardinal-open, monotone.
#' \item  tension [area, line] Depending on the interpolation type, sets the
#'   tension parameter.
#' \item  url [image] The URL from which to retrieve the image.
#' \item  align [image, text] The horizontal alignment of the object. One of
#'   left, right, center.
#' \item  baseline [image, text] The vertical alignment of the object. One of
#'   top, middle, bottom.
#' \item  text [text] The text to display.
#' \item  dx [text] The horizontal margin, in pixels, between the text label
#'   and its anchor point. The value is ignored if the align property is center.
#' \item  dy [text] The vertical margin, in pixels, between the text label
#'   and its anchor point. The value is ignored if the baseline property is
#'   middle.
#' \item  angle [text] The rotation angle of the text, in degrees.
#' \item  font [text] The typeface to set the text in (e.g., Helvetica Neue).
#' \item  fontSize [text] The font size, in pixels.
#' \item  fontWeight [text] The font weight (e.g., bold).
#' \item  fontStyle [text] The font style (e.g., italic).
#' }
#'
#' To each property, you can assign any property object (\code{\link{prop}})
#' either locally (i.e. in the mark), or in a parent \code{\link{node}}.
#' @name marks
#' @param props A \code{\link{props}} object, named according to the
#'   properties listed below.
#' @param data An optional dataset, if you want to override the usual data
#'   inheritance for this mark.
NULL

# Return a character vector of valid properties for a given mark
valid_mark_properties <- function(mark) UseMethod("valid_mark_properties")
#' @S3method valid_mark_properties default
valid_mark_properties.default <- function(mark) {
  stop("Unknown mark type: ", paste(class(mark), collapse=", "))
}

# Return a named list of default properties for a mark.
default_mark_properties <- function(mark) UseMethod("default_mark_properties")
#' @S3method default_mark_properties default
default_mark_properties.default <- function(mark) {
  stop("Unknown mark type: ", paste(class(mark), collapse=", "))
}


#' @rdname marks
#' @export
mark_symbol <- function(props = NULL, data = NULL) mark("symbol", props = props, data = data)
#' @S3method valid_mark_properties mark_symbol
valid_mark_properties.mark_symbol <- function(mark) {
  c("x", "y", "opacity", "fill", "fillOpacity", "stroke", "strokeWidth",
    "strokeOpacity", "size", "shape")
}
#' @S3method default_mark_properties mark_symbol
default_mark_properties.mark_symbol <- function(mark) {
  props(fill = "#000000")
}


#' @rdname marks
#' @export
mark_image <- function(props = NULL, data = NULL) mark("image", props = props, data = data)
#' @S3method valid_mark_properties mark_image
valid_mark_properties.mark_image <- function(mark) {
  c("x", "y", "opacity", "fill", "fillOpacity", "stroke", "strokeWidth",
    "strokeOpacity", "url", "align", "baseline")
}
#' @S3method default_mark_properties mark_image
default_mark_properties.mark_image <- function(mark) {
  props(fill = "#000000")
}


#' @rdname marks
#' @export
mark_arc <- function(props = NULL, data = NULL) mark("arc", props = props, data = data)
#' @S3method valid_mark_properties mark_arc
valid_mark_properties.mark_arc <- function(mark) {
  c("x", "y", "opacity", "fill", "fillOpacity", "stroke", "strokeWidth",
    "strokeOpacity", "innerRadius", "outerRadius", "startAngle", "endAngle")
}
#' @S3method default_mark_properties mark_arc
default_mark_properties.mark_arc <- function(mark) {
  props(fill = "#333333")
}


#' @rdname marks
#' @export
mark_area <- function(props = NULL, data = NULL) mark("area", props = props, data = data)
#' @S3method valid_mark_properties mark_area
valid_mark_properties.mark_area <- function(mark) {
  c("x", "y", "y2", "height", "opacity", "fill", "fillOpacity", "stroke", 
    "strokeWidth", "strokeOpacity", "interpolate", "tension")
}
#' @S3method default_mark_properties mark_area
default_mark_properties.mark_area <- function(mark) {
  props(fill = "#333333")
}


#' @rdname marks
#' @export
mark_line <- function(props = NULL, data = NULL) mark("line", props = props, data = data)
#' @S3method valid_mark_properties mark_line
valid_mark_properties.mark_line <- function(mark) {
  c("x", "y", "opacity", "fill", "fillOpacity", "stroke", "strokeWidth",
    "strokeOpacity", "interpolate", "tension")
}
#' @S3method default_mark_properties mark_line
default_mark_properties.mark_line <- function(mark) {
  props(stroke = "#000000")
}

#' @export
#' @rdname marks
mark_rect <- function(props = NULL, data = NULL) mark("rect", props = props, data = data)
#' @S3method valid_mark_properties mark_rect
valid_mark_properties.mark_rect <- function(mark) {
  c("x", "x2", "y", "y2", "width", "opacity", "fill", "fillOpacity", "stroke",
    "strokeWidth", "strokeOpacity")
}
#' @S3method default_mark_properties mark_rect
default_mark_properties.mark_rect <- function(mark) {
  props(stroke = "#000000", fill = "#333333")
}

#' @export
#' @rdname marks
mark_text <- function(props = NULL, data = NULL) mark("text", props = props, data = data)
#' @S3method valid_mark_properties mark_text
valid_mark_properties.mark_text <- function(mark) {
  c("x", "y", "text", "opacity", "fill", "fillOpacity", "stroke",
    "strokeWidth", "strokeOpacity", "align", "baseline", "dx", "dy",
    "angle", "font", "fontSize", "fontWeight", "fontStyle")
}
#' @S3method default_mark_properties mark_text
default_mark_properties.mark_text <- function(mark) {
  props(fill = "#333333")
}
