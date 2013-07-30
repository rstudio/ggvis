
mark <- function(type, ..., data = NULL) {
  m <- structure(
    compact(list(
      type = type, 
      data = as.pipeline(data), 
      props = props(...)
    )),
    class = c(paste0("mark_", type), "mark", "gigvis_node")
  )

  check_mark_props(m, names(m$props))
  m
}

#' export
is.mark <- function(x) inherits(x, "mark")


#' @S3method format mark
format.mark <- function(x, ...) {
  paste0("<", class(x)[1], ">", 
    if (!is.null(x$pipeline_id)) paste0(" (ID: ", x$pipeline_id, ")"),
    "\n",
    format(x$props))
}

#' @S3method print mark
print.mark <- function(x, ...) cat(format(x), "\n")


#' Vega mark properties.
#'
#' These functions define the properties that that each vega mark has. To
#' each of this you can assign any property (\code{\link{prop}}) either
#' locally (i.e. in the mark function), or in a parent \code{\link{node}}.
#' There are many available property types to choose from:
#' \code{\link{variable}} and \code{\link{constant}} are the most commonly
#' used.
#'
#' @param x  The first (typically left-most) x-coordinate.
#' @param x2 The second (typically right-most) x-coordinate.
#' @param width The width of the mark (if supported).
#' @param y The first (typically top-most) y-coordinate.
#' @param y2 The second (typically bottom-most) y-coordinate.
#' @param height The height of the mark (if supported).
#' @param opacity The overall opacity.
#' @param fill The fill color.
#' @param fillOpacity The fill opacity
#' @param stroke The stroke color.
#' @param strokeWidth The stroke width, in pixels.
#' @param strokeOpacity The stroke opacity.
#' @param size [symbol] The pixel area of the symbol. For example in the case
#'   of circles, the radius is determined in part by the square root of the size
#'   value.
#' @param shape [symbol] The symbol shape to use. One of circle (default),
#'   square, cross, diamond, triangle-up, or triangle-down (symbol only)
#' @param innerRadius [arc] The inner radius of the arc, in pixels.
#' @param outerRadius [arc] The outer radius of the arc, in pixels.
#' @param startAngle [arc] The start angle of the arc, in radians.
#' @param endAngle [arc] The end angle of the arc, in radians.
#' @param interpolate [area, line] The line interpolation method to use. One
#'   of linear, step-before, step-after, basis, basis-open, cardinal,
#'   cardinal-open, monotone.
#' @param tension [area, line] Depending on the interpolation type, sets the
#'   tension parameter.
#' @param url [image] The URL from which to retrieve the image.
#' @param align [image, text] The horizontal alignment of the object. One of
#'   left, right, center.
#' @param baseline [image, text] The vertical alignment of the object. One of
#'   top, middle, bottom.
#' @param text [text] The text to display.
#' @param dx [text] The horizontal margin, in pixels, between the text label
#'   and its anchor point. The value is ignored if the align property is center.
#' @param dy [text] The vertical margin, in pixels, between the text label
#'   and its anchor point. The value is ignored if the baseline property is
#'   middle.
#' @param angle [text] The rotation angle of the text, in degrees.
#' @param font [text] The typeface to set the text in (e.g., Helvetica Neue).
#' @param fontSize [text] The font size, in pixels.
#' @param fontWeight [text] The font weight (e.g., bold).
#' @param fontStyle [text] The font style (e.g., italic).
#' @name marks
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
mark_symbol <- function(...) mark("symbol", ...)
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
mark_image <- function(...) mark("image", ...)
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
mark_arc <- function(...) mark("arc", ...)
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
mark_area <- function(...) mark("area", ...)
#' @S3method valid_mark_properties mark_area
valid_mark_properties.mark_area <- function(mark) {
  c("x", "y", "opacity", "fill", "fillOpacity", "stroke", "strokeWidth",
    "strokeOpacity", "interpolate", "tension")
}
#' @S3method default_mark_properties mark_area
default_mark_properties.mark_area <- function(mark) {
  props(fill = "#333333")
}


#' @rdname marks
#' @export
mark_line <- function(...) mark("line", ...)
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
mark_rect <- function(...) mark("rect", ...)
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
mark_text <- function(...) mark("text", ...)
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
