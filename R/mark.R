
mark <- function(type, ...) {
  structure(
    list(type = type, props = props(...)),
    class = c(paste0("mark_", type), "mark", "gigvis_node")
  )
}

#' @S3method print mark
print.mark <- function(x, ...) str(x)

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

#' @rdname marks
#' @export
mark_point <- function(x = NULL, y = NULL, opacity = NULL,
                       fill = NULL, fillOpacity = NULL,
                       stroke = NULL, strokeWidth = NULL, strokeOpacity = NULL,
                       size = NULL, shape = NULL) {

  mark("point", x = x, y = y, opacity = opacity,
    fill = fill, fillOpacity = fillOpacity,
    stroke = stroke, strokeWidth = strokeWidth, strokeOpacity = strokeOpacity,
    size = size, shape = shape)

}
#' @S3method vega_mark_type mark_point
vega_mark_type.mark_point <- function(mark) "symbol"
#' @S3method default_mark_properties mark_point
default_mark_properties.mark_point <- function(mark) {
  props(fill = "#000000")
}


#' @rdname marks
#' @export
mark_image <- function(x = NULL, y = NULL, opacity = NULL,
                       fill = NULL, fillOpacity = NULL,
                       stroke = NULL, strokeWidth = NULL, strokeOpacity = NULL,
                       url = NULL, align = NULL, baseline = NULL) {
  
  mark("image", x = x, y = y, opacity = opacity,
       fill = fill, fillOpacity = fillOpacity,
       stroke = stroke, strokeWidth = strokeWidth, strokeOpacity = strokeOpacity,
       url = url, align = align, baseline = baseline)
  
}
#' @S3method vega_mark_type mark_image
vega_mark_type.mark_image <- function(mark) "image"
#' @S3method default_mark_properties mark_image
default_mark_properties.mark_image <- function(mark) {
  props(fill = "#000000")
}


#' @rdname marks
#' @export
mark_arc <- function(x = NULL, y = NULL, opacity = NULL,
                     fill = NULL, fillOpacity = NULL,
                     stroke = NULL, strokeWidth = NULL, strokeOpacity = NULL,
                     innerRadius = NULL, outerRadius = NULL, 
                     startAngle = NULL, endAngle = NULL) {
  
  mark("arc", x = x, y = y, opacity = opacity,
       fill = fill, fillOpacity = fillOpacity,
       stroke = stroke, strokeWidth = strokeWidth, strokeOpacity = strokeOpacity,
       innerRadius = innerRadius, outerRadius = outerRadius, 
       startAngle = startAngle, endAngle = endAngle)
}

#' @S3method vega_mark_type mark_arc
vega_mark_type.mark_arc <- function(mark) "arc"
#' @S3method default_mark_properties mark_arc
default_mark_properties.mark_arc <- function(mark) {
  props(fill = "#333333")
}

#' @rdname marks
#' @export
mark_area <- function(x = NULL, y = NULL, opacity = NULL,
                      fill = NULL, fillOpacity = NULL,
                      stroke = NULL, strokeWidth = NULL, strokeOpacity = NULL,
                      interpolote = NULL, tension = NULL) {
  
  mark("arc", x = x, y = y, opacity = opacity,
       fill = fill, fillOpacity = fillOpacity,
       stroke = stroke, strokeWidth = strokeWidth, strokeOpacity = strokeOpacity,
       interpolote = interpolote, tension = tension)
}

#' @S3method vega_mark_type mark_area
vega_mark_type.mark_area <- function(mark) "area"
#' @S3method default_mark_properties mark_area
default_mark_properties.mark_area <- function(mark) {
  props(fill = "#333333")
}


#' @rdname marks
#' @export
mark_line <- function(x = NULL, y = NULL, opacity = NULL,
                      fill = NULL, stroke = NULL, strokeWidth = NULL,
                      strokeOpacity = NULL, interpolote = NULL, tension = NULL) {

  mark("line", x = x, y = y, opacity = opacity, fill = fill,
    stroke = stroke, strokeWidth = strokeWidth, strokeOpacity = strokeOpacity,
    interpolote = interpolote, tension = tension)
  
}
#' @S3method vega_mark_type mark_line
vega_mark_type.mark_line <- function(mark) "line"
#' @S3method default_mark_properties mark_line
default_mark_properties.mark_line <- function(mark) {
  props(stroke = "#000000")
}

#' @export
#' @rdname marks
mark_rect <- function(x = NULL, x2 = NULL, y = NULL, y2 = NULL, width = NULL,
                      opacity = NULL,
                      fill = NULL, fillOpacity = NULL,
                      stroke = NULL, strokeWidth = NULL, strokeOpacity = NULL) {
  mark("rect", x = x, x2 = x2, y = y, y2 = y2, width = width,
    opacity = opacity,
    fill = fill, fillOpacity = fillOpacity,
    stroke = stroke, strokeWidth = strokeWidth, strokeOpacity = strokeOpacity)
}
#' @S3method vega_mark_type mark_rect
vega_mark_type.mark_rect <- function(mark) "rect"
#' @S3method default_mark_properties mark_rect
default_mark_properties.mark_rect <- function(mark) {
  props(stroke = "#000000", fill = "#333333")
}

#' @export
#' @rdname marks
mark_text <- function(x = NULL, y = NULL, text = NULL, opacity = NULL,
                      fill = NULL, fillOpacity = NULL,
                      stroke = NULL, strokeWidth = NULL, strokeOpacity = NULL,
                      align = NULL, baseline = NULL, dx = NULL, dy = NULL, 
                      angle = NULL, font = NULL, fontSize = NULL, 
                      fontWeight = NULL, fontStyle = NULL) {
  
  mark("text", x = x, y = y, text = text, opacity = opacity,
       fill = fill, fillOpacity = fillOpacity,
       stroke = stroke, strokeWidth = strokeWidth, strokeOpacity = strokeOpacity,
       align = align, baseline = baseline, dx = dx, dy = dy, 
       angle = angle, font = font, fontSize = fontSize, 
       fontWeight = fontWeight, fontStyle = fontStyle)
}

#' @S3method vega_mark_type mark_text
vega_mark_type.mark_text <- function(mark) "text"
#' @S3method default_mark_properties mark_text
default_mark_properties.mark_text <- function(mark) {
  props(fill = "#333333")
}
