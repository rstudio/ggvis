
mark <- function(type, ...) {
  structure(
    list(type = type, props = props(...)),
    class = c(paste0("mark_", type), "mark", "gigvis_node")
  )
}

#' @S3method print mark
print.mark <- function(x, ...) str(x)

#' Vega marks properties.
#' 
#' @section Properties:
#' 
#' \itemize{
#'   \item x:  The first (typically left-most) x-coordinate.
#'   
#'   \item x2: The second (typically right-most) x-coordinate.
#'   
#'   \item width: The width of the mark (if supported).
#'   
#'   \item y: The first (typically top-most) y-coordinate.
#'   
#'   \item y2: The second (typically bottom-most) y-coordinate.
#'   
#'   \item height: The height of the mark (if supported).
#'   
#'   \item opacity: The overall opacity.
#'   
#'   \item fill: The fill color.
#'   
#'   \item fillOpacity: The fill opacity
#'   
#'   \item stroke: The stroke color.
#'   
#'   \item strokeWidth: The stroke width, in pixels.
#'   
#'   \item strokeOpacity: The stroke opacity.
#'   
#'   \item size [symbol]: The pixel area of the symbol. For example: in the case
#'   of circles, the radius is determined in part by the square root of the size 
#'   value.
#'   
#'   \item shape [symbol]: The symbol shape to use. One of circle (default), 
#'   square, cross, diamond, triangle-up, or triangle-down (symbol only)
#'   
#'   \item innerRadius [arc]: The inner radius of the arc, in pixels.
#'   
#'   \item outerRadius [arc]: The outer radius of the arc, in pixels.
#'   
#'   \item startAngle [arc]: The start angle of the arc, in radians.
#'   
#'   \item endAngle [arc]: The end angle of the arc, in radians.
#'   
#'   \item interpolate [area, line]: The line interpolation method to use. One 
#'   of linear, step-before, step-after, basis, basis-open, cardinal, 
#'   cardinal-open, monotone.
#'
#'   \item tension [area, line]: Depending on the interpolation type, sets the 
#'   tension parameter.
#'   
#'   \item url [image]: The URL from which to retrieve the image.
#'   
#'   \item align [image, text]: The horizontal alignment of the object. One of 
#'   left, right, center.
#'   
#'   \item baseline [image, text]: The vertical alignment of the object. One of 
#'   top, middle, bottom.
#'   
#'   \item text [text]: The text to display.
#'   
#'   \item dx [text]: The horizontal margin, in pixels, between the text label 
#'   and its anchor point. The value is ignored if the align property is center.
#'   
#'   \item dy [text]: The vertical margin, in pixels, between the text label 
#'   and its anchor point. The value is ignored if the baseline property is 
#'   middle.
#'   
#'   \item angle [text]: The rotation angle of the text, in degrees.
#'   
#'   \item font [text]: The typeface to set the text in (e.g., Helvetica Neue).
#'   
#'   \item fontSize [text]: The font size, in pixels.
#'   
#'   \item fontWeight [text]: The font weight (e.g., bold).
#'   
#'   \item fontStyle [text]: The font style (e.g., italic).
#'   
#' }
#' 
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
