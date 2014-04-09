#' Vega marks.
#'
#' These functions create mark objects, corresponding to vega marks. Marks
#' are leaves in the plot tree, and control the details of the final rendering.
#' Marks are equivalent to the basic geoms in ggplot2 (e.g. point, line,
#' polygon), where ggvis layers correspond to combinations of geoms and
#' statistical transforms.
#'
#' Note that by supplying a fill property to \code{mark_path} will produce
#' a filled property. \code{mark_point} is an alias to \code{mark_symbol}.
#'
#' @template properties
#' @name marks
#' @seealso The "marks" vignette.
#' @param props A \code{\link{props}} object, named according to the
#'   properties listed below.
#' @param data An optional dataset, if you want to override the usual data
#'   inheritance for this mark.
NULL

# Return a character vector of valid properties for a given mark
valid_mark_properties <- function(mark) UseMethod("valid_mark_properties")
#' @export
valid_mark_properties.default <- function(mark) {
  stop("Unknown mark type: ", paste(class(mark), collapse=", "))
}

# Return a named list of default properties for a mark.
default_mark_properties <- function(mark) UseMethod("default_mark_properties")
#' @export
default_mark_properties.default <- function(mark) {
  stop("Unknown mark type: ", paste(class(mark), collapse=", "))
}

#' @rdname marks
#' @export
mark_point <- function(props = NULL, data = NULL) {
  mark("symbol", props = props, data = data)
}
#' @export
valid_mark_properties.mark_symbol <- function(mark) {
  c("x", "y", "opacity", "fill", "fillOpacity", "stroke", "strokeWidth",
    "strokeOpacity", "size", "shape", "key")
}
#' @export
default_mark_properties.mark_symbol <- function(mark) {
  props(fill := "#000000", size := 50)
}

#' @rdname marks
#' @export
mark_symbol <- function(props, data) {
  stop("mark_symbol is deprecated please use mark_point/layer_point instead",
    call. = FALSE)
}

#' @rdname marks
#' @export
mark_image <- function(props = NULL, data = NULL) mark("image", props = props, data = data)
#' @export
valid_mark_properties.mark_image <- function(mark) {
  c("x", "y", "opacity", "fill", "fillOpacity", "stroke", "strokeWidth",
    "strokeOpacity", "url", "align", "baseline", "key")
}
#' @export
default_mark_properties.mark_image <- function(mark) {
  props(fill := "#000000")
}

#' @rdname marks
#' @export
layer_image <- mark_image


#' @rdname marks
#' @export
mark_arc <- function(props = NULL, data = NULL) mark("arc", props = props, data = data)
#' @export
valid_mark_properties.mark_arc <- function(mark) {
  c("x", "y", "opacity", "fill", "fillOpacity", "stroke", "strokeWidth",
    "strokeOpacity", "innerRadius", "outerRadius", "startAngle", "endAngle",
    "key")
}
#' @export
default_mark_properties.mark_arc <- function(mark) {
  props(fill := "#333333")
}

#' @rdname marks
#' @export
layer_arc <- mark_arc



#' @rdname marks
#' @export
mark_area <- function(props = NULL, data = NULL) mark("area", props = props, data = data)
#' @export
valid_mark_properties.mark_area <- function(mark) {
  c("x", "y", "y2", "height", "opacity", "fill", "fillOpacity", "stroke",
    "strokeWidth", "strokeOpacity", "interpolate", "tension", "key")
}
#' @export
default_mark_properties.mark_area <- function(mark) {
  props(fill := "#333333")
}

#' @rdname marks
#' @export
layer_area <- mark_area

#' @rdname marks
#' @export
mark_path <- function(props = NULL, data = NULL) mark("line", props = props, data = data)
#' @export
valid_mark_properties.mark_line <- function(mark) {
  c("x", "y", "opacity", "fill", "fillOpacity", "stroke", "strokeWidth",
    "strokeOpacity", "interpolate", "tension", "key")
}
#' @export
default_mark_properties.mark_line <- function(mark) {
  props(stroke := "#000000")
}

#' @rdname marks
#' @export
layer_path <- mark_path


#' @export
#' @rdname marks
mark_rect <- function(props = NULL, data = NULL) mark("rect", props = props, data = data)
#' @export
valid_mark_properties.mark_rect <- function(mark) {
  c("x", "x2", "y", "y2", "width", "height", "opacity", "fill", "fillOpacity", "stroke",
    "strokeWidth", "strokeOpacity", "key")
}
#' @export
default_mark_properties.mark_rect <- function(mark) {
  props(stroke := "#000000", fill := "#333333")
}

#' @rdname marks
#' @export
layer_rect <- mark_rect


#' @export
#' @rdname marks
mark_text <- function(props = NULL, data = NULL) mark("text", props = props, data = data)
#' @export
valid_mark_properties.mark_text <- function(mark) {
  c("x", "y", "text", "opacity", "fill", "fillOpacity", "stroke",
    "strokeWidth", "strokeOpacity", "align", "baseline", "dx", "dy",
    "angle", "font", "fontSize", "fontWeight", "fontStyle", "key")
}
#' @export
default_mark_properties.mark_text <- function(mark) {
  props(fill := "#333333")
}

#' @rdname marks
#' @export
layer_text <- mark_text


# Hack to stop spurious warnings in R CMD check
globalVariables(c(
  valid_mark_properties.mark_symbol(),
  valid_mark_properties.mark_text(),
  valid_mark_properties.mark_arc(),
  valid_mark_properties.mark_image(),
  valid_mark_properties.mark_line(),
  valid_mark_properties.mark_rect()
))
