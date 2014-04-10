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
mark_point <- function(vis, props = NULL, data = NULL) {
  add_mark(vis, "symbol", props, data, deparse2(substitute(data)))
}
#' @rdname marks
#' @export
layer_point <- mark_point
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
mark_symbol <- function(vis, props, data) {
  stop("mark_symbol is deprecated please use mark_point/layer_point instead",
    call. = FALSE)
}

#' @rdname marks
#' @export
mark_image <- function(vis, props = NULL, data = NULL) {
  add_mark(vis, "image", props, data, deparse2(substitute(data)))
}
#' @rdname marks
#' @export
layer_image <- mark_image
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
mark_arc <- function(vis, props = NULL, data = NULL) {
  add_mark(vis, "arc", props, data, deparse2(substitute(data)))
}
#' @rdname marks
#' @export
layer_arc <- mark_arc
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
mark_area <- function(vis, props = NULL, data = NULL) {
  add_mark(vis, "area", props, data, deparse2(substitute(data)))
}
#' @rdname marks
#' @export
layer_area <- mark_area
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
mark_path <- function(vis, props = NULL, data = NULL) {
  add_mark(vis, "line", props, data, deparse2(substitute(data)))
}
#' @rdname marks
#' @export
layer_path <- mark_path
#' @export
valid_mark_properties.mark_line <- function(mark) {
  c("x", "y", "opacity", "fill", "fillOpacity", "stroke", "strokeWidth",
    "strokeOpacity", "interpolate", "tension", "key")
}
#' @export
default_mark_properties.mark_line <- function(mark) {
  props(stroke := "#000000")
}


#' @export
#' @rdname marks
mark_rect <- function(vis, props = NULL, data = NULL) {
  add_mark(vis, "rect", props, data, deparse2(substitute(data)))
}
#' @rdname marks
#' @export
layer_rect <- mark_rect
#' @export
valid_mark_properties.mark_rect <- function(mark) {
  c("x", "x2", "y", "y2", "width", "height", "opacity", "fill", "fillOpacity", "stroke",
    "strokeWidth", "strokeOpacity", "key")
}
#' @export
default_mark_properties.mark_rect <- function(mark) {
  props(stroke := "#000000", fill := "#333333")
}


#' @export
#' @rdname marks
mark_text <- function(vis, props = NULL, data = NULL) {
  add_mark(vis, "text", props, data, deparse2(substitute(data)))
}
#' @rdname marks
#' @export
layer_text <- mark_text
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


# Hack to stop spurious warnings in R CMD check
globalVariables(c(
  valid_mark_properties.mark_symbol(),
  valid_mark_properties.mark_text(),
  valid_mark_properties.mark_arc(),
  valid_mark_properties.mark_image(),
  valid_mark_properties.mark_line(),
  valid_mark_properties.mark_rect()
))
