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
#' @param vis Visualisation to modify
#' @name marks
#' @param props,... A \code{\link{props}} object, named according to the
#'   properties listed below.
#' @param data An optional dataset, if you want to override the usual data
#'   inheritance for this mark.
NULL

#' @rdname marks
#' @export
emit_points <- function(vis, props) {
  add_mark(vis, "symbol", props)
}
#' @rdname marks
#' @export
layer_points <- function(vis, ..., data = NULL) {
  add_mark(vis, "symbol", props(..., env = parent.frame()), data,
    deparse2(substitute(data)))
}

#' @rdname marks
#' @export
emit_images <- function(vis, props) {
  add_mark(vis, "image", props)
}
#' @rdname marks
#' @export
layer_images <- function(vis, ..., data = NULL) {
  add_mark(vis, "image", props(..., env = parent.frame()), data,
    deparse2(substitute(data)))
}

#' @rdname marks
#' @export
emit_arcs <- function(vis, props) {
  add_mark(vis, "arc", props)
}
#' @rdname marks
#' @export
layer_arcs <- function(vis, ..., data = NULL) {
  add_mark(vis, "arc", props(..., env = parent.frame()), data,
    deparse2(substitute(data)))
}


#' @rdname marks
#' @export
emit_ribbons <- function(vis, props) {
  add_mark(vis, "area", props)
}
#' @rdname marks
#' @export
layer_ribbons <- function(vis, ..., data = NULL) {
  add_mark(vis, "area", props(..., env = parent.frame()), data,
    deparse2(substitute(data)))
}

#' @rdname marks
#' @export
emit_paths <- function(vis, props) {
  add_mark(vis, "line", props)
}
#' @rdname marks
#' @export
layer_paths <- function(vis, ..., data = NULL) {
  add_mark(vis, "line", props(..., env = parent.frame()), data,
    deparse2(substitute(data)))
}

#' @rdname marks
#' @export
emit_rects <- function(vis, props) {
  add_mark(vis, "rect", props)
}
#' @rdname marks
#' @export
layer_rects <- function(vis, ..., data = NULL) {
  add_mark(vis, "rect", props(..., env = parent.frame()), data,
    deparse2(substitute(data)))
}

#' @rdname marks
#' @export
emit_text <- function(vis, props) {
  add_mark(vis, "text", props)
}
#' @rdname marks
#' @export
layer_text <- function(vis, ..., data = NULL) {
  add_mark(vis, "text", props(..., env = parent.frame()), data,
    deparse2(substitute(data)))
}

common_valid_props <- c("x", "y", "stroke", "strokeOpacity", "fill",
  "fillOpacity", "opacity", "strokeWidth", "strokeDash", "key")

valid_props <- list(
  arc = c(common_valid_props, "innerRadius", "outerRadius", "startAngle",
    "endAngle"),
  area = c(common_valid_props, "y2", "height", "interpolate", "tension"),
  image = c(common_valid_props, "x2", "y2", "width", "height", "url", "align",
    "baseline"),
  line = c(common_valid_props, "interpolate", "tension"),
  rect = c(common_valid_props, "x2", "y2", "width", "height"),
  symbol = c(common_valid_props, "size", "shape"),
  text = c(common_valid_props, "text", "align", "baseline", "dx", "dy", "angle",
    "font", "fontSize", "fontWeight", "fontStyle")
)

# Hack to stop spurious warnings in R CMD check. Used in prop.
known_props <- sort(unique(unlist(valid_props)))
globalVariables(known_props)

# Some marks need more detailed validity checks of their props
mark_props_validity_checks <- list(
  image = function(props) {
    url <- props$url.update

    if (!is.null(url) &&
        is.prop_constant(url) &&
        !grepl("http(s)?://", url$value)) {

      warning("image mark's url prop '", props$url.update$value,
        "' should be an absolute URL (http://... or https://...).",
        " Referencing local files for url not yet implemented.")
    }
  }
)

default_props <- function(type) {
  switch(type,
    arc = props(fill := "#333333"),
    area = props(fill := "#333333"),
    line = props(stroke := "#000000"),
    image = props(fill := "#000000"),
    rect = props(stroke := "#000000", fill := "#333333"),
    symbol = props(fill := "#000000", size := 50),
    text = props(fill := "#333333"),
    stop("Unknown type")
  )
}
