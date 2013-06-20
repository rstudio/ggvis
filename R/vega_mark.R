# Given a gigvis mark object and set of scales, output a vega mark object
vega_mark <- function(node, scales) {

  # Generate the fields related to mappings (x, y, etc)
  vega_node <- list(
    type = vega_mark_type(node),
    properties = list(
      update = vega_mark_properties(node, scales)
    )
  )

  if (!node$inherit_data) {
    vega_node$from <- list(data = node$data_id)
  }

  vega_node
}

# Given a gigvis mark object, return the vega mark type
vega_mark_type <- function(mark) UseMethod("vega_mark_type")

# Given a gigvis mark object and set of scales, return a list of vega mark properties
vega_mark_properties <- function(mark, scales) {
  # Keep only the vega-specific fields, then remove the class, drop nulls,
  # and convert to proper format for vega properties.
  defaults <- default_mark_properties(mark)
  props <- merge_props(defaults, mark$props)

  # Convert each property to a Vega-structured property
  mapply(prop = names(props), val = props, MoreArgs = list(scales = scales),
    FUN = vega_mark_property, SIMPLIFY = FALSE)
}

# Return a named list of default properties for a mark.
default_mark_properties <- function(mark) {
  UseMethod("default_mark_properties")
}

# Given a gigvis mark object, return a vector of strings of valid vega
# mark properties
valid_vega_mark_properties <- function(mark) {
  UseMethod("valid_vega_mark_properties")
}

#' @S3method valid_vega_mark_properties default
valid_vega_mark_properties.default <- function(mark) {
  .common_vega_mark_properties
}

#' @S3method valid_vega_mark_properties mark_symbol
valid_vega_mark_properties.mark_symbol <- function(mark) {
  c(.common_vega_mark_properties, "size", "shape")
}

#' @S3method valid_vega_mark_properties area
valid_vega_mark_properties.area <- function(mark) {
  c(.common_vega_mark_properties, "interpolate", "tension")
}

#' @S3method valid_vega_mark_properties line
valid_vega_mark_properties.line <- function(mark) {
  c(.common_vega_mark_properties, "fill", "interpolate", "tension")
}

#' @S3method valid_vega_mark_properties image
valid_vega_mark_properties.image <- function(mark) {
  c(.common_vega_mark_properties, "url", "align", "baseline")
}

#' @S3method valid_vega_mark_properties text
valid_vega_mark_properties.text <- function(mark) {
  c(.common_vega_mark_properties, "text", "align", "baseline", "dx", "dy",
    "angle", "font", "fontSize", "fontWeight", "fontStyle")
}

.common_vega_mark_properties <- c("x", "x2", "width", "y", "y2", "height",
  "opacity", "fill", "fillOpacity", "stroke", "strokeWidth", "strokeOpacity")

vega_mark_property <- function(prop, val, scales) {
  # Convert scales to a named list for convenience
  names(scales) <- vapply(scales, `[[`, "name", FUN.VALUE = character(1))

  vega <- prop_vega(val, default_scale(prop))

  # This is an ugly hack, but not sure yet how to make better.
  if ((prop == "width"  && scales$x$type == "ordinal") ||
      (prop == "height" && scales$y$type == "ordinal")) {
    vega$band <- TRUE
  }

  vega
}

default_scale <- function(prop) {
  stopifnot(is.character(prop), length(prop) == 1)

  as_is <- c("x", "y", "size", "opacity", "fill", "fillOpacity",
    "stroke", "strokeWidth", "strokeOpacity")
  if (prop %in% as_is) return(prop)

  mapped <- c(
    "x2" = "x",
    "width" = "x",
    "y" = "y",
    "y2" = "y"
  )
  if (prop %in% names(mapped)) return(mapped[[prop]])

  stop("Unkown property: ", prop)
}
