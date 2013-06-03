# Given a gigvis mark object, output a vega mark object
vega_mark <- function(node) {

  # Generate the fields related to mappings (x, y, etc)
  vega_node <- list(
    type = vega_mark_type(node),
    properties = list(
      update = c(
        vega_mappings(node$mapping),
        vega_mark_properties(node)
      )
    )
  )

  if (!node$inherit_data) {
    vega_node$from <- list(data = node$data)
  }

  vega_node
}


# Given a gigvis mapping object, return a vega mapping object
vega_mappings <- function(mappings) {
  vm <- lapply(names(mappings), function(name) {
    list(field = paste("data", mappings[[name]], sep = "."), scale = name)
  })
  setNames(vm, names(mappings))
}


# Given a gigvis mark object, return the vega mark type
vega_mark_type <- function(mark) UseMethod("vega_mark_type")

#' @S3method vega_mark_type default
vega_mark_type.default <- function(mark) NULL

#' @S3method vega_mark_type mark_point
vega_mark_type.mark_point <- function(mark) "symbol"

#' @S3method vega_mark_type mark_line
vega_mark_type.mark_line <- function(mark) "line"



# Given a gigvis mark object, return a list of vega mark properties
vega_mark_properties <- function(mark) {
  # Keep only the vega-specific fields, then remove the class, drop nulls,
  # and convert to proper format for vega properties.
  mark <- apply_default_mark_properties(mark)
  mark <- mark[names(mark) %in% valid_vega_mark_properties(mark)]

  mark <- unclass(drop_nulls(mark))
  lapply(mark, function(x) list(value=x))
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

#' @S3method valid_vega_mark_properties mark_point
valid_vega_mark_properties.mark_point <- function(mark) {
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



# Given a mark, return a named list with default values for each property that
# is NULL and unmapped.
apply_default_mark_properties <- function(mark) {
  defaults <- default_mark_properties(mark)

  # Keep only the properties that are NULL
  null_props <- names(mark)[vapply(mark, is.null, FUN.VALUE = logical(1))]
  defaults <- defaults[names(defaults) %in% null_props]

  # Keep only properties that aren't mapped
  defaults <- defaults[!(names(defaults) %in% names(mark$mapping))]

  mark[names(defaults)] <- defaults
  mark
}


# Return a named list of default properties for a mark.
default_mark_properties <- function(mark) {
  UseMethod("default_mark_properties")
}

#' @S3method default_mark_properties mark_point
default_mark_properties.mark_point <- function(mark) {
  list(fill = "#000000")
}

#' @S3method default_mark_properties mark_line
default_mark_properties.mark_line <- function(mark) {
  list(stroke = "#000000")
}

#' @S3method default_mark_properties mark_rect
default_mark_properties.mark_rect <- function(mark) {
  list(stroke = "#000000")
}
