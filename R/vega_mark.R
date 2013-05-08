# Given a gigvis mark object, output a vega mark object
vega_mark <- function(node) {

  # Generate the fields related to mappings (x, y, etc)
  # This assumes that the scale's name is the same as the 'name' field, which
  # is true now but might not be a good assumption in the long run.
  vega_mapping <- list()
  for (name in names(node$mapping)) {
    vega_mapping[[name]] <- list(
      field = paste("data", node$mapping[[name]], sep = "."),
      scale = name
    )
  }

  list(
    type = vega_mark_type(node),
    from = list(data = node$data),
    properties = list(
      update = c(
        vega_mapping,
        vega_mark_properties(node)
      )
    )
  )
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

#' @S3method valid_vega_mark_properties symbol
valid_vega_mark_properties.symbol <- function(mark) {
  c(.common_vega_mark_properties, "size", "shape")
}

#' @S3method valid_vega_mark_properties area
valid_vega_mark_properties.area <- function(mark) {
  c(.common_vega_mark_properties, "interpolate", "tension")
}

#' @S3method valid_vega_mark_properties line
valid_vega_mark_properties.line <- function(mark) {
  c(.common_vega_mark_properties, "interpolate", "tension")
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
