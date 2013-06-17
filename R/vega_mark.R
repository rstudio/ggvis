# Given a gigvis mark object and set of scales, output a vega mark object
vega_mark <- function(node, scales) {

  # Generate the fields related to mappings (x, y, etc)
  vega_node <- list(
    type = vega_mark_type(node),
    properties = list(
      update = c(
        vega_mappings(node$props),
        vega_mark_properties(node, scales)
      )
    )
  )

  if (!node$inherit_data) {
    vega_node$from <- list(data = node$data)
  }

  vega_node
}


# Given a gigvis_props object, return a vega mapping object
vega_mappings <- function(props) {
  vm <- lapply(names(props), function(name) {
    list(
      scale = properties_to_scales(name),
      field = paste("data", as.character(props[[name]]), sep = ".")
    )
  })
  setNames(vm, names(props))
}


# Given a gigvis mark object, return the vega mark type
vega_mark_type <- function(mark) UseMethod("vega_mark_type")

#' @S3method vega_mark_type default
vega_mark_type.default <- function(mark) NULL

#' @S3method vega_mark_type mark_point
vega_mark_type.mark_point <- function(mark) "symbol"

#' @S3method vega_mark_type mark_line
vega_mark_type.mark_line <- function(mark) "line"

#' @S3method vega_mark_type mark_rect
vega_mark_type.mark_rect <- function(mark) "rect"



# Given a gigvis mark object and set of scales, return a list of vega mark properties
vega_mark_properties <- function(mark, scales) {
  # Keep only the vega-specific fields, then remove the class, drop nulls,
  # and convert to proper format for vega properties.
  mark <- apply_default_mark_properties(mark)
  mark <- mark[names(mark) %in% valid_vega_mark_properties(mark)]

  mark <- unclass(drop_nulls(mark))

  # Convert each property to a Vega-structured property
  mapply(prop = names(mark), val = mark, MoreArgs = list(scales = scales),
    FUN = vega_mark_property, SIMPLIFY = FALSE)
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
  defaults <- defaults[!(names(defaults) %in% names(mapped_props(mark$props)))]

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
  list(stroke = "#000000", fill = "#333333")
}



vega_mark_property <- function(prop, val, scales) {
  # Convert scales to a named list for convenience
  names(scales) <- vapply(scales, `[[`, "name", FUN.VALUE = character(1))

  # If val is a _not_ a list, then wrap it into a list. These two calls are
  # therefore equivalent:
  #   mark_rect(y2 = 0)  or  mark_rect(y2 = list(value = 0))
  # This allows users to pass in other properties if needed:
  #   mark_rect(y2 = list(offset = -1))
  if (!is.list(val))  val <- list(value = val)

  if (prop %in% c("x", "y", "size", "opacity", "fill", "fillOpacity",
                  "stroke", "strokeWidth", "strokeOpacity")) {
    list(value = val$value)

  } else if (prop == "x2") {
    list(scale = "x", value = val$value)

  } else if (prop == "y2") {
    list(scale = "y", value = val$value)

  } else if (prop == "width") {
    if (scales$x$type == "ordinal")
      list(scale = "x", band = TRUE, offset = val$offset)
    else
      list(scale = "x", value = val$value)

  } else if (prop == "height") {
    if (scales$x$type == "ordinal")
      list(scale = "y", band = TRUE, offset = val$offset)
    else
      list(scale = "y", value = val$value)

  } else {
    stop("Unkown property: ", prop)
  }
}
