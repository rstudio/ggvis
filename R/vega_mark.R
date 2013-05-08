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
vega_mark_properties <- function(mark) UseMethod("vega_mark_properties")

#' @S3method vega_mark_properties default
vega_mark_properties.default <- function(mark) {
  # For most marks, we can remove some gigvis-specific fields, then remove
  # the class, drop nulls, and convert to proper format for vega properties
  mark[c("type", "data", "data_std", "mapping")] <- NULL
  mark <- unclass(drop_nulls(mark))
  lapply(mark, function(x) list(value=x))
}
