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

  # TODO: Support other properties besides just stroke and fill
  list(
    type = vega_mark_type(node),
    from = list(data = node$data),
    properties = list(
      update = c(
        vega_mapping,
        list(
          stroke = list(value = node$stroke),
          fill = list(value = node$fill)
        )
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
