# TODO: need to be able to pass additional properties (like offset)
#  into constants

#' Property: constant
#' 
#' Use a constant value for a mark property.
#'
#' @param value The value of the constant. If \code{scale = FALSE}, the default
#'   then this value needs to be interpretable in the aesthetic space (e.g.
#'   for colour, "red"; for position or size, an integer number of pixels).
#'   Otherwise, it will be scaled before plot
#' @param scale Should the value be scaled? \code{FALSE}, the default, then
#'   the value will be left as is. If \code{TRUE}, the default scale for that
#'   property will be used. Otherwise, you can supply the name of a specific
#'   scale as a string.
#' @export
#' @examples
#' constant("red")
#' constant("red", scale = TRUE)
#' constant("red", scale = "alarm")
constant <- function(value, scale = FALSE) {
  stopifnot(is.atomic(value), length(value) == 1)
  stopifnot(is.logical(scale) || is.character(scale), length(scale) == 1)
  
  structure(
    list(value = value, scale = scale), 
    class = c("constant", "prop"))
}

#' @S3method format constant
format.constant <- function(x, ...) {
  if (identical(x$scale, TRUE)) {
    scale <- " [auto]"
  } else if (is.character(x$scale)) {
    scale <- paste0(" [", x$scale, "]")
  } else {
    scale <- ""
  }
  
  paste0("<const> ", x$value, scale)
}
#' @S3method print constant
print.constant  <- function(x, ...) cat(format(x, ...), "\n", sep = "")

#' @S3method prop_value constant
prop_value.constant <- function(x, data) {
  # Shouldn't insert any data
  NULL
}

#' @S3method prop_vega constant
prop_vega.constant <- function(x, default_scale) {
  if (isTRUE(x$scale)) {
    scale <- default_scale
  } else if (is.character(x$scale)) {
    scale <- scale
  } else {
    scale <- NULL
  }
  
  compact(list(
    value = x$value,
    scale = scale 
  ))
}
