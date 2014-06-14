reactive_id <- function(x) {
  attr(x, "reactive_id", TRUE)
}

`reactive_id<-` <- function(x, value) {
  attr(x, "reactive_id") <- value
  x
}


# Pull reactives out of various types of objects
extract_reactives <- function(x) UseMethod("extract_reactives")
#' @export
extract_reactives.ggvis_props <- function(x) {
  compact(lapply(x, extract_reactives))
}
#' @export
extract_reactives.prop_reactive <- function(x) x$value
#' @export
extract_reactives.default <- function(x) NULL

# Get the value of a reactive or non-reactive object.
value <- function(x) UseMethod("value")
#' @export
value.default <- function(x) x
#' @export
value.reactive <- function(x) x()

values <- function(x) lapply(x, value)
