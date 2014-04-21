as.reactive <- function(x, session = NULL, ...) UseMethod("as.reactive")

#' @export
as.reactive.function <- function(x, session = NULL, ...) x
#' @export
as.reactive.reactive <- function(x, session = NULL, ...) x
#' @export
as.reactive.default <- function(x, session = NULL, ...) reactive(x, ...)

reactive_label <- function(x) {
  attr(x, "observable")$.label
}

`reactive_label<-` <- function(x, value) {
  attr(x, "observable")$.label <- value
  x
}


# Pull reactives out of various types of objects
extract_reactives <- function(x) UseMethod("extract_reactives")

#' @export
extract_reactives.ggvis_props <- function(x) {
  compact(lapply(x, extract_reactives))
}

#' @export
extract_reactives.prop <- function(x, session = NULL, ...) {
  if (x$type == "reactive")
    x$value
  else
    NULL
}

# Get the value of a reactive or non-reactive object.
value <- function(x) {
  if (is.reactive(x)) x()
  else x
}
