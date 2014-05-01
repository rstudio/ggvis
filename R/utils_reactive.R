reactive_id <- function(x) {
  attr(x, "reactive_id")
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
extract_reactives.prop <- function(x, session = NULL, ...) {
  if (x$type == "reactive")
    x$value
  else
    NULL
}

# Get the value of a reactive or non-reactive object.
value <- function(x) {
  if (shiny::is.reactive(x)) x()
  else x
}
values <- function(x) lapply(x, value)
