#' Determine if an object is dynamic (i.e. needs to be run in a shiny app)
#'
#' @export
#' @keywords internal
is.dynamic <- function(x) UseMethod("is.dynamic")

#' @export
is.dynamic.ggvis <- function(x) {
  any_apply(x$data, is.dynamic) || any_apply(x$props, is.dynamic)
}

#' @export
is.dynamic.ggvis_props <- function(x, ...) {
  any_apply(x, is.dynamic)
}

#' @export
is.dynamic.reactive <- function(x) TRUE

#' @export
is.dynamic.input <- function(x) TRUE

#' @export
is.dynamic.prop <- function(x) x$type == "reactive"

#' @export
is.dynamic.default <- function(x) FALSE

any_apply <- function(xs, f) {
  for (x in xs) {
    if (f(x)) return(TRUE)
  }
  FALSE
}
