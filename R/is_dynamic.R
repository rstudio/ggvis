#' Determine if an object is dynamic (i.e. needs to be run in a shiny app)
#'
#' @export
#' @keywords internal
is.dynamic <- function(x) UseMethod("is.dynamic")

#' @export
is.dynamic.branch <- function(x) {
  is.dynamic(x$data) || is.dynamic(x$props) || any_apply(x$children, is.dynamic)
}

#' @export
is.dynamic.pipeline <- function(x, ...) {
  any_apply(x, is.dynamic)
}
#' @export
is.dynamic.ggvis_props <- is.dynamic.pipeline

#' @export
is.dynamic.datasource_reactive <- function(x) TRUE

#' @export
is.dynamic.transform <- function(x, ...) {
  any_apply(x, is.dynamic) || any_apply(x$dots, is.dynamic)
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
