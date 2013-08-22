#' Determine if an object is dynamic (i.e. needs to be run in a shiny app)
#'
#' @export
#' @keywords internal
is.dynamic <- function(x) UseMethod("is.dynamic")

#' @S3method is.dynamic ggvis_node
is.dynamic.ggvis_node <- function(x) {
  is.dynamic(x$data) || is.dynamic(x$props) || any_apply(x$children, is.dynamic)
}

#' @S3method is.dynamic pipeline
is.dynamic.pipeline <- function(x, ...) {
  any_apply(x, is.dynamic)
}
#' @S3method is.dynamic ggvis_props
is.dynamic.ggvis_props <- is.dynamic.pipeline

#' @S3method is.dynamic datasource_reactive
is.dynamic.datasource_reactive <- function(x) TRUE

#' @S3method is.dynamic transform
is.dynamic.transform <- function(x, ...) {
  any_apply(x, is.dynamic) || any_apply(x$dots, is.dynamic)
}

#' @S3method is.dynamic reactive
is.dynamic.reactive <- function(x) TRUE

#' @S3method is.dynamic input
is.dynamic.input <- function(x) TRUE

#' @S3method is.dynamic prop
is.dynamic.prop <- function(x) x$type == "reactive"

#' @S3method is.dynamic default
is.dynamic.default <- function(x) FALSE

any_apply <- function(xs, f) {
  for (x in xs) {
    if (f(x)) return(TRUE)
  }
  FALSE
}
