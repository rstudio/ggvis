#' Flow data down a pipe or pipeline
#'
#' This flows data down a pipeline (starting with \code{NULL}), applying
#' each pipe (transformation) in sequence.
#'
#' Every element in a pipeline recieves the same set of properties: this
#' generally means that for more complicated transformations you will need
#' to create multiple branches.
#'
#' @param x a pipeline or pipe
#' @param properties a \code{props} object
#' @param data the data source to start the flow
#' @export
#' @keywords internal
flow <- function(x, props, data = NULL) {
  stopifnot(is.gigvis_props(props))
  UseMethod("flow")
}

#' @S3method flow pipeline
flow.pipeline <- function(x, props, data = NULL) {
  for (pipe in x) {
    data <- flow(pipe, props, data)
  }
  data
}

#' @S3method flow NULL
flow.NULL <- function(x, data, props) {
  data
}
