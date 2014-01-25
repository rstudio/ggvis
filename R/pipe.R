#' Create new "pipe" object.
#'
#' A pipe object represents a component in a \code{\link{pipeline}}. Pipes
#' provide a declarative specification of interactive behaviour, and define
#' the behaviour of each component in the data hierarchy.
#' 
#' This function is designed to be used by authors of new types of pipes.
#' If you are a ggvis user, please use an existing pipe: a data frame,
#' a transform, a mark, or a layer
#'
#' @keywords internal
#' @export
pipe <- function(type, ...) {
  check_empty_args()
  structure(
    compact(list(...)),
    class = c(type, "pipe")
  )
}

#' @export
#' @rdname pipe
is.pipe <- function(x) inherits(x, "pipe")

#' @export
#' @rdname pipe
as.pipe <- function(x, ...) UseMethod("as.pipe")

#' @export
as.pipe.pipe <- function(x, ...) x

#' @export
as.pipe.default <- function(x, name = NULL, ...) {
  if (is.null(name)) name <- deparse2(substitute(x))
  datasource(x, name = name)
}

#' @export
print.pipe <- function(x, ...) {
  cat(format(x, ...), "\n", sep = "")
}

# Reports whether a pipe object is a data source.
is_source <- function(x) UseMethod("is_source")

#' @export
is_source.default <- function(x) FALSE

# Give an abbreviated identifier for the pipe
pipe_id <- function(x, props) UseMethod("pipe_id")
