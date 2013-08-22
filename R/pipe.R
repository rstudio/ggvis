#' S3 class: pipe
#'
#' This function is used to create pipe classes and subclasses.
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

#' @S3method as.pipe pipe
as.pipe.pipe <- function(x, ...) x

#' @S3method as.pipe default
as.pipe.default <- function(x, name = NULL, ...) {
  if (is.null(name)) name <- deparse(substitute(x))
  datasource(x, name = name)
}

#' @S3method print pipe
print.pipe <- function(x, ...) {
  cat(format(x, ...), "\n", sep = "")
}

# Reports whether a pipe object is a data source.
is_source <- function(x) UseMethod("is_source")

#' @S3method is_source default
is_source.default <- function(x) FALSE

# Give an abbreviated identifier for the pipe
pipe_id <- function(x, props) UseMethod("pipe_id")
