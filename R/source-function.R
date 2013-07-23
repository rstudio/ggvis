#' A function source.
#'
#' A function source is evaluated when the plot is created.
#'
#' @param data A function that returns a data frame.
#' @export
#' @examples
#' mt <- function() mtcars
#' source_function(mt)
source_function <- function(func, name = NULL) {
  if (is.null(func)) return(NULL)
  
  if (is.null(name)) {
    name <- deparse(substitute(data))
  }
  stopifnot(is.character(name), length(name) == 1)

  pipe("source_function", func = func, name = name)
}

#' @S3method connect source_function
connect.source_function <- function(x, data, props) {
  react(x$func())
}

#' @S3method format source_function
format.source_function <- function(x, ...) {
  paste0("|-> ", x$name, " [function]")
}

#' @S3method is_source source_function
is_source.source_function <- function(x) TRUE

#' @S3method pipe_id source_function
pipe_id.source_function <- function(x, props) x$name
