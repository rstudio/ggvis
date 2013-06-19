#' An eager source.
#' 
#' An eager source captures the data frame immediately when it is called.
#' This is useful when you are constructing plot components inside a function
#' because they will be self-contained, but creates heavy weight gigvis
#' objects. See \code{\link{source_lazy}} for a light-weight (but not self-
#' contained) alternative
#' 
#' @param data a data frame
#' @param name the name of the data frame (used in error messages etc.)
#' @export
#' @examples
#' source_eager(mtcars)
source_eager <- function(data, name = NULL) {
  stopifnot(is.data.frame(data))
  if (is.null(name)) {
    name <- deparse(substitute(x))
  }
  stopifnot(is.character(name), length(name) == 1)
  
  pipe("source_eager", data = data, name = name)
}

#' @S3method flow source_eager
flow.source_eager <- function(x, data, props, ...) {
  x$data
}

#' @S3method format source_eager
format.source_eager <- function(x, ...) {
  paste0("|-> ", x$name, " [eager]")
}

#' @S3method is_source source_eager
is_source.source_eager <- function(x) TRUE
