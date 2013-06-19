#' A data pipeline.
#' 
#' This creates an S3 object that represents pipeline of data tranformations. 
#' The data argument to a gigvis branch, must always be a data pipeline: to
#' ensure this \code{as.pipeline} is also called on the input, automatically
#' converting simpler expressions (like data frames and strings).
#' 
#' @param ... a list of pipes
#' @param .pipes if you already have the pipes in a list, use this argument.
#' @param x an object to test/coerce
#' @export
#' @examples
#' # You can refer to data by value or by reference
#' # If you refer to it by name, you need to provide R some help to figure
#' # out itss name
#' pipeline(mtcars)
#' as.pipeline(mtcars)
#' pipeline(mtcars = mtcars)
#' pipeline("mtcars")
#' 
#' # A pipeline can contain multiple data sets - but in practice only
#' # the last one will be used.
#' pipeline("mtcars", mtcars)
#' 
#' # More useful pipelines combine data and transformations
#' pipeline("mtcars", transform_bin())
#' pipeline("mtcars", split_by("cyl"), transform_bin())
pipeline <- function(..., .pipes = list()) {
  input <- c(list(...), .pipes)
  
  names <- names(input) %||% rep(list(NULL), length(input))
  pipes <- compact(Map(as.pipe, input, names))
  
  structure(
    list(pipes = pipes),
    class = "pipeline"
  )
}

#' @export
#' @rdname pipeline
is.pipeline <- function(x) inherits(x, "pipeline")

#' @export
#' @rdname pipeline
as.pipeline <- function(x, ...) {
  UseMethod("as.pipeline")
}

#' @S3method as.pipeline pipeline
as.pipeline.pipeline <- function(x, ...) x

#' @S3method as.pipeline data.frame
as.pipeline.data.frame <- function(x, name = NULL, ...) {
  if (is.null(name)) name <- deparse(substitute(x))
  pipeline(source_eager(x, name = name))
}
#' @S3method as.pipeline list
as.pipeline.list <- function(x, ...) {
  pipes <- lapply(x, pipe, ...)
  pipeline(.pipes = pipes)
}
#' @S3method as.pipeline character
as.pipeline.character <- function(x, ...) pipeline(source_lazy(x))

format.pipeline <- function(x, ...) {
  pipes <- vapply(x$pipes, format, character(1))
  paste0(pipes, collapse = "\n")
}

print.pipeline <- function(x, ...) {
  cat(format(x, ...), "\n", sep = "")
}

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
flow <- function(x, props, data = NULL, ...) {
  stopifnot(is.gigvis_props(props))
  UseMethod("flow")
}

#' @S3method flow pipeline
flow.pipeline <- function(x, props, data = NULL, ...) {
  for (pipe in x$pipes) {
    data <- flow(pipe, props, data, ...)
  }
  data
}

#' @S3method flow NULL
flow.NULL <- function(x, data, props, ...) {
  data
}
