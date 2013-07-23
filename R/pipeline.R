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
  pipes <- trim_to_source(compact(Map(as.pipe, input, names)))

  structure(
    pipes,
    class = "pipeline"
  )
}

#' @S3method c pipeline
c.pipeline <- function(x, ...) {
  new_pipes <- lapply(list(...), as.pipeline)

  structure(
    trim_to_source(c(unclass(x), unlist(new_pipes, recursive = FALSE))),
    class = "pipeline"
  )
}

#' @S3method [ pipeline
`[.pipeline` <- function(x, ...) {
  structure(NextMethod(x, ...), class = "pipeline")
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
#' @S3method as.pipeline NULL
as.pipeline.NULL <- function(x, ...) pipeline(NULL)
#' @S3method as.pipeline refMethodDef
as.pipeline.refMethodDef <- function(x, ...) pipeline(source_reactive(x))
#' @S3method as.pipeline function
as.pipeline.function <- function(x, ...) pipeline(source_function(x))

#' @S3method as.pipeline pipe
as.pipeline.pipe <- function(x, ...) pipeline(x)

format.pipeline <- function(x, ...) {
  pipes <- vapply(x, format, character(1))
  paste0(pipes, collapse = "\n")
}

print.pipeline <- function(x, ...) {
  cat(format(x, ...), "\n", sep = "")
}

# Return an id string, summarizing the pipeline
pipeline_id <- function(x, props) {
  if (length(x) == 0) return(NULL)
  paste(vapply(x, props = props, pipe_id, character(1)), collapse = "_")
}

# Given a pipeline object, trim off all items previous to the last source
trim_to_source <- function(x) {
  sources <- vapply(x, is_source, FUN.VALUE = logical(1))

  if (any(sources))
    x <- x[max(which(sources)):length(x)]

  x
}

#' @S3method split_vars pipeline
split_vars.pipeline <- function(x) {
  unlist(lapply(x, split_vars), recursive = FALSE)
}
