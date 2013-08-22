#' A data pipeline.
#'
#' This creates an S3 object that represents pipeline of data tranformations.
#' The data argument to a ggvis branch, must always be a data pipeline: to
#' ensure this \code{as.pipeline} is also called on the input, automatically
#' converting simpler expressions (like data frames and strings).
#'
#' @param ... a list of pipes
#' @param .pipes if you already have the pipes in a list, use this argument.
#' @param x an object to test/coerce
#' @export
#' @examples
#' pipeline(mtcars)
#' as.pipeline(mtcars)
#' pipeline(cars = mtcars)
#'
#' # A pipeline can contain multiple data sets, but only the last one is
#' # returned
#' pipeline(mtcars, sleep)
#'
#' # More useful pipelines combine data and transformations
#' pipeline(mtcars, transform_bin())
#' pipeline(mtcars, by_group("cyl"), transform_bin())
pipeline <- function(..., .pipes = list()) {
  check_empty_args()
  args <- list(...)
  if (is.null(names(args))) {
    names(args) <- vapply(dots(...), function(x) deparse(x), character(1))
  }
  input <- c(args, .pipes)
  if (length(input) == 0) return()

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

#' @S3method as.pipeline pipe
as.pipeline.pipe <- function(x, ...) pipeline(x)

#' @S3method as.pipeline default
as.pipeline.default <- function(x, name = NULL, ...) {
  if (is.null(name)) name <- deparse(substitute(x))
  pipeline(datasource(x, name = name))
}


#' @S3method format pipeline
format.pipeline <- function(x, ...) {
  pipes <- vapply(x, format, character(1))
  paste0(pipes, collapse = "\n")
}

#' @S3method print pipeline
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
