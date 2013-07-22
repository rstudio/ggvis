#' A reactive source.
#'
#' @param data A reactive values object or reactive expression.
#' @param name A name for the object.
#' @export
#' @examples
#' source_reactive(reactive())
source_reactive <- function(data, name = NULL) {
  if (is.null(name))
    name <- paste0("reactive_", digest(data, algo = "crc32"))

  pipe("source_reactive", data = data, name = name)
}

#' @S3method flow source_reactive
flow.source_reactive <- function(x, data, props) {
  isolate(x$data())
}

#' @S3method format source_reactive
format.source_reactive <- function(x, ...) {
  paste0("|-> ", x$name, " [reactive]")
}

#' @S3method is_source source_reactive
is_source.source_reactive <- function(x) TRUE

#' @S3method pipe_id source_reactive
pipe_id.source_reactive <- function(x, props) {
  x$name
}
