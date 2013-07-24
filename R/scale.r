#' @export
scale <- function(name, type = NULL, zero = NULL) {
  structure(
    drop_nulls(list(
      name = name,
      type = type,
      zero = zero
    )),
    class = "scale"
  )
  # TODO: validate arguments. Some scales don't use some properties; e.g.,
  # color doesn't use zero.
}

#' @export
is.scale <- function(x) inherits(x, "scale")

#' @S3method format scale
format.scale <- function(x, ...) {
  str <- sprintf("%-9s ", paste0(x$name, ":"))
  if (!is.null(x$type)) str <- paste0(str, "type:", x$type)
  if (!is.null(x$zero)) str <- paste0(str, "zero:", x$zero)
  str
}

#' @S3method print scale
print.scale <- function(x, ...) cat(format(x, ...), "\n", sep = "")
