#' Create a gigvis_scales object
#'
#' Given arguments which are scales object, return a list with those objects,
#' where the entries also have names that are the same as the "name" field of
#' the objects.
#'
#' @export
scales <- function(...) {
  args <- list(...)
  stopifnot(all(vapply(args, is.scale, logical(1))))

  names(args) <- vapply(args, "name", FUN = `[[`, FUN.VALUE = character(1))

  structure(args, class = "gigvis_scales")
}

#' @export
is.scales <- function(x) inherits(x, "gigvis_scales")

#' S3method format gigvis_scales
format.gigvis_scales <- function(x, ...) {
  paste("*", vapply(x, format, character(1)), collapse = "\n")
}

#' S3method print gigvis_scales
print.gigvis_scales <- function(x, ...) cat(format(x, ...), "\n", sep = "")


#' @export
scale <- function(name, type = "linear", zero = FALSE) {
  structure(
    list(
      name = name,
      type = type,
      zero = zero
    ),
    class = "scale"
  )
  # TODO: validate arguments. Some scales don't use some properties; e.g.,
  # color doesn't use zero.
}

#' @export
is.scale <- function(x) inherits(x, "scale")

#' S3method format scale
format.scale <- function(x, ...) {
  sprintf("%s: %-7s zero:%s", x$name, x$type, x$zero)
}

#' S3method print scale
print.scale <- function(x, ...) cat(format(x, ...), "\n", sep = "")

# Merge two gigvis scales objects
#
# merge_scales(scales(scale("x", "linear")))
# merge_scales(scales(scale("x", "linear")), scales(scale("y", "linear")))
# merge_scales(scales(scale("x", "linear"), scale("y", "linear")),
#              scales(scale("y", "ordinal")))
merge_scales <- function(parent = NULL, child = NULL) {
  if (is.null(parent)) return(child)
  if (is.null(child)) return(parent)
  stopifnot(is.scales(parent), is.scales(child))

  structure(merge_vectors(parent, child), class = "gigvis_scales")
}
