#' Create a gigvis_scales object
#'
#' Given arguments which are scales object, return a list with those objects,
#' where the entries also have names that are the same as the "name" field of
#' the objects.
#'
#' @export
#' @param ...,.scales scales to combine into a single scales object
#' @keywords internal
scales <- function(..., .scales = list()) {
  args <- c(list(...), .scales)
  stopifnot(all(vapply(args, is.scale, logical(1))))

  names(args) <- vapply(args, "name", FUN = `[[`, FUN.VALUE = character(1))

  structure(args, class = "gigvis_scales")
}

#' @export
is.scales <- function(x) inherits(x, "gigvis_scales")

#' @S3method format gigvis_scales
format.gigvis_scales <- function(x, ...) {
  paste("*", vapply(x, format, character(1)), collapse = "\n")
}

#' @S3method print gigvis_scales
print.gigvis_scales <- function(x, ...) cat(format(x, ...), "\n", sep = "")

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
