#' Create a ggvis_scales object
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
  if (length(args) == 0) return(NULL)
  stopifnot(all(vapply(args, is.scale, logical(1))))

  names(args) <- vapply(args, "name", FUN = `[[`, FUN.VALUE = character(1))

  structure(args, class = "ggvis_scales")
}

#' @export
#' @rdname scales
#' @param x object to test for scales-ness
is.scales <- function(x) inherits(x, "ggvis_scales")

#' @S3method format ggvis_scales
format.ggvis_scales <- function(x, ...) {
  paste("*", vapply(x, format, character(1)), collapse = "\n")
}

#' @S3method print ggvis_scales
print.ggvis_scales <- function(x, ...) cat(format(x, ...), "\n", sep = "")

# Merge two ggvis scales objects
#
# merge_scales(scales(scale("x", "linear")))
# merge_scales(scales(scale("x", "linear")), scales(scale("y", "linear")))
# merge_scales(scales(scale("x", "linear"), scale("y", "linear")),
#              scales(scale("y", "ordinal")))
merge_scales <- function(parent = NULL, child = NULL) {
  if (is.null(parent)) return(child)
  if (is.null(child)) return(parent)
  stopifnot(is.scales(parent), is.scales(child))

  structure(merge_vectors(parent, child), class = "ggvis_scales")
}
