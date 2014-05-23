#' Create a "scales" object.
#'
#' A scales object is used to manage multiple scales, essentially converting
#' a unnamed list into a named list.
#'
#' @export
#' @param ...,.scales scales to combine into a single scales object
#' @keywords internal
scales <- function(..., .scales = list()) {
  args <- c(list(...), .scales)
  if (length(args) == 0) return(NULL)
  stopifnot(all(vapply(args, is.scale, logical(1))))

  names(args) <- vapply(args, "name", FUN = `[[`, FUN.VALUE = character(1))

  structure(args, class = "scales")
}

#' @export
#' @rdname scales
#' @param x object to test for scales-ness
is.scales <- function(x) inherits(x, "scales")

#' @export
format.scales <- function(x, ...) {
  paste("*", vapply(x, format, character(1)), collapse = "\n")
}

#' @export
print.scales <- function(x, ...) cat(format(x, ...), "\n", sep = "")

# Merge two ggvis scales objects
#
# merge_scales(scales(vega_scale("x", "linear")))
# merge_scales(scales(vega_scale("x", "linear")), scales(vega_scale("y", "linear")))
# merge_scales(scales(vega_scale("x", "linear"), vega_scale("y", "linear")),
#              scales(vega_scale("y", "ordinal")))
merge_scales <- function(parent = NULL, child = NULL) {
  if (is.null(parent)) return(child)
  if (is.null(child)) return(parent)
  stopifnot(is.scales(parent), is.scales(child))

  structure(merge_vectors(parent, child), class = "scales")
}

# Given a ggvis object, add all needed vega scales, with correct domain
# values set.
add_default_scales <- function(vis) {
  scales <- vis$scales

  # Add in scales not already specified in spec
  needed <- setdiff(names(vis$scale_info), names(scales))
  for (scale_n in needed) {
    info <- vis$scale_info[[scale_n]]
    scale <- default_scale(scale_n, info$type)
    vis <- add_scale(vis, scale)
  }

  vis
}
