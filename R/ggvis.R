#' Create a ggvis graphic.
#'
#' @param ... components: data, \code{\link{props}}, \code{\link{branch}}es,
#'   \code{\link{marks}}, \code{\link{scales}}, \code{\link{axis}} or 
#'   \code{\link{legend}} objects.
#' @param width,height width and height of plot, in pixels
#' @param padding padding, as described by \code{\link{padding}}
#'
#' @return a \code{branch} object. Will display the plot when printed;
#'   see \code{\link{save_spec}}, \code{\link{view_static}} and
#'   \code{\link{view_dynamic}} for other options.
#' @export
#' @import assertthat
ggvis <- function(...) {
  check_empty_args()
  vis <- parse_components(...)
  class(vis) <- c("ggvis", "branch")

  set_last_vis(vis)
  vis
}

#' Is an object a ggvis object?
#'
#' @export
#' @param x an object to test
#' @keywords internal
is.ggvis <- function(x) inherits(x, "ggvis")

#' Tools to save and view static specs.
#'
#' These functions are mainly useful for testing.
#'
#' @param path location to save spec to, or load spec from
#' @param x a ggvis object
#' @param ... other arguments passed to \code{as.vega}
#' @keywords internal
#' @export
save_spec <- function(path, x = last_vis(), ...) {
  assert_that(is.ggvis(x), is.string(path))

  json <- toJSON(as.vega(x, ...), pretty = TRUE)
  writeLines(json, path)
}

#' @importFrom RJSONIO fromJSON
#' @rdname save_spec
view_spec <- function(path, ...) {
  contents <- paste0(readLines(path), collapse = "\n")
  spec <- fromJSON(contents)
  view_static(spec)
}
