#' Create a gigvis object, or a node in a gigvis plot.
#'
#' @param data A data \code{\link{pipeline}}, or anything that can be coerced
#'   to a pipeline using \code{\link{as.pipeline}}
#' @param props a list of \code{\link{props}} defining default mark properties
#'   for this node and all its children.
#' @param ... components: \code{node}s,  \code{\link{marks}}, 
#'   \code{\link{scales}}, \code{\link{axis}} or \code{\link{legend}} objects
#' @export
#' @import assertthat
gigvis <- function(data = NULL, props = NULL, ...) {
  args <- list(...)
  classes <- vapply(args, function(x) last(class(x)), character(1))
  components <- gigvis_components(...)

  vis <- structure(
    list(
      data = as.pipeline(data), 
      props = props, 
      scales = scales(.scales = components$scale),
      axes = components$axis,
      legends = components$legend,
      children = components$node),
    class = c("gigvis", "gigvis_node")
  )
  set_last_vis(vis)
  vis
}

gigvis_components <- function(...) {
  args <- list(...)
  types <- vapply(args, component_type, character(1))
  
  split(args, types)
}

component_type <- function(x) UseMethod("component_type")
#' @S3method component_type gigvis_node
component_type.gigvis_node <- function(x) "node"
#' @S3method component_type scale
component_type.scale <- function(x) "scale"
#' @S3method component_type vega_legend
component_type.vega_legend <- function(x) "legend"
#' @S3method component_type vega_axis
component_type.vega_axis <- function(x) "axis"

#' @export
#' @rdname gigvis
is.gigvis <- function(x) inherits(x, "gigvis")

#' @export
#' @rdname gigvis
node <- function(..., data = NULL, props = NULL) {
  structure(
    list(
      data = as.pipeline(data),
      props = props,
      children = list(...)
    ),
    class = "gigvis_node"
  )
}

#' @S3method print gigvis
print.gigvis <- function(x, dynamic = NA, ...) {
  set_last_vis(x)

  if (is.na(dynamic)) dynamic <- is.dynamic(x)
  
  if (dynamic) {
    view_dynamic(x)
  } else {
    view_static(x)
  }
}

#' Tools to save and view static specs.
#' 
#' These functions are mainly useful for testing.
#' 
#' @param path location to save spec to, or load spec from
#' @param x a gigvis object
#' @param ... other arguments passed to \code{as.vega}
#' @keywords internal
#' @export
save_spec <- function(path, x = last_vis(), ...) {
  assert_that(is.gigvis(x), is.string(path))
  
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
