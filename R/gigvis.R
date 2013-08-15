#' Create a gigvis object, or a node in a gigvis plot.
#'
#' @section Hierarchy:
#' 
#' A gigvis plot has a hierarchical structure, where each node inherits
#' data and properties from its parent. This is somewhat similar to ggplot2,
#' but ggplot2 plots only had a single layer of hierarchy - with gigvis, you
#' can have multiple levels, making it easier to avoid redundancy, both in
#' your specification and in computation.
#' 
#' For example, take a linear model. You often want to display both the 
#' predictions and the standard error from a linear model. In ggplot2, you
#' had to use \code{geom_smooth()}, which was a special geom that combined a
#' line and a ribbon. With gigvis, you can do it yourself by using two marks
#' nested inside a node: (and in fact, this is exactly how 
#' \code{\link{branch_smooth}}) works.
#' 
#' \code{
#' gigvis(mtcars, props(x ~ disp, y ~ mpg),
#'   node(data = transform_smooth(),
#'     mark_area(props(y ~ y_min, y2 ~ y_max, fill = "#eee")),
#'     mark_line()
#'   ),
#'   mark_symbol()
#' ) 
#' }
#'
#' @param data A data \code{\link{pipeline}}, or anything that can be coerced
#'   to a pipeline using \code{\link{as.pipeline}}. Typically this will be 
#'   a data frame, but some transforms will accept other types of objects,
#'   and you can also supply a \code{\link[shiny]{reactive}} whose value changes
#'   over time.
#' @param props a list of \code{\link{props}} defining default mark properties
#'   for this node and all its children.
#' @param ... components: \code{node}s,  \code{\link{marks}}, 
#'   \code{\link{scales}}, \code{\link{axis}} or \code{\link{legend}} objects.
#'   A node can only contain other nodes and marks.
#' @return a \code{gigvis_node} object. Will display the plot when printed;
#'   see \code{\link{save_spec}}, \code{\link{view_static}} and 
#'   \code{\link{view_dynamic}} for other options.
#' @export
#' @import assertthat
gigvis <- function(data = NULL, props = NULL, ...) {
  args <- list(...)
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

#' @export
#' @rdname gigvis
node <- function(..., data = NULL, props = NULL) {
  components <- gigvis_components(...)

  incorrect <- setdiff(names(components), "node")
  if (length(incorrect) > 0) {
    stop("Nodes may only contain other nodes, not scales, legends or axis",
      call. = FALSE)
  }
  
  structure(
    list(
      data = as.pipeline(data),
      props = props,
      children = components$node
    ),
    class = "gigvis_node"
  )
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

#' Is an object a gigvis object?
#' 
#' @export
#' @param x an object to test
#' @keywords internal
is.gigvis <- function(x) inherits(x, "gigvis")

#' @S3method print gigvis
print.gigvis <- function(x, dynamic = NA, ...) {
  set_last_vis(x)

  if (is.na(dynamic)) dynamic <- is.dynamic(x)
  
  if (dynamic) {
    view_dynamic(x, ...)
  } else {
    view_static(x, ...)
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
