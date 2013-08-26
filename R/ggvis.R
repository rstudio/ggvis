#' Create a ggvis object, or a node in a ggvis plot.
#'
#' @section Hierarchy:
#'
#' A ggvis plot has a hierarchical structure, where each node inherits
#' data and properties from its parent. This is somewhat similar to ggplot2,
#' but ggplot2 plots only had a single layer of hierarchy - with ggvis, you
#' can have multiple levels, making it easier to avoid redundancy, both in
#' your specification and in computation.
#'
#' For example, take a linear model. You often want to display both the
#' predictions and the standard error from a linear model. In ggplot2, you
#' had to use \code{geom_smooth()}, which was a special geom that combined a
#' line and a ribbon. With ggvis, you can do it yourself by using two marks
#' nested inside a node: (and in fact, this is exactly how
#' \code{\link{branch_smooth}}) works.
#'
#' \code{
#' ggvis(mtcars, props(x ~ disp, y ~ mpg),
#'   node(data = transform_smooth(),
#'     mark_area(props(y ~ y_min, y2 ~ y_max, fill = "#eee")),
#'     mark_line()
#'   ),
#'   mark_symbol()
#' )
#' }
#'
#' @param ... components: data, \code{\link{props}}, \code{node}s,
#'   \code{\link{marks}}, \code{\link{scales}}, \code{\link{axis}} or 
#'   \code{\link{legend}} objects. A node can only contain other nodes and 
#'   marks.
#' @return a \code{ggvis_node} object. Will display the plot when printed;
#'   see \code{\link{save_spec}}, \code{\link{view_static}} and
#'   \code{\link{view_dynamic}} for other options.
#' @export
#' @import assertthat
ggvis <- function(...) {
  check_empty_args()
  vis <- ggvis_node(subclass = "ggvis", ...)
  set_last_vis(vis)
  vis
}

#' @export
#' @rdname ggvis
node <- function(...) {
  check_empty_args()
  node <- ggvis_node(...)

  incorrect <- setdiff(names(node), c("children", "data", "props"))
  if (length(incorrect) > 0) {
    stop("Nodes may only contain other nodes, not scales, legends or axes",
      call. = FALSE)
  }
  
  node
}

ggvis_node <- function(..., subclass = character()) {
  args <- unname(list(...))
  types <- vapply(args, component_type, character(1))

  components <- split(args, types)
  components$props <- Reduce(merge_props, components$props)
  if (length(components$data) > 0) {
    # Capture names from ...
    names <- dot_names(...)[types == "data"]
    # Convert each component to a pipeline, preserving original names
    pls <- Map(as.pipeline, components$data, name = names)
    # Collapse into single pipeline
    pl <- structure(unlist(pls, recursive = FALSE), class = "pipeline")    
    # Trim any redundant sources
    components$data <- trim_to_source(pl)
  }
  
  components$scales <- scales(.scales = components$scale)
  
  structure(components, class = c(subclass, "ggvis_node"))
}

component_type <- function(x) UseMethod("component_type")
#' @S3method component_type ggvis_node
component_type.ggvis_node <- function(x) "children"
#' @S3method component_type scale
component_type.scale <- function(x) "scales"
#' @S3method component_type vega_legend
component_type.vega_legend <- function(x) "legends"
#' @S3method component_type vega_axis
component_type.vega_axis <- function(x) "axes"
#' @S3method component_type ggvis_props
component_type.ggvis_props <- function(x) "props"
#' @S3method component_type default
component_type.default <- function(x) "data"

#' Is an object a ggvis object?
#'
#' @export
#' @param x an object to test
#' @keywords internal
is.ggvis <- function(x) inherits(x, "ggvis")

#' @S3method print ggvis
print.ggvis <- function(x, dynamic = NA, ...) {
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
