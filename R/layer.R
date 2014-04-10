# Add a simple mark layer to a ggvis object.
add_mark_layer <- function(vis, type = NULL, props = NULL, data = NULL,
                           data_name = "unnamed_data") {
  if (is.null(data)) {
    data <- vis$cur_data

  } else {
    if (!is.reactive(data))
      data <- as.reactive(data)

    vis <- register_data(vis, data, prefix = ,
                         update_current = FALSE)
  }


  # Calculate the props for this layer
  if (is.null(props)) {
    props <- vis$cur_props

  } else {
    # Merge new props into parent, and then register the props with the vis
    props <- merge_props(vis$cur_props, props)
    vis$props[[props_id(props)]] <- props
  }

  add_layer(vis, mark(type, props = props, data = data))
}

#' Create a new layer.
#'
#' Layers are used to describe the data hierarchy of ggvis. As well as
#' using this function to create them, you can also use the many specialised
#' \code{layer_} functions that combine marks and transforms to create
#' useful visualisations.
#'
#' @section Hierarchy:
#'
#' A ggvis plot has a hierarchical structure, where each layer inherits
#' data and properties from its parent. This is somewhat similar to ggplot2,
#' but ggplot2 plots only had a single layer of hierarchy - with ggvis, you
#' can have multiple levels, making it easier to avoid redundancy, both in
#' your specification and in computation.
#'
#' For example, take a linear model. You often want to display both the
#' predictions and the standard error from a linear model. In ggplot2, you
#' had to use \code{geom_smooth()}, which was a special geom that combined a
#' line and a ribbon. With ggvis, you can do it yourself by using two marks
#' nested inside a layer: (and in fact, this is exactly how
#' \code{\link{layer_smooth}}) works.
#'
#' \code{
#' ggvis(mtcars, props(x = ~disp, y = ~mpg),
#'   layer(transform_smooth(),
#'     layer_area(props(y = ~y_min, y2 = ~y_max, fill := "#eee")),
#'     layer_path()
#'   ),
#'   layer_point()
#' )
#' }
#' @param ... components: data, \code{\link{props}}, \code{layer}es,
#'   or \code{\link{marks}}
#' @param drop_named if \code{FALSE}, the default, will throw an error if
#'   any of the arguments in \code{...} are named. If \code{TRUE} it will
#'   silently drop them - this is primarily useful for \code{layer_} functions
#'   which send named arguments to the transform, and unnamed arguments to the
#'   layer.
#' @export
layer <- function(..., drop_named = FALSE) {
  check_empty_args()

  comp <- parse_components(..., drop_named = drop_named)
  check_layer_components(comp)
  class(comp) <- "layer"

  comp
}

#' @rdname layer
#' @export
#' @param x object to test for "layer"-ness
is.layer <- function(x) inherits(x, "layer")

check_layer_components <- function(x) {
  incorrect <- setdiff(names(x), c("children", "data", "props", "NULL"))
  if (length(incorrect) > 0) {
    stop("Layer may only contain other layers, not scales, legends or axes",
      call. = FALSE)
  }
}

parse_components <- function(..., drop_named = FALSE) {
  args <- list(...)
  named <- named(args)

  if (any(named)) {
    if (drop_named) {
      args <- args[!named]
    } else {
      stop("Inputs to ggvis/layer should not be named", call. = FALSE)
    }
  }
  names(args) <- NULL

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

  components$scales <- scales(.scales = components$scales)
  components
}

component_type <- function(x) UseMethod("component_type")
#' @export
component_type.layer <- function(x) "children"
#' @export
component_type.scale <- function(x) "scales"
#' @export
component_type.vega_legend <- function(x) "legends"
#' @export
component_type.vega_axis <- function(x) "axes"
#' @export
component_type.ggvis_props <- function(x) "props"
#' @export
component_type.ggvis_opts <- function(x) "opts"
#' @export
component_type.default <- function(x) "data"
#' @export
component_type.handler <- function(x) "handlers"
#' @export
component_type.NULL <- function(x) "NULL"
