#' Create a new branch.
#' 
#' Branches are used to describe the data hierarchy of ggvis. As well as 
#' using this function to create them, you can also use the many specialised
#' \code{branch_} functions that combine marks and transforms to create 
#' useful visualisations.
#'
#' @section Hierarchy:
#'
#' A ggvis plot has a hierarchical structure, where each branch inherits
#' data and properties from its parent. This is somewhat similar to ggplot2,
#' but ggplot2 plots only had a single layer of hierarchy - with ggvis, you
#' can have multiple levels, making it easier to avoid redundancy, both in
#' your specification and in computation.
#'
#' For example, take a linear model. You often want to display both the
#' predictions and the standard error from a linear model. In ggplot2, you
#' had to use \code{geom_smooth()}, which was a special geom that combined a
#' line and a ribbon. With ggvis, you can do it yourself by using two marks
#' nested inside a branch: (and in fact, this is exactly how
#' \code{\link{branch_smooth}}) works.
#'
#' \code{
#' ggvis(mtcars, props(x = ~disp, y = ~mpg),
#'   branch(transform_smooth(),
#'     mark_area(props(y = ~y_min, y2 = ~y_max, fill := "#eee")),
#'     mark_line()
#'   ),
#'   mark_symbol()
#' )
#' }
#' @param ... components: data, \code{\link{props}}, \code{branch}es,
#'   or \code{\link{marks}}
#' @param drop_unnamed if \code{FALSE}, the default, will throw an error if
#'   any of the arguments in \code{...} are named. If \code{TRUE} it will
#'   silently drop them - this is primarily useful for \code{branch_} functions
#'   which send named arguments to the transform, and unnamed arguments to the
#'   branch.
#' @export
branch <- function(..., drop_named = FALSE) {
  check_empty_args()
  
  comp <- parse_components(..., drop_named = drop_named)
  check_branch_components(comp)
  class(comp) <- "branch"

  comp
}

#' @rdname branch
#' @export
#' @param x object to test for "branch"-ness
is.branch <- function(x) inherits(x, "branch")

check_branch_components <- function(x) {
  incorrect <- setdiff(names(x), c("children", "data", "props"))
  if (length(incorrect) > 0) {
    stop("Branch may only contain other branches, not scales, legends or axes",
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
      stop("Inputs to ggvis/branch should not be named", call. = FALSE)
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
#' @S3method component_type branch
component_type.branch <- function(x) "children"
#' @S3method component_type scale
component_type.scale <- function(x) "scales"
#' @S3method component_type vega_legend
component_type.vega_legend <- function(x) "legends"
#' @S3method component_type vega_axis
component_type.vega_axis <- function(x) "axes"
#' @S3method component_type ggvis_props
component_type.ggvis_props <- function(x) "props"
#' @S3method component_type ggvis_opts
component_type.ggvis_opts <- function(x) "opts"
#' @S3method component_type default
component_type.default <- function(x) "data"
