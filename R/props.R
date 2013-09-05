#' Manage a list of properties.
#'
#' \code{props()} provides a tool for concise creation of \code{prop} objects
#' using a set of conventions designed to capture the most common use cases.
#' If you need something less common, you'll need to use \code{\link{prop}} to
#' access all possible options.
#' 
#' @section Heuristics:
#' 
#' If the values are not already objects of class \code{prop}, \code{props}
#' uses the following heuristics to when creating the prop:
#'
#' \itemize{
#'  \item atomic vectors, e.g. \code{x = 1}: scaled = FALSE
#'  \item an interative input, e.g. \code{x = input_slider}:
#'     scaled = FALSE
#'  \item a formula containing a single value, e.g. \code{x ~ 1}: 
#'     scaled = TRUE
#'  \item a formula containing a name or expression, \code{x ~ mpg}:
#'     scaled = TRUE
#' }
#'
#' @section Non-standard evaluation:
#'
#' \code{props} uses non-standard evaluation in a slightly unusual way: 
#' if you provide a formula input, the LHS of the formula will provide the 
#' name of the component. In otherwise, \code{props(x = y ~ 1)} is the
#' same as \code{props(y ~ 1)}.
#' 
#' You can combine variables from the dataset and variables defined in the 
#' local environment: expressions will be evaluated in the environment which
#' the formula was defined.
#' 
#' If you have the name of a variable in a string, see the
#' props vignette for how to create the needed property mapping.
#' 
#' @template properties
#' @param ... A set of name-value pairs. The name should be a valid vega
#'   property.
#' @param inherit If \code{TRUE}, the defaults, will inherit from properties
#'   from the parent branch If \code{FALSE}, it will start from nothing.
#' @export
#' @aliases ":="
#' @examples
#' # Set to constant values
#' props(x := 1, y := 2)
#' # Map to variables in the dataset
#' props(x = ~ mpg, y = ~ cyl)
#' # Set to a constant value in the data space
#' props(x = 1, y = 1)
#' # Use an interactive slider
#' props(opacity := input_slider(0, 1))
#' 
#' # To control other settings (like custom scales, mult and offset)
#' # use a prop object
#' props(x = prop("old", scale = TRUE, offset = -1))
props <- function(..., inherit = TRUE) {
  check_empty_args()
  args <- dots(...)
  env <- parent.frame()
  
  # If named, use regular evaluation and scale
  scaled <- lapply(args[named(args)], function(x) {
    val <- eval(x, env)
    prop(val, scale = TRUE)
  })
  
  # If unnamed, check that it uses := and don't scale
  unscaled <- lapply(args[!named(args)], function(x) {
    check_unscaled_form(x)

    val <- eval(x[[3]], env)
    prop(val, scale = FALSE)
  })
  names(unscaled) <- vapply(args[!named(args)], function(x) as.character(x[[2]]),
    character(1))

  structure(
    c(scaled, unscaled),
    inherit = inherit,
    class = "ggvis_props"
  )
}

check_unscaled_form <- function(x) {
  if (!is.call(x) || !identical(x[[1]], quote(`:=`))) {
    stop("Arguments to props must use either := or =", call. = FALSE)
  }
}

#' @S3method format ggvis_props
format.ggvis_props <- function(x, ...) {
  labels <- lapply(x, format, ...)
  if (length(labels) > 0) {
    paste0("* ", names(x), ": ", labels, collapse = "\n")  
  } else {
    "props()"
  }
}
#' @S3method print ggvis_props
print.ggvis_props <- function(x, ...) cat(format(x, ...))

#' @rdname props
#' @export
#' @param x an object to test for props-ness.
is.ggvis_props <- function(x) inherits(x, "ggvis_props")

#' @S3method [ ggvis_props
`[.ggvis_props` <- function(x, idx) {
  structure(.subset(x, idx), inherit = x$inherit, class = "ggvis_props")
}

# Merge two ggvis_props objects
#
# merge_props(props(x = ~ x))
# merge_props(props(x = ~ x), props(x = ~ y))
# merge_props(props(x = ~ x, y = 1), props(x = ~ y))
# merge_props(props(x = ~ x, y = 1), props(x = ~ y, inherit = FALSE))
merge_props <- function(parent = NULL, child = NULL) {
  if (is.null(parent)) return(child)
  if (is.null(child)) return(parent)
  stopifnot(is.ggvis_props(parent), is.ggvis_props(child))

  if (identical(attr(child, "inherit"), FALSE)) return(child)

  structure(merge_vectors(parent, child), class = "ggvis_props")
}

is.formula <- function(x) inherits(x, "formula")
