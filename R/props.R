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
#' @section Enter, exit, hover, and update events:
#'
#' There are four different property events that the marks
#' can use. These can, for example, be used to change the appearance of a mark
#' when the mouse cursor is hovering over it: when the mark is hovered over, it
#' uses the hover event, and when the mark isn't hovered over, it uses the
#' update event
#'
#' \itemize{
#'   \item enter: This event is used by marks when they are added to a plot.
#'   \item update: This event is used by marks after they have entered, and
#'     also after they have been hovered over.
#'   \item exit: This event is used by marks as they are removed from a plot.
#'   \item hover: This event is used when the mouse cursor is over the mark.
#' }
#'
#' You can specify the event for a property, by putting a period and the
#' event after the property name. For example,
#' \code{props(fill.update := "black", fill.hover := "red")} will make a mark
#' have a black fill normally, and red fill when it is hovered over.
#'
#' The default event is update, so if you run \code{props(fill := "red")},
#' this is equivalent to \code{props(fill.update := "red")}.
#'
#' In practice, the enter and exit events are useful only when the update has
#' a duration (and is therefore not instantaneous). The update event can be
#' thought of as the "default" state.
#'
#' @section Key property:
#'
#' In addition to the standard properties, there is a special optional property
#' called \code{key}. This is useful for plots with dynamic data and smooth
#' transitions: as the data changes, the key is used to tell the plot how the
#' new data rows should be matched to the old data rows. Note that the key must
#' be an unscaled value. Additionally, the key property doesn't have a event,
#' since it is independent of enter, update, exit, and hover events.
#'
#' @template properties
#' @param ... A set of name-value pairs. The name should be a valid vega
#'   property.
#'
#'   The first two unnamed components are taken to be \code{x} and \code{y}.
#'   Any additional unnamed components will raise an error.
#' @param .props When calling \code{props} from other functions, you'll
#'   often have a list of quoted function functions. You can pass that function
#'   to the \code{.props} argument instead of messing around with
#'   substitute. In other words, \code{.props} lets you opt out of the
#'   non-standard evaluation that \code{props} does.
#' @param inherit If \code{TRUE}, the defaults, will inherit from properties
#'   from the parent layer If \code{FALSE}, it will start from nothing.
#' @param env The environment in which to evaluate variable properties.
#' @export
#' @examples
#' # Set to constant values
#' props(x := 1, y := 2)
#' # Map to variables in the dataset
#' props(x = ~mpg, y = ~cyl)
#' # Set to a constant value in the data space
#' props(x = 1, y = 1)
#' # Use an interactive slider
#' props(opacity := input_slider(0, 1))
#'
#' # To control other settings (like custom scales, mult and offset)
#' # use a prop object
#' props(prop("x", "old", scale = "x", offset = -1))
#'
#' # Red when hovered over, black otherwise (these are equivalent)
#' props(fill := "black", fill.hover := "red")
#' props(fill.update := "black", fill.hover := "red")
#'
#' # Use a column called id as the key (for dynamic data)
#' props(key := ~id)
#'
#' # Explicitly create prop objects. The following are equivalent:
#' props(fill = ~cyl)
#' props(fill.update = ~cyl)
#' props(prop("fill", ~cyl))
#' props(prop("fill", ~cyl, scale = "fill", event = "update"))
#'
#' # Prop objects can be programmatically created and added:
#' property <- "fill"
#' expr <- parse(text = "wt/mpg")[[1]]
#' p <- prop(property, expr)
#' props(p)
#'
#' # Using .props
#' props(.props = list(x = 1, y = 2))
#' props(.props = list(x = ~mpg, y = ~cyl))
#' props(.props = list(quote(x := ~mpg)))
props <- function(..., .props = NULL, inherit = TRUE, env = parent.frame()) {
  check_empty_args()

  args <- pluck(lazyeval::lazy_dots(...), "expr")
  all <- args_to_props(c(args, .props), env)

  structure(
    all,
    inherit = inherit,
    class = "ggvis_props"
  )
}

uses_colon_equals <- function(x) {
  is.call(x) && identical(x[[1]], quote(`:=`))
}

# Given a list of unevaluated expressions, return a list of prop objects
args_to_props <- function(args, env) {
  # Given a name and expression, create a prop
  expr_to_prop <- function(name, expr, scale = NULL) {
    name <- strsplit(name, ".", fixed = TRUE)[[1]]
    property <- name[1]
    event <- if (length(name) > 1) name[2] else NULL
    val <- eval(expr, env)
    prop(property, val, scale = scale, event = event, label = as.character(val))
  }

  # Given a prop, get the full name, like x.update
  prop_full_name <- function(p) {
    # Use this form so that if event is NULL, there won't be trailing .
    paste(c(p$property, p$event), collapse = ".")
  }

  arg_names <- names2(args)
  named_args <- args[arg_names != ""]
  unnamed_args <- args[arg_names == ""]

  # First pass: Convert named arguments to props
  named_args <- Map(named_args, names(named_args),
    f = function(x, name) expr_to_prop(name, x, scale = TRUE)
  )

  # Second pass: Convert unnamed arguments to prop objects, or raw value
  unnamed_args <- lapply(unnamed_args, function(x) {
    if (uses_colon_equals(x)) {
      expr_to_prop(deparse(x[[2]]), x[[3]], scale = FALSE)
    } else {
      # It's either prop() call, or an unnamed value
      eval(x, env)
    }
  })

  # Some of the unnamed items are now props, others are raw values.
  is_prop <- vapply(unnamed_args, is.prop, logical(1))
  unnamed_props <- unnamed_args[is_prop]
  unnamed_values <- unnamed_args[!is_prop]

  # Assign full name, like x.update
  names(named_args) <- vapply(named_args, prop_full_name, character(1))
  names(unnamed_props) <- vapply(unnamed_props, prop_full_name, character(1))

  # Assign missing names to unnamed values
  missing_names <- setdiff(c("x.update", "y.update"),
                           c(names(named_args), names(unnamed_props)))
  if (length(unnamed_values) > length(missing_names)) {
    stop("Too many unnamed properties (can only have x and y)", call. = FALSE)
  }
  names(unnamed_values) <- missing_names[seq_along(unnamed_values)]

  # Final pass: Convert the now-named values to props
  unnamed_values <- Map(unnamed_values, names(unnamed_values),
    f = function(x, name) expr_to_prop(name, x, scale = TRUE)
  )

  c(named_args, unnamed_props, unnamed_values)
}

#' @export
format.ggvis_props <- function(x, ...) {
  labels <- lapply(x, format, ...)
  if (length(labels) > 0) {
    inherit <- if (!attr(x, "inherit", TRUE)) "\ninherit: FALSE" else ""
    paste0(
      paste0("* ", names(x), ": ", labels, collapse = "\n"),
      inherit,
      sep = "\n")
  } else {
    "props()"
  }
}
#' @export
print.ggvis_props <- function(x, ...) cat(format(x, ...))

#' @rdname props
#' @export
#' @param x an object to test for props-ness.
is.ggvis_props <- function(x) inherits(x, "ggvis_props")

#' @export
`[.ggvis_props` <- function(x, idx) {
  structure(NextMethod(), inherit = attr(x, "inherit", TRUE), class = "ggvis_props")
}

# Merge two ggvis_props objects
#
# merge_props(props(x = ~x))
# merge_props(props(x = ~x), props(x = ~y))
# merge_props(props(x = ~x, y = 1), props(x = ~y))
# merge_props(props(x = ~x, y = 1), props(x = ~y, inherit = FALSE))
merge_props <- function(parent = NULL, child = NULL,
                        inherit = attr(child, "inherit", TRUE)) {
  if (is.null(parent)) return(child)
  if (is.null(child)) return(parent)
  stopifnot(is.ggvis_props(parent), is.ggvis_props(child))

  if (identical(inherit, FALSE)) return(child)

  structure(merge_vectors(parent, child),
    inherit = attr(parent, "inherit", TRUE),
    class = "ggvis_props")
}

is.formula <- function(x) inherits(x, "formula")

find_prop_var <- function(props, name) {
  prop <- props[[name]]
  if (is.null(prop)) {
    stop("Can't find prop ", name, call. = FALSE)
  }

  if (!is.prop_variable(prop)) {
    stop("Visual property ", name, " is not a variable", call. = FALSE)
  }

  formula(prop)
}
