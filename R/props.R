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
#' @section Enter, exit, hover, and update propsets:
#'
#' There are four different sets of properties (propsets) that the marks
#' can use. These can, for example, be used to change the appearance of a mark
#' when the mouse cursor is hovering over it: when the mark is hovered over, it
#' uses the hover propset, and when the mark isn't hovered over, it uses the
#' update propset.
#'
#' \itemize{
#'   \item enter: This propset is used by marks when they are added to a plot.
#'   \item update: This propset is used by marks after they have entered, and
#'     also after they have been hovered over.
#'   \item exit: This propset is used by marks as they are removed from a plot.
#'   \item hover: This propset is used when the mouse cursor is over the mark.
#' }
#'
#' You can specify the propset for a property, by putting a period and the
#' propset after the property name. For example,
#' \code{props(fill.update := "black", fill.hover := "red")} will make a mark
#' have a black fill normally, and red fill when it is hovered over.
#'
#' The default propset is update, so if you run \code{props(fill := "red")},
#' this is equivalent to \code{props(fill.update := "red")}.
#'
#' In practice, the enter and exit propsets are useful only when the update has
#' a duration (and is therefore not instantaneous). The update propset can be
#' thought of as the "default" state.
#'
#' @section Key property:
#'
#' In addition to the standard properties, there is a special optional property
#' called \code{key}. This is useful for plots with dynamic data and smooth
#' transitions: as the data changes, the key is used to tell the plot how the
#' new data rows should be matched to the old data rows. Note that the key must
#' be an unscaled value. Additionally, the key property doesn't have a propset,
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
#' props(x = prop("old", scale = TRUE, offset = -1))
#'
#' # Red when hovered over, black otherwise (these are equivalent)
#' props(fill := "black", fill.hover := "red")
#' props(fill.update := "black", fill.hover := "red")
#'
#' # Use a column called id as the key (for dynamic data)
#' props(key := ~id)
#'
#' # Using .props
#' props(.props = list(x = 1, y = 2))
#' props(.props = list(x = ~mpg, y = ~cyl))
#' props(.props = list(quote(x := ~mpg)))
props <- function(..., .props = NULL, inherit = TRUE) {
  check_empty_args()
  args <- props_default_names(c(dots(...), .props))
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

  all <- c(scaled, unscaled)

  if (!is.null(all$key)) {
    if (all$key$scale) {
      stop("The special prop 'key' must be unscaled. Use `key :=` instead of `key =`")
    }
    if (all$key$type == "constant") {
      stop("The special prop 'key' cannot be a constant.")
    }
  }

  # Append ".update" to any props that don't already have ".enter", ".exit",
  # ".update", ".hover", or ".brush". But don't modify a prop named "key".
  needs_propset <- !has_propset(names(all)) & names(all) != "key"
  names(all)[needs_propset] <- paste0(names(all)[needs_propset], ".update")

  structure(
    all,
    inherit = inherit,
    class = "ggvis_props"
  )
}

#' @export
#' @rdname props
#' @param prop,value Name of property and the unscaled value that should
#'   be mapped to it.
`:=` <- function(prop, value) {
  stop(":= may only be used inside props", call. = FALSE)
}

uses_colon_equals <- function(x) {
  is.call(x) && identical(x[[1]], quote(`:=`))
}

check_unscaled_form <- function(x) {
  if (!uses_colon_equals(x)) {
    stop("Arguments to props must use either := or =", call. = FALSE)
  }
}

#' @export
format.ggvis_props <- function(x, ...) {
  labels <- lapply(x, format, ...)
  if (length(labels) > 0) {
    inherit <- if (!attr(x, "inherit")) "\ninherit: FALSE" else ""
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
  structure(NextMethod(), inherit = attr(x, "inherit"), class = "ggvis_props")
}

# Merge two ggvis_props objects
#
# merge_props(props(x = ~x))
# merge_props(props(x = ~x), props(x = ~y))
# merge_props(props(x = ~x, y = 1), props(x = ~y))
# merge_props(props(x = ~x, y = 1), props(x = ~y, inherit = FALSE))
merge_props <- function(parent = NULL, child = NULL) {
  if (is.null(parent)) return(child)
  if (is.null(child)) return(parent)
  stopifnot(is.ggvis_props(parent), is.ggvis_props(child))

  if (identical(attr(child, "inherit"), FALSE)) return(child)

  structure(merge_vectors(parent, child), inherit = attr(parent, "inherit"),
    class = "ggvis_props")
}

is.formula <- function(x) inherits(x, "formula")


# Given a props object, return a unique name for that set of props
props_id <- function(x) {
  digest(x, algo = "crc32")
}
