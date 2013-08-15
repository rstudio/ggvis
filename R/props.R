#' Manage a list of properties.
#'
#' @param ... A set of name-value pairs. The name should be a valid vega
#'   property.
#' @param inherit If \code{TRUE}, the defaults, will inherit from properties
#'   from the parent node. If \code{FALSE}, it will start from nothing.
#' @export
#' @examples
#' # Set to constant values
#' props(x = 1, y = 2)
#' # Map to variables in the dataset
#' props(x ~ mpg, y ~ cyl)
#' # Set to a constant value in the data space
#' props(x ~ 1, y ~ 1)
props <- function(..., inherit = TRUE) {
  pieces <- compact(list(...))

  # Pull apart formulae in to name and value
  is_formula <- vapply(pieces, is.formula, logical(1))
  formulae <- pieces[is_formula]
  parsed <- lapply(formulae, parse_component)
  names <- lapply(parsed, "[[", "name")
  values <- lapply(parsed, "[[", "value")
  pieces[is_formula] <- values
  names(pieces)[is_formula] <- names

  # Anything else that's not already a prop gets turned into a constant
  is_constant <- !vapply(pieces, is.prop, logical(1))
  pieces[is_constant] <- lapply(pieces[is_constant], prop_const)

  structure(
    pieces,
    inherit = inherit,
    class = "ggvis_props"
  )
}

#' @S3method format ggvis_props
format.ggvis_props <- function(x, ...) {
  labels <- lapply(x, format, ...)
  paste0("* ", names(x), ": ", labels, collapse = "\n")
}
#' @S3method print ggvis_props
print.ggvis_props <- function(x, ...) cat(format(x, ...))

# Return a list of properties that are mapped (not set)
mapped_props <- function(p) {
  if (!is.ggvis_props(p)) {
    stop("p is not a ggvis_props object", call. = FALSE)
  }
  p[vapply(p, is.prop_var, logical(1))]
}

is.ggvis_props <- function(x) inherits(x, "ggvis_props")

# Merge two ggvis_props objects
#
# merge_props(props(x ~ x))
# merge_props(props(x ~ x), props(x ~ y))
# merge_props(props(x ~ x, y ~ 1), props(x ~ y))
# merge_props(props(x ~ x, y ~ 1), props(x ~ y, inherit = FALSE))
merge_props <- function(parent = NULL, child = NULL) {
  if (is.null(parent)) return(child)
  if (is.null(child)) return(parent)
  stopifnot(is.ggvis_props(parent), is.ggvis_props(child))

  if (identical(attr(child, "inherit"), FALSE)) return(child)

  structure(merge_vectors(parent, child), class = "ggvis_props")
}

parse_component <- function(x) {
  stopifnot(is.formula(x))
  if (length(x) != 3) {
    stop(x, " not in form name ~ value", call. = FALSE)
  }

  name <- as.character(x[[2]])
  if (is.atomic(x[[3]])) {
    value <- prop_const(x[[3]], scale = TRUE)
  } else {
    value <- prop_var(x[[3]])
  }


  list(name = name, value = value)
}

is.formula <- function(x) inherits(x, "formula")
