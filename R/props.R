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
  pieces <- list(...)
  is_formula <- vapply(pieces, is.formula, logical(1))

  # Pull apart formulae in to name and value
  formulae <- pieces[is_formula]
  parsed <- lapply(formulae, parse_component)
  names <- lapply(parsed, "[[", "name")
  values <- lapply(parsed, "[[", "value")

  structure(
    c(pieces[!is_formula], setNames(values, names)),
    inherit = inherit,
    class = "gigvis_props"
  )
}

# Return a list of properties that are mapped (not set)
mapped_props <- function(p) {
  if (!is.gigvis_props(p)) {
    stop("p is not a gigvis_props object", call. = FALSE)
  }
  p[vapply(p, is.variable, logical(1))]
}

# Given a variable object, return a string representation of the value
# @examples
# p <- props(x ~ mpg, y = 10)
# as.character.variable(p$x)
#' @S3method as.character variable
as.character.variable <- function(x, ...) {
  if (!is.variable(x)) {
    stop("x is not a variable object", call. = FALSE)
  }
  deparse(x[[1]])
}

#' @S3method print gigvis_props
print.gigvis_props <- function(x) str(x)

is.gigvis_props <- function(x) inherits(x, "gigvis_props")

# Merge two gigvis_props objects
merge_props <- function(a, b) {
  if ((!is.null(a) && !is.gigvis_props(a)) ||
      (!is.null(b) && !is.gigvis_props(b))) {
    stop("a and b are not both gigvis_props objects.", call. = FALSE)
  }

  structure(merge_vectors(a, b), class = "gigvis_props")
}

parse_component <- function(x) {
  stopifnot(is.formula(x))
  if (length(x) != 3) {
    stop(x, " not in form name ~ value", call. = FALSE)
  }

  name <- as.character(x[[2]])
  value <- variable(x[[3]])

  list(name = name, value = value)
}

# Given a quoted object, wrap it in a list and attach a class. The list-wrapping
# is needed because attaching a class directly to a symbol object results in
# buggy behavior in R. For example, using str() will remove class:
# x <- structure(quote(foo), class="bar"); str(x); str(x)
variable <- function(x) structure(list(x), class = "variable")

is.variable <- function(x) inherits(x, "variable")

is.formula <- function(x) inherits(x, "formula")
