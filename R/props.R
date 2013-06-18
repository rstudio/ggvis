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

#' @S3method format gigvis_props
format.gigvis_props <- function(x, ...) {
  labels <- lapply(x, format, ...)
  paste0("* ", names(x), ": ", labels, collapse = "\n")
}
#' @S3method print gigvis_props
print.gigvis_props <- function(x, ...) cat(format(x, ...))

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

is.gigvis_props <- function(x) inherits(x, "gigvis_props")

# Merge two gigvis_props objects
#
# merge_props(props(x ~ x))
# merge_props(props(x ~ x), props(x ~ y))
# merge_props(props(x ~ x, y ~ 1), props(x ~ y))
# merge_props(props(x ~ x, y ~ 1), props(x ~ y, inherit = FALSE))
merge_props <- function(parent = NULL, child = NULL) {
  if (is.null(parent)) return(child)
  if (is.null(child)) return(parent)
  stopifnot(is.gigvis_props(parent), is.gigvis_props(child))

  if (!attr(child, "inherit")) return(child)

  structure(merge_vectors(parent, child), class = "gigvis_props")
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

#' @S3method format variable
format.variable <- function(x, ...) {
  paste0("<field> ", deparse(x[[1]]), "\n")
}
#' @S3method print variable
print.variable <- function(x, ...) cat(format(x, ...), "\n", sep = "")

is.variable <- function(x) inherits(x, "variable")

is.formula <- function(x) inherits(x, "formula")
