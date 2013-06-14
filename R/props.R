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
                                   
 c(pieces[!is_formula], setNames(values, names))
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

variable <- function(x) structure(x, class = "variable")

is.formula <- function(x) inherits(x, "formula")
