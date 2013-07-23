#' Property: variable
#'
#' Given a quoted object, wrap it in a list and attach a class. The
#' list-wrapping is needed because attaching a class directly to a symbol
#' is not possible.
#'
#' Long-term this function needs to behave more like dplyr::partial_eval so
#' that it captures local values immediately.
#'
#' @param x A quoted object
#' @examples
#' variable(quote(x))
#' variable(quote(1))
#' variable(quote(x * y))
#'
#' v <- variable(quote(cyl))
#' prop_value(v, mtcars)
variable <- function(x, scale = TRUE) {
  stopifnot(is.quoted(x))

  structure(list(x), class = c("variable", "prop"))
}

#' @S3method format variable
format.variable <- function(x, ...) {
  paste0("<field> ", deparse(x[[1]]))
}

#' @S3method print variable
print.variable <- function(x, ...) cat(format(x, ...), "\n", sep = "")

#' @rdname variable
is.variable <- function(x) inherits(x, "variable")

#' @S3method prop_value variable
prop_value.variable <- function(x, data, processed = FALSE) {
  if (processed)
    data[[prop_name(x)]]
  else
    eval(x[[1]], data, baseenv())
}

#' @S3method prop_name variable
prop_name.variable <- function(x) {
  var <- x[[1]]

  if (is.symbol(var)) {
    safe_jsvar(as.character(var))

  } else if (is.language(var)) {
    # var is calculated from an expression; get a unique, JS-safe name. Prepend
    # a string to so that an expression with same text as a var will have a
    # different hash, e.g., the expression wt/mpg vs. the variable `wt/mpg`.
    safe_jsvar(paste("[e]", deparse(var)))
    
  } else {
    # var is a constant
    ""
  }
}

#' @S3method prop_scale variable
prop_scale.variable <- function(x, default_scale) {
  default_scale
}

#' @S3method prop_vega variable
prop_vega.variable <- function(x, default_scale) {
  compact(list(
    field = paste0("data.", prop_name(x)),
    scale = prop_scale(x, default_scale)
  ))
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

as.variable <- function(x) UseMethod("as.variable")
#' @S3method as.variable character
as.variable.character <- function(x) variable(as.name(x))
#' @S3method as.variable name
as.variable.name <- function(x) variable(x)
#' @S3method as.variable call
as.variable.call <- function(x) variable(x)
#' @S3method as.variable variable
as.variable.variable <- function(x) x
