#' Property: variable
#'
#' Given a quoted object, wrap it in a list and attach a class. The 
#' list-wrapping is needed because attaching a class directly to a symbol
#' is not possible
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
variable <- function(x) {
  stopifnot(is.quoted(x))
  
  structure(list(x), class = "variable")
}

#' @S3method format variable
format.variable <- function(x, ...) {
  paste0("<field> ", deparse(x[[1]]), "\n")
}

#' @S3method print variable
print.variable <- function(x, ...) cat(format(x, ...), "\n", sep = "")

#' @rdname variable
is.variable <- function(x) inherits(x, "variable")

#' @S3method get_value variable
prop_value.variable <- function(x, data) {
  eval(x[[1]], data, baseenv())
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
