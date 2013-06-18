
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
