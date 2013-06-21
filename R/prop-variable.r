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
    # var is a single variable; just return it
    as.character(var)

  } else if (is.language(var)) {
    # var is calculated; translate some of the special characters (so it doesn't
    # cause problems in the Vega spec) and hash it (so that other calculated
    # columns in the same data set won't have the same name).
    var_str <- deparse(var)
    paste(gsub("[^a-zA-Z0-9]", "_", var_str),
          digest(var_str, algo = "crc32"), sep = "_")

  } else {
    # var is a constant
    ""
  }
}

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
