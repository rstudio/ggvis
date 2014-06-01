#' Create a new "scale" object.
#'
#' A scale object is a close mapping to a vega mark object. Vega scales
#' are documented in \url{https://github.com/trifacta/vega/wiki/Scales}.
#'
#' This function is designed to be used by authors of new types of scale.
#' If you are a ggvis user, please use one of the more specific scale
#' functions starting with the \code{scale_}.
#'
#' This is very close, but not exactly a vega scale object. Instead of being a
#' named list with a set of values, the domain can be  a vector of values, or a
#' reactive that returns such values.
#'
#' @param name name of the scale.
#' @param type type of scale. Should be one of "linear", "ordinal", "time",
#'   "utc", "linear", "log", "pow", "sqrt", "quantile", "quantize", "threshold".
#' @param domain The domain of the scale, representing the set of data values.
#'   For ordinal scales, a character vector; for quantitative scales, a numeric
#'   vector of length two. Either value (but not both) may be NA, in which
#'   case \code{domainMin} or \code{domainMax} is set.
#' @param range The range of the scale, representing the set of visual values.
#'   For numeric values, the range can take the form of a two-element array with
#'   minimum and maximum values. For ordinal data, the range may by an array of
#'   desired output values, which are mapped to elements in the specified
#'   domain. The following range literals are also available: "width", "height",
#'   "shapes", "category10", "category20".
#' @param reverse  If true, flips the scale range.
#' @param round If true, rounds numeric output values to integers. This can be
#'   helpful for snapping to the pixel grid.
#' @param ... other named arguments.
#' @param subclass Class name for subclass.  Will have \code{scale_} prepended.
#' @seealso \url{https://github.com/trifacta/vega/wiki/Scales}
#' @export
#' @keywords internal
#' @examples
#' vega_scale("x", "linear")
#' vega_scale("x", "ord")
vega_scale <- function(name, type = NULL, domain = NULL, range = NULL,
                  reverse = FALSE, round = FALSE, ..., subclass = NULL) {
  assert_that(is.string(name))
  type <- match.arg(type, c("linear", "ordinal", "time", "utc", "log",
    "pow", "sqrt", "quantile", "quantize", "threshold"))
  assert_that(is.flag(reverse), is.flag(round))

  if (!is.null(subclass)) {
    assert_that(is.string(subclass))
    subclass <- paste0("scale_", subclass)
  }

  structure(
    drop_nulls(c(
      list(name = name, type = type, reverse = reverse, round = round,
           domain = domain, ...),
      range_prop(range, "range")
    )),
    class = c(subclass, "vega_scale")
  )
}

#' @export
#' @rdname vega_scale
#' @param x object to test for scale-ness
is.vega_scale <- function(x) inherits(x, "vega_scale")

#' @export
format.vega_scale <- format.vega_axis

#' @export
print.vega_scale <- print.vega_axis
