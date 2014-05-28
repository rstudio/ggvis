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

#' Create an ordinal scale.
#'
#' A ordinal scale controls the mapping of ordinal and nominal variables to
#' visual properties.
#'
#' Generally, you should create new scales with \code{dscale} because
#' that will automatically set the range to a reasonable default, and it will
#' automatically pick the correct type of scale given the variable type.
#'
#' @inheritParams vega_scale
#' @param points If \code{TRUE}, distributes the ordinal values over a
#'   quantitative range at uniformly spaced points. The spacing of the points
#'   can be adjusted using the padding property. If \code{FALSE}, the ordinal
#'   scale will construct evenly-spaced bands, rather than points.
#' @param padding Applies spacing among ordinal elements in the scale range.
#'   The actual effect depends on how the scale is configured. If the points
#'   parameter is true, the padding value is interpreted as a multiple of the
#'   spacing between points. A reasonable value is 1.0, such that the first and
#'   last point will be offset from the minimum and maximum value by half the
#'   distance between points. Otherwise, padding is typically in the range
#'   [0, 1] and corresponds to the fraction of space in the range interval to
#'   allocate to padding. A value of 0.5 means that the range band width will
#'   be equal to the padding width.
#' @param sort  If \code{TRUE}, the values in the scale domain will be sorted
#'   according to their natural order. The default value is \code{FALSE}.
#' @seealso \url{https://github.com/trifacta/vega/wiki/Scales#ordinal-scale-properties}
#' @seealso \url{https://github.com/mbostock/d3/wiki/Ordinal-Scales}
#' @export
#' @family vega scales
#' @examples
#' p <- mtcars %>% ggvis(~wt, ~mpg) %>% layer_points()
#'
#' scale_ordinal("x")
#' p %>% set_dscale("x", "nominal")
#'
#' scale_ordinal("x", padding = 0.5, points = FALSE)
#' p %>% set_dscale("x", "nominal", points = FALSE)
vega_scale_ordinal <- function(name, points = TRUE, padding = NULL, sort = FALSE,
                          domain = NULL, range = NULL, reverse = FALSE,
                          round = FALSE) {
  assert_that(is.flag(points))
  assert_that(is.null(padding) || (is.numeric(padding) && length(padding) == 1))
  assert_that(is.flag(sort))

  vega_scale(name, "ordinal",
    points = points, padding = padding, sort = sort, subclass = "ordinal",
    domain = domain, range = range, reverse = reverse, round = round)

}

#' Create a quantitative scale
#'
#' A quantitative scale controls the mapping of continuous variables to
#' visual properties.
#'
#' Generally, you should create new scales with \code{dscale} because
#' that will automatically set the range to a reasonable default, and it will
#' automatically pick the correct type of scale given the variable type.
#'
#' @inheritParams vega_scale
#' @param trans A scale transformation: one of "linear", "log", "pow", "sqrt",
#'   "quantile", "quantize", "threshold"
#' @param exponent Sets the exponent of the scale transformation. For pow
#'   transform only.
#' @param clamp  If \code{TRUE}, values that exceed the data domain are clamped
#'   to either the minimum or maximum range value.
#' @param nice If \code{TRUE}, modifies the scale domain to use a more
#'   human-friendly number range (e.g., 7 instead of 6.96).
#' @param zero If \code{TRUE}, ensures that a zero baseline value is included
#'   in the scale domain. This option is ignored for non-quantitative scales.
#' @seealso \url{https://github.com/trifacta/vega/wiki/Scales#quantitative-scale-properties}
#' @family vega scales
#' @export
#' @examples
#' p <- mtcars %>% ggvis(~wt, ~mpg) %>% layer_points()
#'
#' scale_quantitative("y")
#' p %>% set_dscale("y", "numeric")
#'
#' scale_quantitative("y", "pow", 0.5)
#' p %>% set_dscale("y", "numeric", trans = "pow", exp = 0.5)
#'
#' scale_quantitative("x", clamp = TRUE, nice = FALSE, zero = TRUE)
#' p %>% set_dscale("x", "numeric", clamp = TRUE, nice = FALSE, zero = TRUE)
vega_scale_quantitative <- function(name, trans = "linear", exponent = NULL,
                               clamp = FALSE, nice = TRUE, zero = FALSE,
                               domain = NULL, range = NULL, reverse = FALSE,
                               round = FALSE) {

  trans <- match.arg(trans,  c("linear", "log", "pow", "sqrt", "quantile",
    "quantize", "threshold"))
  if (trans != "pow" && !is.null(exponent)) {
    stop("May only set exponent when pow = 'trans'", call. = FALSE)
  }
  assert_that(is.null(exponent) || (
    is.numeric(exponent) && length(exponent) == 1))
  assert_that(is.flag(clamp), is.flag(nice), is.flag(zero))

  vega_scale(name, trans, subclass = "quantitative",
    exponent = exponent, clamp = clamp, nice = nice, zero = zero,
    domain = domain, range = range, reverse = reverse, round = round)
}

#' Create a date/time scale.
#'
#' A date/time scale controls the mapping of date and time variables to
#' visual properties.
#'
#' Generally, you should create new scales with \code{dscale} because
#' that will automatically set the range to a reasonable default, and it will
#' automatically pick the correct type of scale given the variable type.
#'
#' @inheritParams vega_scale
#' @param clamp  If true, values that exceed the data domain are clamped to
#'   either the minimum or maximum range value.
#' @param nice If specified, modifies the scale domain to use a more
#'   human-friendly value range. Should be a string indicating the desired time
#'   interval; legal values are "second", "minute", "hour", "day", "week",
#'   "month", or "year"
#' @param utc if \code{TRUE}, uses UTC times.
#' @seealso \url{https://github.com/trifacta/vega/wiki/Scales#time-scale-properties}
#' @family vega scales
#' @export
#' @examples
#' set.seed(2934)
#' dat <- data.frame(times = as.POSIXct("2013-07-01", tz = "GMT") +
#'                           rnorm(200) * 60 * 60 * 24 * 7)
#' p <- dat %>% ggvis(x = ~times) %>% layer_histograms()
#' p
#'
#' scale_time("x", nice = "year")
#' p %>% set_dscale("x", "datetime", nice = "year")
#'
#' scale_time("x", utc = TRUE)
#' p %>% set_dscale("x", "datetime", utc = TRUE)
vega_scale_time <- function(name, utc = FALSE, clamp = FALSE, nice = NULL,
                       domain = NULL, range = NULL, reverse = FALSE,
                       round = FALSE) {
  assert_that(is.flag(clamp))
  if (!is.null(nice)) {
    nice <- match.arg(nice, c("second", "minute", "hour", "day", "week",
      "month", "year"))
  }

  vega_scale(name, if (utc) "utc" else "time", subclass = "time",
    clamp = clamp, nice = nice, domain = domain, range = range,
    reverse = reverse, round = round)
}
