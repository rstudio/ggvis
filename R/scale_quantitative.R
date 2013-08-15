#' Quantitative scale
#'
#' @inheritParams scale
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
#' scale_quantitative("y")
#' scale_quantitative("y", "pow", 0.5)
#' scale_quantitative("x", clamp = TRUE, nice = FALSE, zero = TRUE)
scale_quantitative <- function(name, trans = "linear", exponent = NULL,
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

  scale(name, trans, subclass = "quantitative",
        exponent = exponent, clamp = clamp, nice = nice, zero = zero,
        domain = domain, range = range, reverse = reverse, round = round)
}
