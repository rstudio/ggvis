#' Date time scale.
#'
#' @inheritParams scale
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
#' scale_time("x", nice = "year")
#' scale_time("x", utc = TRUE)
scale_time <- function(name, utc = FALSE, clamp = FALSE, nice = NULL,
                       domain = NULL, range = NULL, reverse = FALSE,
                       round = FALSE) {
  assert_that(is.flag(clamp))
  if (!is.null(nice)) {
    nice <- match.arg(nice, c("second", "minute", "hour", "day", "week",
      "month", "year"))
  }

  scale(name, if (utc) "utc" else "time", subclass = "time",
    clamp = clamp, nice = nice, domain = domain, range = range,
    reverse = reverse, round = round)
}
