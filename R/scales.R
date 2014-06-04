#' Add a scale to a ggvis plot
#'
#' This creates a scale object for a given scale and variable type, and adds it
#' to a ggvis plot. The scale object is populated with default settings, which
#' depend on the scale (e.g. fill, x, opacity) and the type of variable (e.g.
#' numeric, nominal, ordinal). Any settings that are passed in as arguments
#' will override the defaults.
#'
#' @section Scale selection:
#'
#' ggvis supports the following types of scales. Typical uses for each scale
#' type are listed below:
#' \itemize{
#'   \item numeric For continuous numeric values.
#'   \item nominal For character vectors and factors.
#'   \item ordinal For ordered factors (these presently behave the same as
#'     nominal).
#'   \item logical For logical (TRUE/FALSE) values.
#'   \item datetime For dates and date-times.
#' }
#'
#' Each type has a a corresponding function: \code{scale_numeric},
#' \code{scale_nominal}, and so on.
#'
#' The scale types for ggvis are mapped to scale types for Vega, which include
#' "ordinal", "quantitative", and "time". See \code{\link{vega_scale}} for more
#' details.
#'
#' Given a scale and type, the range is selected based on the combination of the
#' \code{scale} and \code{type}. For example, you get a different range of
#' colours depending on whether the data is numeric, ordinal, or nominal. Some
#' scales also set other properties. For example, nominal/ordinal position
#' scales also add some padding so that points are spaced away from plot edges.
#'
#' Not all combinations have an existing default scale. If you use a
#' combination that does not have an existing combination, it may suggest
#' you're displaying the data in a suboptimal way. For example, there is
#' no default for a numeric shape scale, because there's no obvious way to
#' map continuous values to discrete shapes.
#'
#' @param vis A ggvis object.
#' @param scale The name of a scale, such as "x", "y", "fill", "stroke", etc.
#' @param type A variable type. One of "numeric", "nominal", "ordinal",
#'   "logical", "datetime".
#' @param ... other arguments passed to the scale function. See the help for
#'   \code{\link{scale_numeric}}, \code{\link{scale_ordinal}} and
#'   \code{\link{scale_datetime}} for more details. For example, you might
#'   supply \code{trans = "log"} to create a log scale.
#' @param name If \code{NULL}, the default, the scale name is the same as
#'   \code{scale}. Set this to a custom name to create multiple scales for
#'   stroke or fill, or (god forbid) a secondary y scale.
#' @examples
#' p <- mtcars %>%
#'   ggvis(x = ~wt, y = ~mpg, fill = ~factor(cyl), stroke = ~hp) %>%
#'   layer_points()
#'
#' p %>% scale_numeric("x")
#' p %>% scale_numeric("stroke")
#' p %>% scale_nominal("stroke")
#'
#' # You can also supply additional arguments or override the defaults
#' p %>% scale_numeric("x", trans = "log")
#' p %>% scale_nominal("stroke", range = c("red", "blue"))
#' @name scales
#' @aliases set_default_scale set_dscale
NULL

#' Add a numeric scale to a ggvis object.
#'
#' A numeric (quantitative) scale controls the mapping of continuous variables
#' to visual properties.
#'
#' @param vis A ggvis object.
#' @param scale The name of a scale, such as "x", "y", "fill", "stroke", etc.
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
#' @seealso \code{\link{scales}}, \code{\link{scale_ordinal}},
#'   \url{https://github.com/trifacta/vega/wiki/Scales#quantitative-scale-properties}
#' @family scales
#' @export
#' @examples
#' p <- mtcars %>% ggvis(~wt, ~mpg, fill = ~hp) %>% layer_points()
#'
#' p %>% scale_numeric("y")
#'
#' p %>% scale_numeric("y", trans = "pow", exp = 0.5)
#'
#' p %>% scale_numeric("y", trans = "log")
#'
#' # Can control other properties other than x and y
#' p %>% scale_numeric("fill", domain = c(0, 120), clamp = TRUE)
#'
#' # Set range of data from 0 to 3
#' p %>% scale_numeric("x", domain = c(0, 3), clamp = TRUE, nice = FALSE)
#'
#' # Lower bound is set to lower limit of data, upper bound set to 3.
#' p %>% scale_numeric("x", domain = c(NA, 3), clamp = TRUE, nice = FALSE)
scale_numeric <- function(vis, scale, domain = NULL, range = NULL,
                          reverse = FALSE, round = FALSE,
                          trans = "linear", clamp = FALSE, exponent = NULL,
                          nice = TRUE, zero = FALSE, name = NULL) {
  trans <- match.arg(
    trans,
    c("linear", "log", "pow", "sqrt", "quantile", "quantize", "threshold")
  )
  if (trans != "pow" && !is.null(exponent)) {
    stop("May only set exponent when pow = 'trans'", call. = FALSE)
  }

  assert_that(
    is.null(exponent) ||
    (is.numeric(exponent) && length(exponent) == 1)
  )
  assert_that(is.flag(clamp), is.flag(nice), is.flag(zero))

  if (is.null(range)) {
    range <- switch(scale,
      x = "width",
      y = "height",
      stroke = c("#132B43", "#56B1F7"),
      fill = c("#132B43", "#56B1F7"),
      size = c(20, 100),
      fontSize = c(10, 20),
      opacity = c(0, 1),
      angle = c(0, 2 * pi),
      radius = c(0, 50),
      stop("Don't know how to automatically set range for ", scale, ".")
    )
  }

  vscale <- vega_scale(
    name = name %||% scale,
    type = trans,
    subclass = "quantitative",
    exponent = exponent,
    clamp = clamp,
    nice = nice,
    zero = zero,
    domain = domain,
    range = range,
    reverse = reverse,
    round = round
  )

  add_scale(vis, vscale)
}

#' Add a date-time scale to a ggvis object.
#'
#' A date/time scale controls the mapping of date and time variables to
#' visual properties.
#'
#' @param vis A ggvis object.
#' @param scale The name of a scale, such as "x", "y", "fill", "stroke", etc.
#' @inheritParams vega_scale
#' @param clamp  If true, values that exceed the data domain are clamped to
#'   either the minimum or maximum range value.
#' @param nice If specified, modifies the scale domain to use a more
#'   human-friendly value range. Should be a string indicating the desired time
#'   interval; legal values are "second", "minute", "hour", "day", "week",
#'   "month", or "year"
#' @param utc if \code{TRUE}, uses UTC times.
#' @seealso \code{\link{scales}}, \code{\link{scale_numeric}},
#'   \url{https://github.com/trifacta/vega/wiki/Scales#time-scale-properties}
#' @family scales
#' @export
#' @examples
#' set.seed(2934)
#' dat <- data.frame(
#'   time = as.Date("2013-07-01") + 1:100,
#'   value = seq(1, 10, length.out = 100) + rnorm(100)
#' )
#' p <- dat %>% ggvis(~time, ~value) %>% layer_points()
#'
#' # Start and end on month boundaries
#' p %>% scale_datetime("x", nice = "month")
#'
#'
#' dist <- data.frame(times = as.POSIXct("2013-07-01", tz = "GMT") +
#'                            rnorm(200) * 60 * 60 * 24 * 7)
#' p <- dist %>% ggvis(x = ~times) %>% layer_histograms()
#' p
#'
#' # Start and end on month boundaries
#' p %>% scale_datetime("x", nice = "month")
#'
#' p %>% scale_datetime("x", utc = TRUE)
scale_datetime <- function(vis, scale, domain = NULL, range = NULL,
                           reverse = FALSE, round = FALSE, utc = FALSE,
                           clamp = FALSE, nice = NULL, name = NULL) {
  assert_that(is.flag(clamp))
  if (!is.null(nice)) {
    nice <- match.arg(
      nice,
      c("second", "minute", "hour", "day", "week","month", "year")
    )
  }

  if (is.null(range)) {
    range <- switch(scale,
      x = "width",
      y = "height",
      stop("Don't know how to automatically set range for ", scale, ".")
    )
  }

  vscale <- vega_scale(
    name = name %||% scale,
    type = if (utc) "utc" else "time",
    subclass = "time",
    clamp = clamp,
    nice = nice,
    domain = domain,
    range = range,
    reverse = reverse,
    round = round
  )

  add_scale(vis, vscale)
}

#' Add a ordinal, nominal, or logical scale to a ggvis object.
#'
#' Ordinal, nominal, and logical scales are all categorical, and are treated
#' similarly by ggvis.
#'
#' @param vis A ggvis object.
#' @param scale The name of a scale, such as "x", "y", "fill", "stroke", etc.
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
#' @seealso \code{\link{scales}}, \code{\link{scale_numeric}},
#'   \url{https://github.com/trifacta/vega/wiki/Scales#ordinal-scale-properties},
#'   \url{https://github.com/mbostock/d3/wiki/Ordinal-Scales}
#' @family scales
#' @export
#' @examples
#' p <- PlantGrowth %>% ggvis(~group, ~weight) %>% layer_points()
#'
#' p
#' p %>% scale_nominal("x", padding = 0)
#' p %>% scale_nominal("x", padding = 1)
#'
#' p %>% scale_nominal("x", reverse = TRUE)
#'
#' p <- ToothGrowth %>% group_by(supp) %>%
#'   ggvis(~len, fill = ~supp) %>%
#'   layer_histograms(width = 4, stack = TRUE)
#'
#' # Control range of fill scale
#' p %>% scale_nominal("fill", range = c("pink", "lightblue"))
scale_ordinal <- function(vis, scale, domain = NULL, range = NULL,
                          reverse = FALSE, round = FALSE,
                          points = TRUE, padding = NULL, sort = FALSE,
                          name = NULL) {
  assert_that(is.flag(points))
  assert_that(is.null(padding) || (is.numeric(padding) && length(padding) == 1))
  assert_that(is.flag(sort))

  if (is.null(range)) {
    range <- switch(scale,
      x = "width",
      y = "height",
      stroke = "category10",
      fill = "category10",
      size = c(10, 100),
      stop("Don't know how to automatically set range for ", scale, ".")
    )
  }

  if (is.null(padding)) {
    padding <- switch(scale,
      x = 0.5,
      y = 0.5,
      NULL
    )
  }

  vscale <- vega_scale(
    name = name %||% scale,
    type = "ordinal",
    points = points,
    padding = padding,
    sort = sort,
    subclass = "ordinal",
    domain = domain,
    range = range,
    reverse = reverse,
    round = round
  )

  add_scale(vis, vscale)
}

#' @rdname scale_ordinal
#' @export
scale_nominal <- function(vis, scale, domain = NULL, range = NULL,
                          reverse = FALSE, round = FALSE,
                          points = TRUE, padding = NULL, sort = FALSE,
                          name = NULL) {
  if (is.null(range)) {
    range <- switch(scale,
      x = "width",
      y = "height",
      stroke = "category10",
      fill = "category10",
      shape = "shapes",
      stop("Don't know how to automatically set range for ", scale, ".")
    )
  }

  scale_ordinal(vis, scale, domain, range, reverse, round, points, padding,
                sort, name)
}

#' @rdname scale_ordinal
#' @export
scale_logical <- scale_nominal


# Given a ggvis object, add all needed vega scales, with correct domain
# values set.
add_missing_scales <- function(vis, quiet = TRUE) {

  # Add in scales not already specified in spec
  needed <- setdiff(names(vis$scale_info), names(vis$scales))
  for (scale_n in needed) {
    vis <- scale_auto(vis, scale_n, quiet = quiet)
  }

  # Add special x_rel and y_rel scales. Do it directly instead of using
  # scale_quantitative function, because we need data_domain=FALSE.
  x_rel <- vega_scale(name = "x_rel", type = "linear", subclass = "quantitative",
                      domain = c(0, 1), range = "width")
  y_rel <- vega_scale(name = "y_rel", type = "linear", subclass = "quantitative",
                      domain = c(0, 1), range = "height")
  vis <- add_scale(vis, x_rel, data_domain = FALSE)
  vis <- add_scale(vis, y_rel, data_domain = FALSE)

  vis
}

scale_auto <- function(vis, scale, ..., quiet = FALSE) {
  info <- vis$scale_info[[scale]]

  if (!quiet) {
    message("Adding scale_", info$type, "(\"", scale, "\")")
  }
  scale_fun <- match.fun(paste0("scale_", info$type))
  vis <- scale_fun(vis, scale, ...)
}

