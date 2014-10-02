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
#' Each type has a corresponding function: \code{scale_numeric},
#' \code{scale_nominal}, and so on.
#'
#' The scale types for ggvis are mapped to scale types for Vega, which include
#' "ordinal", "quantitative", and "time". See \code{\link{ggvis_scale}} for more
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
#' p %>% scale_nominal("fill")
#'
#' # You can also supply additional arguments or override the defaults
#' p %>% scale_numeric("x", trans = "log")
#' p %>% scale_numeric("stroke", range = c("red", "blue"))
#' @name scales
#' @aliases set_default_scale set_dscale
NULL

#' Add a numeric scale to a ggvis object.
#'
#' A numeric (quantitative) scale controls the mapping of continuous variables
#' to visual properties.
#'
#' The default values for most of the arguments is NULL. When the plot is
#' created, these NULL values will be replaced with default values, as indicated
#' below.
#'
#' @param vis A ggvis object.
#' @param property The name of a visual property, such as "x", "y", "fill",
#'   "stroke". Note both x and x2 use the "x" scale (similarly for y and y2).
#'   fillOpacity, opacity and strokeOpacity use the "opacity" scale.
#' @inheritParams ggvis_scale
#' @param trans A scale transformation: one of "linear", "log", "pow", "sqrt",
#'   "quantile", "quantize", "threshold". Default is "linear".
#' @param exponent Sets the exponent of the scale transformation. For pow
#'   transform only.
#' @param clamp  If \code{TRUE}, values that exceed the data domain are clamped
#'   to either the minimum or maximum range value. Default is \code{FALSE}.
#' @param nice If \code{TRUE}, modifies the scale domain to use a more
#'   human-friendly number range (e.g., 7 instead of 6.96). Default is
#'   \code{FALSE}.
#' @param zero If \code{TRUE}, ensures that a zero baseline value is included
#'   in the scale domain. This option is ignored for non-quantitative scales.
#'   Default is \code{FALSE}.
#' @param expand A multiplier for how much the scale should be expanded beyond
#'   the domain of the data. For example, if the data goes from 10 to 110, and
#'   \code{expand} is 0.05, then the resulting domain of the scale is 5 to 115.
#'   Set to 0 and use \code{nice=FALSE} if you want exact control over the
#'   domain. If left \code{NULL}, behavior will depend on the scale type. For
#'   positional scales (x and y), \code{expand} will default to 0.05. For other
#'   scales, it will default to 0.
#' @seealso \code{\link{scales}}, \code{\link{scale_ordinal}},
#'   \url{https://github.com/trifacta/vega/wiki/Scales#quantitative-scale-properties}
#' @family scales
#' @export
#' @examples
#' p <- mtcars %>% ggvis(~wt, ~mpg, fill = ~hp) %>% layer_points()
#'
#' p %>% scale_numeric("y")
#'
#' p %>% scale_numeric("y", trans = "pow", exponent = 0.5)
#'
#' p %>% scale_numeric("y", trans = "log")
#'
#' # Can control other properties other than x and y
#' p %>% scale_numeric("fill", domain = c(0, 120), clamp = TRUE)
#'
#' # Set range of data from 0 to 3
#' p %>% scale_numeric("x", domain = c(0, 3), clamp = TRUE, expand = 0,
#'                      nice = FALSE)
#'
#' # Lower bound is set to lower limit of data, upper bound set to 3.
#' p %>% scale_numeric("x", domain = c(NA, 3), clamp = TRUE, nice = FALSE)
scale_numeric <- function(vis, property, domain = NULL, range = NULL,
                          reverse = NULL, round = NULL,
                          trans = NULL, clamp = NULL, exponent = NULL,
                          nice = NULL, zero = NULL, expand = NULL,
                          name = property, label = NULL, override = NULL) {
  assert_that(is.null(trans) || trans %in% c("linear", "log", "pow", "sqrt",
    "quantile", "quantize", "threshold"))
  if (!is.null(exponent) && !identical(trans, "pow")) {
    stop("May only set exponent when pow = 'trans'")
  }
  assert_that(is.null(exponent) || (is.numeric(exponent) && length(exponent) == 1))
  assert_that(is.null(reverse) || is.flag(reverse))
  assert_that(is.null(round) || is.flag(round))
  assert_that(is.null(clamp) || is.flag(clamp))
  assert_that(is.null(nice) || is.flag(nice))
  assert_that(is.null(zero) || is.flag(zero))
  assert_that(is.null(expand) || (is.numeric(expand) && length(expand) <= 2))

  vscale <- ggvis_scale(
    property = property,
    name = name,
    label = label,
    type = trans,
    subclass = "numeric",
    exponent = exponent,
    clamp = clamp,
    nice = nice,
    zero = zero,
    domain = domain,
    range = range,
    reverse = reverse,
    round = round,
    expand = expand,
    override = override
  )
  add_scale(vis, vscale)
}

#' Add a date-time scale to a ggvis object.
#'
#' A date/time scale controls the mapping of date and time variables to
#' visual properties.
#'
#' @param vis A ggvis object.
#' @param property The name of a property, such as "x", "y", "fill", "stroke", etc.
#' @inheritParams ggvis_scale
#' @param clamp  If \code{TRUE}, values that exceed the data domain are clamped
#'   to either the minimum or maximum range value. Default is \code{FALSE}.
#' @param nice If specified, modifies the scale domain to use a more
#'   human-friendly value range. Should be a string indicating the desired time
#'   interval; legal values are "second", "minute", "hour", "day", "week",
#'   "month", or "year".
#' @param expand A multiplier for how much the scale should be expanded beyond
#'   the domain of the data. For example, if the data goes from 10 to 110, and
#'   \code{expand} is 0.05, then the resulting domain of the scale is 5 to 115.
#'   Set to 0 and use \code{nice=FALSE} if you want exact control over the
#'   domain.
#' @param utc if \code{TRUE}, uses UTC times. Default is \code{FALSE}.
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
scale_datetime <- function(vis, property, domain = NULL, range = NULL,
                           reverse = NULL, round = NULL, utc = NULL,
                           clamp = NULL, nice = NULL, expand = NULL,
                           name = property, label = NULL, override = NULL) {
  assert_that(is.null(reverse) || is.flag(reverse))
  assert_that(is.null(round) || is.flag(round))
  assert_that(is.null(utc) || is.flag(utc))
  assert_that(is.null(clamp) || is.flag(clamp))
  assert_that(is.null(nice) || nice %in% c("second", "minute", "hour", "day",
    "week", "month", "year"))
  assert_that(is.null(expand) || (is.numeric(expand) && length(expand) <= 2))

  if (!is.null(utc)) {
    utc <- if (isTRUE(utc)) "utc" else "time"
  }

  vscale <- ggvis_scale(
    property = property,
    name = name,
    label = label,
    type = utc,
    subclass = "datetime",
    clamp = clamp,
    nice = nice,
    domain = domain,
    range = range,
    reverse = reverse,
    round = round,
    expand = expand,
    override = override
  )
  add_scale(vis, vscale)
}

#' Add a ordinal, nominal, or logical scale to a ggvis object.
#'
#' Ordinal, nominal, and logical scales are all categorical, and are treated
#' similarly by ggvis.
#'
#' @param vis A ggvis object.
#' @param property The name of a property, such as "x", "y", "fill", "stroke", etc.
#' @inheritParams ggvis_scale
#' @param points If \code{TRUE} (default), distributes the ordinal values over a
#'   quantitative range at uniformly spaced points. The spacing of the points
#'   can be adjusted using the padding property. If \code{FALSE}, the ordinal
#'   scale will construct evenly-spaced bands, rather than points. Note that
#'   if any mark is added with a \code{\link{band}()} prop, then the scale for
#'   that prop will automatically have \code{points} set to \code{FALSE}.
#' @param padding Applies spacing among ordinal elements in the scale range.
#'   The actual effect depends on how the scale is configured. If the points
#'   parameter is true, the padding value is interpreted as a multiple of the
#'   spacing between points. A reasonable value is 1.0, such that the first and
#'   last point will be offset from the minimum and maximum value by half the
#'   distance between points. Otherwise, padding is typically in the range
#'   [0, 1] and corresponds to the fraction of space in the range interval to
#'   allocate to padding. A value of 0.5 means that the range band width will
#'   be equal to the padding width. For positional (x and y) scales, the default
#'   padding is 0.1. For other scales, the default padding is 0.5.
#' @param sort  If \code{TRUE}, the values in the scale domain will be sorted
#'   according to their natural order. Default is \code{FALSE}.
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
#'
#' # There's no default range when the data is categorical but the output range
#' # is continuous, as in the case of opacity. In these cases, you can
#' # manually specify the range for the scale.
#' mtcars %>% ggvis(x = ~wt, y = ~mpg, opacity = ~factor(cyl)) %>%
#'   layer_points() %>%
#'   scale_nominal("opacity", range = c(0.2, 1))
scale_ordinal <- function(vis, property, domain = NULL, range = NULL,
                          reverse = NULL, round = NULL,
                          points = NULL, padding = NULL, sort = NULL,
                          name = property, label = NULL, override = NULL) {
  assert_that(is.null(reverse) || is.flag(reverse))
  assert_that(is.null(round) || is.flag(round))
  assert_that(is.null(points) || is.flag(points))
  assert_that(is.null(padding) || (is.numeric(padding) && length(padding) == 1))
  assert_that(is.null(sort) || is.flag(sort))

  vscale <- ggvis_scale(
    property = property,
    name = name,
    label = label,
    type = "ordinal",
    points = points,
    padding = padding,
    sort = sort,
    subclass = "ordinal",
    domain = domain,
    range = range,
    reverse = reverse,
    round = round,
    override = override
  )
  add_scale(vis, vscale)
}

#' @rdname scale_ordinal
#' @export
scale_nominal <- function(vis, property, domain = NULL, range = NULL,
                          reverse = NULL, round = NULL,
                          points = NULL, padding = NULL, sort = NULL,
                          name = property, label = NULL, override = NULL) {
  assert_that(is.null(reverse) || is.flag(reverse))
  assert_that(is.null(round) || is.flag(round))
  assert_that(is.null(points) || is.flag(points))
  assert_that(is.null(padding) || (is.numeric(padding) && length(padding) == 1))
  assert_that(is.null(sort) || is.flag(sort))

  vscale <- ggvis_scale(
    property = property,
    name = name,
    label = label,
    type = "ordinal",
    points = points,
    padding = padding,
    sort = sort,
    subclass = "nominal",
    domain = domain,
    range = range,
    reverse = reverse,
    round = round,
    override = override
  )
  add_scale(vis, vscale)
}

#' @rdname scale_ordinal
#' @export
scale_logical <- scale_nominal


#' Add x_rel and y_rel scales
#'
#' This function adds scales named \code{x_rel} and \code{y_rel}, each of which
#' has a domain of 0 to 1, and the range is the plot's width or height.
#' These scales are useful for positioning visual elements relative to the
#' plotting area. For example, with legends.
#'
#' @seealso \code{\link{add_legend}} for a usage example.
#'
#' @param vis A ggvis object.
#' @export
add_relative_scales <- function(vis) {
  vis <- scale_numeric(vis, "x", name = "x_rel", domain = c(0, 1),
                       range = "width", expand = 0)
  vis <- scale_numeric(vis, "y", name = "y_rel", domain = c(0, 1),
                       range = "height", expand = 0)
  vis
}

scale_auto <- function(vis, scale, ..., quiet = FALSE) {
  info <- vis$scales[[scale]]

  if (!quiet) {
    message("Adding scale_", info$type, "(\"", scale, "\")")
  }
  scale_fun <- match.fun(paste0("scale_", info$type))
  vis <- scale_fun(vis, scale, ...)
}

#' Set the label for a scale
#' @param vis A ggvis object.
#' @param scale The name of a scale, like "x".
#' @param label Text to use for the label.
set_scale_label <- function(vis, scale, label) {
  add_scale(vis, ggvis_scale(scale, label = label))
}

# Is this scale object countable?
scale_countable <- function(scale) UseMethod("scale_countable")
#' @export
scale_countable.scale_numeric <- function(scale) FALSE
#' @export
scale_countable.scale_datetime <- function(scale) FALSE
#' @export
scale_countable.scale_nominal <- function(scale) TRUE
#' @export
scale_countable.scale_ordinal <- function(scale) TRUE
#' @export
scale_countable.scale_logical <- function(scale) TRUE
#' @export
scale_countable.default <- function(scale) NULL


# Make sure that scales are well-formed for a vega spec
check_scales_complete <- function(vis) {
  check_scale_complete <- function(scale) {
    if (is.null(scale$range)) {
      warning(paste(
        sprintf("Scale '%s' for property '%s' is missing a range.", scale$name, scale$property),
        "ggvis tries to automatically provide ranges for scales, but it doesn't know how in this case.",
        "You must specify the range manually.",
        "See ?scale_nominal or ?scale_numeric for more information and examples."
      ), call. = FALSE)
    }
  }

  lapply(vis$scales, check_scale_complete)
}
