#' Create an axis_props object for controlling axis properties.
#'
#' The items in each of the lists can be a literal value, like \code{5} or
#' "blue", or they can be a \code{\link{scaled_value}} object.
#'
#' @param axis A named list of line properties for the axis line.
#' @param ticks A named list of line properties for ticks.
#' @param majorTicks A named list of line properties for major ticks.
#' @param minorTicks A named list of line properties for minor ticks.
#' @param grid A named list of line properties for grid lines.
#' @param labels A named list of text properties for axis labels.
#' @param title A named list of text properties for the axis title.
#'
#' @export
axis_props <- function(ticks = NULL, majorTicks = NULL, minorTicks = NULL,
                       grid = NULL, labels = NULL, title = NULL, axis = NULL) {

  args <- list(ticks = ticks, majorTicks = majorTicks, minorTicks = minorTicks,
               grid = grid, labels = labels, title = title, axis = axis)

  # Validate properties
  prop_names <- lapply(args, function(arg) names(arg))
  types <- c("line", "line", "line", "line", "text", "text", "line")
  Map(check_mark_props, types, prop_names)

  structure(
    compact(args),
    class = "axis_props"
  )
}

#' Tests whether an object is an axis_props object
#' @param x an object to test
#' @export
#' @keywords internal
is.axis_props <- function(x) inherits(x, "axis_props")

#' @export
as.vega.axis_props <- function(x) {
  as_value <- function(item) {
    lapply(item, function(val) {
      if (is.scaled_value(val)) {
        val
      } else {
        list(value = val)
      }
    })
  }

  lapply(x, as_value)
}

#' Create an axis_props object for controlling legend properties.
#'
#' The items in each of the lists can be a literal value, like \code{5} or
#' "blue", or they can be a \code{\link{scaled_value}} object.
#'
#' @param title A named list of text properties for the legend title.
#' @param labels A named list of text properties for legend labels.
#' @param symbols A named list of line properties for symbols (for discrete
#'   legend items).
#' @param gradient A named list of line properties a continuous color gradient.
#' @param legend A named list of line properties for the overall legend. The
#'   x and y position can be set here, which will override automatic
#'   positioning.
#'
#' @export
legend_props <- function(title = NULL, labels = NULL,
                         symbols = NULL, gradient = NULL, legend = NULL) {

  args <- list(title = title, labels = labels, symbols = symbols,
               gradient = gradient, legend = legend)

  # Validate properties
  prop_names <- lapply(args, function(arg) names(arg))
  types <- c("text", "text", "symbol", "rect", "rect")
  Map(check_mark_props, types, prop_names)

  structure(
    compact(args),
    class = "legend_props"
  )
}

#' Tests whether an object is a legend_props object
#' @param x an object to test
#' @export
#' @keywords internal
is.legend_props <- function(x) inherits(x, "legend_props")

#' @export
as.vega.legend_props <- as.vega.axis_props


#' Create a scaled_value object
#'
#' These are for use with legends and axes.
#' @param scale The name of a scale, e.g., "x", "fill".
#' @param value A value which will be transformed using the scale.
#'
#' @export
scaled_value <- function(scale, value) {
  structure(list(scale = scale, value = value), class = "scaled_value")
}

#' Tests whether an object is a scaled_value object
#' @param x an object to test
#' @export
#' @keywords internal
is.scaled_value <- function(x) inherits(x, "scaled_value")

#' @export
as.vega.scaled_value <- function(x) x
