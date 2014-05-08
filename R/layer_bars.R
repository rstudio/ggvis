#' Display data with bars (a barchart).
#'
#' This will add bars to a plot. The exact behavior is complicated because
#' the term bar chart is used to describe four important variations on a theme.
#' The action of \code{layer_bars} depends on two factors: whether or not a
#' \code{y} prop has been specified, and whether the \code{x} props is
#' continuous or categorical.
#'
#' @section Visualisations:
#'
#' If no y prop has been specified, then this will count the number of entries
#' at each unique x value. There will be one bar at each unique x value, and
#' the y value (or height) of each bar will represent the count at that x value.
#'
#' If a y prop has been specified, then those y values will be used as weights
#' for a weighted count at each unique x value. If no x values appear more than
#' once in the data, then the end result is a plot where the height of the bar
#' at each x value is simply the y value. However, if an x value appear more
#' than once in the data, then this will sum up the y values at each x.
#'
#' If the x variable is continuous, then a continuous x axis will be used, and
#' the width of each bar is equal to the resolution of the data -- that is, the
#' smallest difference between any two x values.
#'
#' If the x variable is categorical, then a categorical x axis will be used. By
#' default, the width of each bar is 0.9 times the space between the items.
#'
#' @param vis Visualisation to modify
#' @param ... Visual properties used to override defaults.
#' @param width Width of each bar (only used when x is categorical).
#' @inheritParams compute_count
#' @seealso \code{\link{layer_histograms}} For bar graphs of counts at each unique
#'   x value, in contrast to a histogram's bins along x ranges.
#' @seealso \code{\link{compute_count}} and \code{\link{compute_tabulate}} for
#'   more information on how data is transformed.
#' @export
#' @examples
#' # Discrete x: bar graph of counts at each x value
#' mtcars %>% ggvis(~factor(cyl)) %>% layer_bars()
#'
#' # Discrete x & continuous y: sum up heights at each count
#' pressure %>% ggvis(~temperature, ~pressure) %>% layer_bars()
#' mtcars %>% ggvis(~factor(cyl), ~disp) %>% layer_bars()
#'
#' # Narrower bars
#' mtcars %>% ggvis(~factor(cyl)) %>% layer_bars(width = band(mult = 0.5))
#'
#' # Continuous x: bar graph of counts at unique locations
#' mtcars %>% ggvis(~cyl) %>% layer_bars()
#' # Notice how it differs from a histogram: a histogram has bins that span
#' # ranges of x, but layer_bars shows the count at each unique x value.
#' mtcars %>% ggvis(~wt) %>% layer_histograms()
#' mtcars %>% ggvis(~wt) %>% layer_bars()
#'
#' # It's possible to use a nominal x scale when the x value is continuous.
#' # Notice how each x value is labeled and ordered by numeric value.
#' pressure %>% ggvis(x = ~temperature, y = ~pressure) %>%
#'   layer_bars() %>%
#'   set_dscale("x", "nominal", padding = 0, points = FALSE)
#'
#' # Y var, and discrete x
#' # FIXME: x values are currently sorted alphabetically, instead of by factor
#' # level order.
#' pressure %>% ggvis(~factor(temperature), ~pressure) %>% layer_bars()
layer_bars <- function(vis, ..., width = band(mult = 0.9)) {
  new_props <- merge_props(cur_props(vis), props(...))

  x_var <- find_prop_var(new_props, "x.update")
  discrete_x <- prop_countable(cur_data(vis), new_props$x.update)

  if (!is.null(new_props$y.update)) {
    if (prop_countable(cur_data(vis), new_props$y.update)) {
      stop("y variable (weights) must be numeric.")
    }
    y_var <- find_prop_var(new_props, "y.update")

  } else {
    y_var <- NULL
  }

  if (discrete_x) {
    vis <- layer_f(vis, function(v) {
      v <- compute_tabulate(v, x_var, y_var)
      v <- layer_rects(v, x = ~x_, y = 0, y2 = ~count_, width = width)
      v
    })
    vis <- set_dscale(vis, "x", "nominal", padding = 0, points = FALSE)

  } else {
    vis <- layer_f(vis, function(v) {
      v <- compute_count(v, x_var, y_var)
      v <- layer_rects(v, x = ~xmin_, x2 = ~xmax_, y = 0, y2 = ~count_)
      v
    })
  }

  vis <- set_dscale(vis, "y", "numeric", domain = c(0, NA))
  vis
}
