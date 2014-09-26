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
#' the width of each bar is by default equal to the resolution of the data --
#' that is, the smallest difference between any two x values.
#'
#' If the x variable is categorical, then a categorical x axis will be used. By
#' default, the width of each bar is 0.9 times the space between the items.
#'
#' @param vis Visualisation to modify
#' @param ... Visual properties used to override defaults.
#' @param width Width of each bar. When x is continuous, this controls the width
#'   in the same units as x. When x is categorical, this controls the width as a
#'   proportion of the spacing between items (default is 0.9).
#' @param stack If there are multiple bars to be drawn at an x location, should
#'   the bars be stacked? If FALSE, the bars will be overplotted on each other.
#' @inheritParams compute_count
#' @seealso \code{\link{layer_histograms}} For bar graphs of counts at each unique
#'   x value, in contrast to a histogram's bins along x ranges.
#' @seealso \code{\link{compute_count}} and \code{\link{compute_tabulate}} for
#'   more information on how data is transformed.
#' @export
#' @examples
#' # Discrete x: bar graph of counts at each x value
#' cocaine %>% ggvis(~state) %>% layer_bars()
#' # Continuous x: bar graph of counts at unique locations
#' cocaine %>% ggvis(~month) %>% layer_bars()
#'
#' # Use y prop to weight by additional variable. This is also useful
#' # if you have pretabulated data
#' cocaine %>% ggvis(~state, ~weight) %>% layer_bars()
#' cocaine %>% ggvis(~month, ~weight) %>% layer_bars()
#'
#' # For continuous x, layer_bars is useful when the variable has a few
#' # unique values that you want to preserve. If you have many unique
#' # values and you want to bin, use layer_histogram
#' cocaine %>% ggvis(~price) %>% layer_bars()
#' cocaine %>% ggvis(~price) %>% layer_histograms(width = 100)
#'
#' # If you have unique x values, you can use layer_bars() as an alternative
#' # to layer_points()
#' pressure %>% ggvis(~temperature, ~pressure) %>% layer_points()
#' pressure %>% ggvis(~temperature, ~pressure) %>% layer_bars()
#'
#' # When x is continuous, width controls the width in x units
#' pressure %>% ggvis(~temperature, ~pressure) %>% layer_bars(width = 10)
#' # When x is categorical, width is proportional to spacing between bars
#' pressure %>% ggvis(~factor(temperature), ~pressure) %>%
#'   layer_bars(width = 0.5)
#'
#' # Stacked bars
#' # If grouping var is continuous, you need to manually specify grouping
#' ToothGrowth %>% group_by(dose) %>%
#'   ggvis(x = ~supp, y = ~len, fill = ~dose) %>% layer_bars()
#' # If grouping var is categorical, grouping is done automatically
#' cocaine %>% ggvis(x = ~state, fill = ~as.factor(month)) %>%
#'   layer_bars()
layer_bars <- function(vis, ..., stack = TRUE, width = NULL) {
  new_props <- merge_props(cur_props(vis), props(...))

  check_unsupported_props(new_props, c("x", "y", "x2", "y2"),
                          c("enter", "exit", "hover"), "layer_bars")

  x_var <- find_prop_var(new_props, "x.update")
  discrete_x <- prop_countable(cur_data(vis), new_props$x.update)

  vis <- set_scale_label(vis, "x", prop_label(cur_props(vis)$x.update))

  if (!is.null(new_props$y.update)) {
    if (prop_countable(cur_data(vis), new_props$y.update)) {
      stop("y variable (weights) must be numeric.")
    }
    y_var <- find_prop_var(new_props, "y.update")
    vis <- set_scale_label(vis, "y", prop_label(cur_props(vis)$y.update))

  } else {
    y_var <- NULL
    vis <- set_scale_label(vis, "y", "count")
  }

  if (discrete_x) {
    if (is.null(width)) {
      width <- 0.9
    }

    vis <- layer_f(vis, function(v) {
      v <- add_props(v, .props = new_props)
      v <- auto_group(v, exclude = c("x", "y"))
      v <- compute_count(v, x_var, y_var)

      if (stack) {
        v <- compute_stack(v, stack_var = ~count_, group_var = ~x_)
        v <- layer_rects(v, x = ~x_, y = ~stack_lwr_, y2 = ~stack_upr_,
                         width = band())
      } else {
        v <- layer_rects(v, x = ~x_, y = 0, y2 = ~count_, width = band())
      }
      v
    })
    vis <- scale_nominal(vis, "x", padding = 1 - width, points = FALSE)

  } else {
    vis <- layer_f(vis, function(v) {
      v <- add_props(v, .props = new_props)
      v <- compute_count(v, x_var, y_var)
      v <- compute_align(v, ~x_, length = width)
      if (stack) {
        v <- compute_stack(v, stack_var = ~count_, group_var = ~x_)
        v <- layer_rects(v, x = ~xmin_, x2 = ~xmax_, y = ~stack_upr_,
                         y2 = ~stack_lwr_)
      } else {
        v <- layer_rects(v, x = ~xmin_, x2 = ~xmax_, y = 0, y2 = ~count_)
      }
      v
    })
  }

  vis <- scale_numeric(vis, "y", domain = c(0, NA), expand = c(0, 0.05))
  vis
}
