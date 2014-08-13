#' Display data with a boxplot.
#'
#' This will add boxplots to a plot. The action of \code{layer_boxplot} depends
#' on whether the \code{x} prop is continuous or categorical.
#'
#' @param vis Visualisation to modify
#' @param ... Visual properties used to override defaults.
#' @param width Width of each bar. When x is continuous, this controls the width
#'   in the same units as x. When x is categorical, this controls the width as a
#'   proportion of the spacing between items (default is 0.9).
#' @inheritParams compute_boxplot
#' @seealso \code{\link{compute_boxplot}} for more information on how data is
#'   transformed.
#' @export
#' @examples
#' library(dplyr)
#'
#' # Make data set with categorical x
#' mtc <- mtcars
#' mtc$cyl <- factor(mtc$cyl)
#'
#' mtc %>% ggvis(~cyl, ~mpg) %>% layer_boxplot()
#' # Set the width of the boxes to half the space between tick marks
#' mtc %>% ggvis(~cyl, ~mpg) %>% layer_boxplot(width = 0.5)
#'
#' # Continuous x: boxes fill width between data values
#' mtcars %>% ggvis(~cyl, ~mpg) %>% layer_boxplot()
#' # Setting width=0.5 makes it 0.5 wide in the data space, which is 1/4 of the
#' # distance between data values in this particular case.
#' mtcars %>% ggvis(~cyl, ~mpg) %>% layer_boxplot(width = 0.5)
#' @export
layer_boxplot <- function(vis, ..., width = NULL) {
  new_props <- merge_props(cur_props(vis), props(fill := "white"))
  new_props <- merge_props(new_props, props(...))

  new_outlier_props <- merge_props(cur_props(vis), props(fill := "black"))

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
  }

  if (discrete_x) {
    if (is.null(width)) {
      width <- 0.9
    }
    # Whiskers and outliers are on a centered x scale
    whisker_props <- props(prop("x", x_var, scale = "xcenter"),
                           y = ~min_, y2 = ~max_, width := 0.5)
    outlier_props <- props(prop("x", x_var, scale = "xcenter"), y = ~value_)

    rect_props <- props(x = x_var, y = ~lower_, y2 = ~upper_, width = band())
    median_props <- props(x = x_var, y = ~median_, height := 1, width = band())

    vis <- scale_nominal(vis, "x", padding = 1 - width, points = FALSE)

    # To get whiskers to line up with centers of x, need to use these padding
    # and points settings.
    vis <- scale_nominal(vis, "x", name = "xcenter", padding = 1 + (1 - width),
                         points = TRUE)
  } else {
    whisker_props <- props(x = x_var, y = ~min_, y2 = ~max_, width := 0.5)
    rect_props <- props(x = ~xmin_, x2 = ~xmax_, y = ~lower_, y2 = ~upper_)
    median_props <- props(x = ~xmin_, x2 = ~xmax_, y = ~median_, height := 1)
    outlier_props <- props(x = x_var, y = ~value_)
  }

  vis <- layer_f(vis, function(v) {
    v <- add_props(v, .props = new_props)

    # Group by x variable
    # FIXME: The do_call is a workaround for issue #177
    v <- do_call(group_by, quote(v), .args = list(x_var[[2]]))
    v <- compute_boxplot(v, y_var)
    if (!discrete_x) {
      v <- compute_align(v, x_var, length = width)
    }

    v <- emit_rects(v, merge_props(new_props, whisker_props))
    v <- emit_rects(v, merge_props(new_props, rect_props))
    # Median line
    v <- emit_rects(v, merge_props(new_props, median_props))

    # Outlier points need their own data set
    v <- compute_boxplot_outliers(v)
    v <- emit_points(v, merge_props(new_outlier_props, outlier_props))

    v
  })

  vis
}
