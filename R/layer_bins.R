#' Display binned data
#'
#' @param vis Visualisation to modify
#' @param ... Visual properties used to override defaults.
#' @param stack If \code{TRUE}, will automatically stack overlapping bars.
#' @inheritParams compute_bin
#' @seealso \code{\link{layer_bars}} For bar graphs of counts at each unique
#'   x value, in contrast to a histogram's bins along x ranges.
#' @export
#' @examples
#' # Create histograms and frequency polygons with layers
#' mtcars %>% ggvis(~mpg) %>% layer_histograms()
#' mtcars %>% ggvis(~mpg) %>% layer_histograms(binwidth = 2)
#' mtcars %>% ggvis(~mpg) %>% layer_freqpolys(binwidth = 2)
#'
#' # These are equivalent to combining compute_bin with the corresponding
#' # mark
#' mtcars %>% compute_bin(~mpg) %>% ggvis(~x_, ~count_) %>% layer_paths()
#'
#' # With grouping
#' mtcars %>% ggvis(~mpg, fill = ~factor(cyl)) %>% group_by(cyl) %>%
#'   layer_histograms(binwidth = 2)
#' mtcars %>% ggvis(~mpg, stroke = ~factor(cyl)) %>% group_by(cyl) %>%
#'   layer_freqpolys(binwidth = 2)
layer_histograms <- function(vis, ..., binwidth = NULL, origin = NULL,
                            right = TRUE, stack = TRUE) {

  new_props <- merge_props(cur_props(vis), props(...))

  check_unsupported_props(new_props, c("x", "y", "x2", "y2"),
                          c("enter", "exit", "hover"), "layer_histograms")

  x_var <- find_prop_var(new_props, "x.update")
  x_val <- eval_vector(cur_data(vis), x_var)

  vis <- set_scale_label(vis, "x", prop_label(new_props$x.update))
  vis <- scale_numeric(vis, "y", domain = c(0, NA), expand = c(0, 0.05),
                       label = "count")

  layer_f(vis, function(x) {
    x <- compute_bin(x, x_var, binwidth = binwidth, origin = origin,
                     right = right, pad = FALSE)

    if (stack) {
      x <- compute_stack(x, stack_var = ~count_, group_var = ~x_)

      rect_props <- merge_props(
        new_props,
        props(x = ~xmin_, x2 = ~xmax_, y = ~stack_upr_, y2 = ~stack_lwr_)
      )
      x <- emit_rects(x, rect_props)

    } else {
      rect_props <- merge_props(
        new_props,
        props(x = ~xmin_, x2 = ~xmax_, y = ~count_, y2 = 0)
      )
      x <- emit_rects(x, rect_props)
    }
    x
  })
}

#' @rdname layer_histograms
#' @export
layer_freqpolys <- function(vis, ..., binwidth = NULL, origin = NULL,
                            right = TRUE) {
  new_props <- merge_props(cur_props(vis), props(...))

  check_unsupported_props(new_props, c("x", "y"),
                          c("enter", "exit", "hover"), "layer_freqpolys")

  x_var <- find_prop_var(new_props, "x.update")
  x_val <- eval_vector(cur_data(vis), x_var)

  vis <- set_scale_label(vis, "x", prop_label(new_props$x.update))
  vis <- set_scale_label(vis, "y", "count")

  params <- bin_params(range(x_val, na.rm = TRUE), binwidth = value(binwidth),
    origin = value(origin), right = value(right))

  layer_f(vis, function(x) {
    x <- compute_bin(x, x_var, binwidth = params$binwidth,
      origin = params$origin, right = params$right)

    path_props <- merge_props(new_props, props(x = ~x_, y = ~count_))
    x <- emit_paths(x, path_props)
    x
  })
}
