#' @export
layer_boxplot <- function(vis, ..., stack = TRUE, width = NULL) {
  new_props <- merge_props(cur_props(vis), props(fill := "white"))
  new_props <- merge_props(new_props, props(...))

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

    vis <- layer_f(vis, function(v) {
      v <- add_props(v, .props = new_props)

      # Group by x variable
      # FIXME: The do_call is a workaround for issue #177
      v <- do_call(group_by, quote(v), .args = list(x_var[[2]]))
      v <- compute_boxplot(v, y_var)

      # Add the whiskers using an x scale named "xcenter"
      whisker_props <- props(prop("x", x_var, scale = "xcenter"),
                             y = ~min_, y2 = ~max_, width := 0.5)
      v <- emit_rects(v, merge_props(new_props, whisker_props))


      rect_props <- props(x = x_var, y = ~lower_, y2 = ~upper_, width = band())
      v <- emit_rects(v, merge_props(new_props, rect_props))

      median_props <- props(x = x_var, y = ~median_, height := 1,
                            width = band())
      v <- emit_rects(v, merge_props(new_props, median_props))

      v
    })

    vis <- scale_nominal(vis, "x", padding = 1 - width, points = FALSE)

    # To get whiskers to line up with centers of x, need to use these padding
    # and points settings.
    vis <- scale_nominal(vis, "x", name = "xcenter", padding = 1/(width),
                         points = TRUE)

  } else {
    vis <- layer_f(vis, function(v) {
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
