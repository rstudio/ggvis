#' Display binned data
#'
#' @param vis Visualisation to modify
#' @param ... Visual properties used to override defaults.
#' @inheritParams compute_bin
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
layer_histograms <- function(vis, ..., binwidth = NULL, origin = NULL,
                            right = TRUE) {

  rect_props <- merge_props(props(x = ~xmin_, x2 = ~xmax_, y = ~count_, y2 = 0),
    props(...))

  x_var <- find_prop_var(vis$cur_props, "x.update")
  x_val <- eval_vector(cur_data(vis), x_var)
  params <- bin_params(range(x_val, na.rm = TRUE), binwidth = binwidth,
    origin = origin, right = right)

  layer_f(vis, function(x) {
    x <- compute_bin(x, x_var, binwidth = params$binwidth,
      origin = params$origin, right = params$right)
    x <- emit_rects(x, rect_props)
    x
  })
}

#' @rdname layer_histograms
#' @export
layer_freqpolys <- function(vis, ..., binwidth = NULL, origin = NULL,
                            right = TRUE) {
  path_props <- merge_props(props(x = ~x_, y = ~count_), props(...))

  x_var <- find_prop_var(vis$cur_props, "x.update")
  x_val <- eval_vector(cur_data(vis), x_var)
  params <- bin_params(range(x_val, na.rm = TRUE), binwidth = value(binwidth),
    origin = value(origin), right = value(right))

  layer_f(vis, function(x) {
    x <- compute_bin(x, x_var, binwidth = params$binwidth,
      origin = params$origin, right = params$right)
    x <- emit_paths(x, path_props)
    x
  })
}

#' @rdname layer_histograms
#' @export
layer_bars <- function(vis, ..., right = TRUE) {
  rect_props <- merge_props(props(x = ~x, width = band(mult = 0.9),
    y2 = ~count__, y = 0), props(...))

  x_var <- find_prop_var(vis$cur_props, "x.update")

  layer_f(vis, function(x) {
    x <- compute_bin(x, x_var, binwidth = 1,
      origin = 0.5, right = right)
    x <- emit_rects(x, rect_props)
    x
  })
}
