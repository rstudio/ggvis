#' Create a new layering function.
#'
#' The layer function is run, and then the state before the code was run
#' is restored - this allows layers to be effectively isolated from
#' the rest of the plot.
#'
#' @param vis The ggvis visualisation to modify.
#' @param fun A function that takes a single argument, the current
#'   visualisation as input, and returns a modified visualisation.
#' @keywords internal
#' @export
#' @examples
#' mtcars %>% ggvis(~mpg) %>%
#'   layer_f(function(v) {
#'      v %>% compute_bin(~mpg) %>% layer_points(x = ~x_, y = ~count_)
#'   }) %>%
#'   layer_points(y = ~wt)
layer_f <- function(vis, fun) {
  # Save current data and props
  old_data  <- vis$cur_data
  old_props <- vis$cur_props

  vis <- fun(vis)

  # Restore previous data and props
  vis$cur_data  <- old_data
  vis$cur_props <- old_props

  vis
}
