#' Guess the right type of layer based on current properties.
#'
#' \code{layer_guess} provides the magic behind the default behaviour of
#' \code{\link{qvis}}.
#'
#' @section Defaults:
#'
#' \itemize{
#'   \item Continuous x, \code{\link{layer_histogram}}
#'   \item Categorical x, \code{\link{layer_barchart}}
#'   \item Continuous x and y, \code{\link{layer_points}}
#' }
#'
#' @param vis The visualisation to add the new layer to.
#' @export
#' @examples
#' # A scatterplot:
#' mtcars %>% qvis(~mpg, ~wt)
#' mtcars %>% ggvis(~mpg, ~wt) %>% layer_guess()
#'
#' # A histogram:
#' mtcars %>% qvis(~mpg)
#' mtcars %>% ggvis(~mpg) %>% layer_guess()
layer_guess <- function(vis, ...) {
  props <- vis$cur_props
  data <- isolate(vis$cur_data())

  if ("y.update" %in% names(props)) {
    layer_point(vis, ...)
  } else {
    if (prop_countable(data, props$x)) {
      vis <- set_dscale(vis, "x", "nominal", range = "width", padding = 0,
        points = FALSE)
      layer_barchart(vis, ...)
    } else {
      layer_histogram(vis, ...)
    }
  }
}
