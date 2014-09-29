#' Create a subvisualisation.
#'
#' @keywords internal
#' @examples
#' # Examples don't work yet
#' \dontrun{
#' library(dplyr, warn.conflicts = FALSE)
#'
#' small <- nasaweather::atmos %>%
#'   filter(lat <= -11.217391, long <= -106.287, year == 1995) %>%
#'   group_by(long, lat)
#' small %>%
#'   ggvis(~long, ~lat) %>%
#'   layer_points()
#'
#' small %>%
#'   ggvis(~long, ~lat) %>%
#'   subvis(width := 100, height := 100, stroke := "red") %>%
#'     layer_points(~month, ~ozone)
#'
#' small %>%
#'   ggvis(~long, ~lat) %>%
#'   subvis(width := 100, height := 100, stroke := "red") %>%
#'     layer_points(~month, ~ozone) %>%
#'     add_axis("x", ticks = 3) %>%
#'     add_axis("y", ticks = 3)
#' }
subvis <- function(vis, ..., data = NULL, width = NULL, height = NULL) {
  # Very similar to add_mark, but changes to cur_data and cur_props are
  # persistent, we use the special mark_group() and update cur_vis
  vis <- add_data(vis, data, deparse2(substitute(data)))
  vis <- add_props(vis, .props = props(...))
  vis <- register_scales_from_props(vis, cur_props(vis))

  new_mark <- mark_group(vis$cur_props, vis$cur_data)
  vis$marks <- append(vis$marks, list(new_mark))

  # Parent properties control apperance mark_group, and are not
  # inherited by child
  vis$cur_props <- NULL
  vis$cur_vis <- c(vis$cur_vis, length(vis$marks))
  vis

  vis
}
