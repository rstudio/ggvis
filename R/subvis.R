#' Create a subvisualisation.
#'
#' A subvis is a recursive mark: a mark that contain other marks. Compared
#' to other marks, it can also contain scales, axes and legends.
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' small <- nasaweather::atmos %>%
#'   filter(lat <= -11.217391, long <= -106.287, year == 1995) %>%
#'   group_by(long, lat)
#' small %>%
#'   ggvis(~long, ~lat) %>%
#'   layer_points()

#' small %>%
#'   ggvis(~long, ~lat) %>%
#'   subvis(width := 20, height := 20, stroke := "red") %>%
#'     layer_points(
#'       prop("x", ~month, scale = "x2"),
#'       prop("y", ~ozone, scale = "y2"))
subvis <- function(vis, ..., data = NULL) {
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

#   old <- set_prefix("xyz-")
#   on.exit(set_prefix(old), add = TRUE)

#   my_data <- vis$data[[length(vis$data)]]
#   vis <- register_scales_from_props(vis, my_props)
#
#   # Create ggvis object initialised with current data and props
#   child <- ggvis()
#   child <- add_data(child, data = cur_data(vis), name = names(last(vis$data)),
#     add_suffix = FALSE)
#
#   # Run layer function and add missing scales
#   child <- layer(child)
#   # child <- add_missing_scales(child)
#
#   # Copy child data back into parent.
#   vis$data <- c(vis$data, child$data)
#
#   # Merge into reactives and related from child into parent
#   vis$reactives <- c(vis$reactives, child$reactives)
#   vis$connectors <- c(vis$connectors, child$connectors)
#   vis$handlers <- c(vis$handlers, child$handlers)
#   vis$controls <- c(vis$controls, child$controls)
#
#   child$reactives <- NULL
#   child$controls <- NULL
#   child$connectors <- NULL
#   child$handlers <- NULL
#
#   child$options <- NULL
#   child$cur_date <- NULL
#   child$cur_props <- NULL
#
#   # Make this object behave more like a regular mark
#   # assuming for now that there's only one dataset
#   child$data <- my_data
#   child$props <- my_props
#   class(child) <- "subvis"
#   vis$marks <- c(vis$marks, list(child))

  vis
}
