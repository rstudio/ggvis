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
#'   subvis(width := 20, height := 20, stroke := "red",
#'     layer = function(x) x %>% layer_points(~month, ~ozone))
subvis <- function(vis, ..., layer) {
  # Initial hacky implementation
  my_props <- merge_props(cur_props(vis), props(...))

  old <- set_prefix("xyz-")
  on.exit(set_prefix(old), add = TRUE)
  my_data <- vis$data[[length(vis$data)]]
  vis <- register_scales_from_props(vis, my_props)

  # Create ggvis object initialised with current data and props
  child <- ggvis()
  child <- add_data(child, data = cur_data(vis), name = names(last(vis$data)),
    add_suffix = FALSE)

  # Run layer function and add missing scales
  child <- layer(child)
  # child <- add_missing_scales(child)

  # Copy child data back into parent.
  vis$data <- c(vis$data, child$data)

  # Merge into reactives and related from child into parent
  vis$reactives <- c(vis$reactives, child$reactives)
  vis$connectors <- c(vis$connectors, child$connectors)
  vis$handlers <- c(vis$handlers, child$handlers)
  vis$controls <- c(vis$controls, child$controls)

  child$reactives <- NULL
  child$controls <- NULL
  child$connectors <- NULL
  child$handlers <- NULL

  child$options <- NULL
  child$cur_date <- NULL
  child$cur_props <- NULL

  # Make this object behave more like a regular mark
  # assuming for now that there's only one dataset
  child$data <- my_data
  child$props <- my_props
  class(child) <- "subvis"
  vis$marks <- c(vis$marks, list(child))

  vis
}



is.subvis <- function(x) inherits(x, "subvis")
