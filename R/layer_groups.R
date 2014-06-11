#' @examples
#' library(nasaweather)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' small <- atmos %>% filter(lat <= 28.71304, long <= -106.287, year == 1995)
#' small %>%
#'   group_by(long, lat) %>%
#'   ggvis(~long, ~lat) %>%
#'   subvis(
#'     layer = function(x) x %>% layer_points(~month, ~ozone)) %>%
#'  show_spec("marks")


#' A subvis is a recursive mark: a mark that contain other marks. Compared
#' to other marks, it can also contain scales, axes and legends.
#'
#' Once you've started a subvis, scale_*(), add_axis() and add_legend().
#' These functions will need to be modified to modify the properties on
#' the last mark (if it's is a sbuvis)
#'
#' Flatten needs to be recursive (again) - it has to spider through all
#' the datasets including the child
#'
#' Can't change data sets inside a group - need to overlay multiple groups

subvis <- function(vis, ..., layer) {
  new_props <- merge_props(cur_props(vis), props(...))

  # Initial hacky implementation

  # Create ggvis object initialised with current data and props
  child <- ggvis()
  child <- add_data(child, data = cur_data(vis))
  child <- add_props(child, .props = new_props)

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
  child$data <- child$data[[length(child$data)]]
  child$props <- new_props
  class(child) <- "subvis"
  vis$marks <- c(vis$marks, list(child))

  vis
}
