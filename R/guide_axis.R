#' Add a vega axis specification to a ggvis plot
#'
#' Axis specifications allow you to either override the default axes,
#' or additional axes.
#'
#' More information about axes can be found in the "axes and legends" vignettes.
#'
#' @section Compared to ggplot2:
#'
#' In ggplot2, axis (and legend) properties are part of the scales
#' specification. In vega, they are separate, which allows the specification
#' of multiple axes, and more flexible linkage between scales and axes.
#'
#' @param vis A ggvis object.
#' @param type The type of axis. Either x or y.
#' @param scale The name of the scale backing the axis component. Defaults to
#'   the scale type - you will need to specify if you want (e.g.) a scale
#'   for a secondary y-axis.
#' @param orient The orientation of the axis. One of top, bottom, left or right.
#'   The orientation can be used to further specialize the axis type (e.g., a y
#'   axis oriented for the right edge of the chart) - defaults to bottom for
#'   x axes, and left for y axes.
#' @param title A title for the axis. By default, it uses the name of the field
#'   in the first data set used by the scale. Use \code{""} to suppress the
#'   title.
#' @param title_offset The offset (in pixels) from the axis at which to place
#'   the title.
#' @param format The formatting pattern for axis labels. Vega uses D3's format
#'   pattern: \url{https://github.com/mbostock/d3/wiki/Formatting}
#' @param ticks A desired number of ticks. The resulting number may be different
#'   so that values are "nice" (multiples of 2, 5, 10) and lie within the
#'   underlying scale's range.
#' @param values Explicitly set the visible axis tick values.
#' @param subdivide If provided, sets the number of minor ticks between major
#'   ticks (the value 9 results in decimal subdivision).
#' @param tick_padding The padding, in pixels, between ticks and text labels.
#' @param tick_size_major,tick_size_minor,tick_size_end
#'   The size, in pixels, of major, minor and end ticks.
#' @param offset The offset, in pixels, by which to displace the axis from the
#'   edge of the enclosing group or data rectangle.
#' @param layer A string indicating if the axis (and any gridlines) should be
#'   placed above or below the data marks. One of "front" or "back" (default).
#' @param grid A flag indicating if gridlines should be created in addition to
#'   ticks.
#' @param properties Optional mark property definitions for custom axis styling.
#'   Should be a named list (ticks, majorTicks, minorTicks, labels and axis) of
#'   \code{\link{props}}.
#' @seealso Vega axis documentation:
#'   \url{https://github.com/trifacta/vega/wiki/Axes}
#' @export
#' @examples
#' mtcars %>% ggvis(x = ~wt, y = ~mpg, fill = ~cyl) %>%
#'   layer_point() %>%
#'   set_guide_axis("x", title = "Weight", orient = "top")
#'
#' mtcars %>% ggvis(x = ~wt, y = ~mpg, fill = ~cyl) %>%
#'   layer_point() %>%
#'   set_guide_axis("x", properties = list(ticks = props(stroke = "red")))
set_guide_axis <- function(vis, type, scale = type, orient = NULL, title = NULL,
                           title_offset = NULL, format = NULL, ticks = NULL,
                           values = NULL, subdivide = NULL, tick_padding = NULL,
                           tick_size_major = NULL, tick_size_minor = tick_size_major,
                           tick_size_end = tick_size_major, offset = NULL,
                           layer = "back", grid = TRUE, properties = list()) {

  axis <- guide_axis(type, scale, orient, title, title_offset, format, ticks,
                     values, subdivide, tick_padding, tick_size_major,
                     tick_size_minor, tick_size_end, offset,
                     layer, grid, properties)

  add_axis(vis, axis)
}

# Create an axis object.
guide_axis <- function(type, scale = type, orient = NULL, title = NULL,
                 title_offset = NULL, format = NULL, ticks = NULL,
                 values = NULL, subdivide = NULL, tick_padding = NULL,
                 tick_size_major = NULL, tick_size_minor = tick_size_major,
                 tick_size_end = tick_size_major, offset = NULL,
                 layer = "back", grid = TRUE, properties = list()) {

  assert_that(type %in% c("x", "y"))
  assert_that(is.string(scale))

  if (is.null(orient)) orient <- c(x = "bottom", y = "left")[type]
  orient <- match.arg(orient, c("top", "right", "bottom", "left"))

  assert_that(is.null(title) || is.string(title))
#   assert_that(is.number(title_offset))

  layer <- match.arg(layer, c("front", "back"))
  assert_that(is.flag(grid))

  structure(compact(list(
      type = type, scale = scale, orient = orient, title = title,
      titleOffset = title_offset, format = format, ticks = ticks,
      values = values, subdivide = subdivide, tickPadding = tick_padding,
      tickSizeMajor = tick_size_major, tickSizeMinor = tick_size_minor,
      tickSizeEnd = tick_size_end, offset = offset, layer = layer,
      grid = grid, properties = properties
  )), class = "vega_axis")
}


add_default_axes <- function(vis) {
  axes <- vis$axes
  scales <- vis$scales

  present <- vapply(axes, "[[", "scale", FUN.VALUE = character(1))
  missing <- setdiff(intersect(names(scales), c("x", "y")), present)

  for (scale in missing) {
    axes[[scale]] <- guide_axis(scale)
  }

  for (axis in axes) {
    vis <- add_axis(vis, axis)
  }
  vis
}

# Some axis settings require examining the scale
apply_axes_defaults <- function(vis) {
  axes <- vis$axes
  scales <- vis$scales

  lapply(axes, function(axis) {
    scale <- scales[[axis$scale]]

    # If we don't have a title, try to get it from the scale.
    # Domain can be a named list with the field, in which case we can get the
    # title from the field; or it can be a numeric vector, in which case we
    # can't automatically get the title from the scale.
    if (is.null(axis$title) && is.list(scale$domain)) {
      title <- scale$domain$fields[[1]]$field
      title <- sub("^data\\.", "", title)
      axis$title <- title
    }

    axis
  })

  # Replace the original axes with the new ones
  vis$axes <- axes
  vis
}

#' @export
format.vega_axis <- function(x) {
  params <- param_string(x, collapse = FALSE)
  param_s <- paste0("  ", format(paste0(names(params), ":")), " ", format(params),
    collapse = "\n")

  paste0("<", class(x)[1], ">\n", param_s)
}

#' @export
print.vega_axis <- function(x, ...) cat(format(x, ...), "\n", sep = "")
