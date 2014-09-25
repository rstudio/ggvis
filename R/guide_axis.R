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
#'   Should be an object created by \code{\link{axis_props}}, with properties
#'   for ticks, majorTicks, minorTicks, grid, labels, title, and axis.
#' @seealso Vega axis documentation:
#'   \url{https://github.com/trifacta/vega/wiki/Axes}
#' @export
#' @examples
#' mtcars %>% ggvis(x = ~wt, y = ~mpg, fill = ~cyl) %>%
#'   layer_points() %>%
#'   add_axis("x", title = "Weight", orient = "top")
#'
#' # Suppress axis with hide_axis
#' mtcars %>% ggvis(x = ~wt, y = ~mpg, fill = ~cyl) %>%
#'   layer_points() %>%
#'   hide_axis("x") %>% hide_axis("y")
#'
#' mtcars %>% ggvis(x = ~wt, y = ~mpg) %>% layer_points() %>%
#'   add_axis("x", title = "Weight", ticks = 40,
#'     properties = axis_props(
#'       ticks = list(stroke = "red"),
#'       majorTicks = list(strokeWidth = 2),
#'       grid = list(stroke = "red"),
#'       labels = list(
#'         fill = "steelblue",
#'         angle = 50,
#'         fontSize = 14,
#'         align = "left",
#'         baseline = "middle",
#'         dx = 3
#'       ),
#'       title = list(fontSize = 16),
#'       axis = list(stroke = "#333", strokeWidth = 1.5)
#'     )
#'   )
add_axis <- function(vis, type, scale = NULL, orient = NULL, title = NULL,
                       title_offset = NULL, format = NULL, ticks = NULL,
                       values = NULL, subdivide = NULL, tick_padding = NULL,
                       tick_size_major = NULL, tick_size_minor = tick_size_major,
                       tick_size_end = tick_size_major, offset = NULL,
                       layer = "back", grid = TRUE, properties = NULL) {

  if (is.null(scale)) {
    if (is.null(vis$cur_vis)) {
      scale <- type
    } else {
      scale <- paste0(type, paste0(vis$cur_vis, collapse = "-"))
    }
  }

  axis <- create_axis(type, scale, orient, title, title_offset, format,
                      ticks, values, subdivide, tick_padding,
                      tick_size_major, tick_size_minor, tick_size_end,
                      offset, layer, grid, properties)

  append_ggvis(vis, "axes", axis)
}

#' @rdname add_axis
#' @export
hide_axis <- function(vis, scale) {
  axis <- structure(list(scale = scale, hide = TRUE), class = "ggvis_axis")
  append_ggvis(vis, "axes", axis)
}

#' Defunct function for adding an axis
#'
#' This function has been replaced with \code{\link{add_axis}}.
#' @param ... Other arguments.
#' @export
add_guide_axis <- function(...) {
  stop("add_guide_axis() has been replaced by add_axis().")
}

# Create an axis object.
create_axis <- function(type, scale = type, orient = NULL, title = NULL,
                 title_offset = NULL, format = NULL, ticks = NULL,
                 values = NULL, subdivide = NULL, tick_padding = NULL,
                 tick_size_major = NULL, tick_size_minor = tick_size_major,
                 tick_size_end = tick_size_major, offset = NULL,
                 layer = "back", grid = TRUE, properties = NULL) {

  assert_that(type %in% c("x", "y"))
  assert_that(is.string(scale))

  if (is.null(orient)) orient <- c(x = "bottom", y = "left")[type]
  orient <- match.arg(orient, c("top", "right", "bottom", "left"))

  assert_that(is.null(title) || is.string(title))
#   assert_that(is.number(title_offset))

  layer <- match.arg(layer, c("front", "back"))
  assert_that(is.flag(grid))

  assert_that(is.null(properties) || is.axis_props(properties))

  structure(compact(list(
      type = type, scale = scale, orient = orient, title = title,
      titleOffset = title_offset, format = format, ticks = ticks,
      values = values, subdivide = subdivide, tickPadding = tick_padding,
      tickSizeMajor = tick_size_major, tickSizeMinor = tick_size_minor,
      tickSizeEnd = tick_size_end, offset = offset, layer = layer,
      grid = grid, properties = properties
  )), class = "ggvis_axis")
}


add_missing_axes <- function(vis) {
  axes <- vis$axes
  scales <- vis$scales

  present <- vapply(axes, "[[", "scale", FUN.VALUE = character(1))
  missing <- setdiff(intersect(names(scales), c("x", "y")), present)

  for (scale in missing) {
    vis <- add_axis(vis, scale)
  }
  vis
}

# Some axis settings require examining the scale
apply_axes_defaults <- function(vis) {
  axes <- vis$axes
  scales <- vis$scales

  axes <- lapply(axes, function(axis) {
    scale <- scales[[axis$scale]]

    # If we don't have a title, try to get it from the scale.
    # Use [[-indexing to avoid partial name matching of "titleOffset". (#269)
    if (is.null(axis[["title"]])) {
      axis$title <- scale$label
    }

    axis
  })

  # Replace the original axes with the new ones
  vis$axes <- axes
  vis
}

#' @export
format.ggvis_axis <- function(x, ...) {
  params <- param_string(x, collapse = FALSE)
  param_s <- paste0("  ", format(paste0(names(params), ":")), " ", format(params),
    collapse = "\n")

  paste0("<", class(x)[1], ">\n", param_s)
}

#' @export
print.ggvis_axis <- function(x, ...) cat(format(x, ...), "\n", sep = "")
