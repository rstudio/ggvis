#' Add a vega legend specification to a ggvis plot
#'
#' Axis specifications allow you to either override the default legends,
#' or supply additional legends.
#'
#' More information about axes can be found in the "axes and legends" vignettes.
#'
#' @section Compared to ggplot2:
#'
#' In ggplot2, legend (and axis) properties are part of the scales
#' specification. In vega, they are separate, which allows the specification
#' of multiple legends, and more flexible linkage between scales and legends.
#'
#' @param vis A ggvis object.
#' @param scales The name of one or more scales for which to add a legend.
#'   Typically one of "size", "shape", "fill", "stroke", although custom scale
#'   names may also be used. Multiple names can also be used, like
#'   \code{c("fill", "shape")}.
#' @param orient The orientation of the legend. One of "left" or "right". This
#'   determines how the legend is positioned within the scene. The default is
#'   "right".
#' @param title A title for the legend. By default, it uses the name the fields
#'   used in the legend. Use \code{""} to suppress the title.
#' @param format The formatting pattern for axis labels. Vega uses D3's format
#'   pattern: \url{https://github.com/mbostock/d3/wiki/Formatting}
#' @param values  Explicitly set the visible legend values.
#' @param properties Optional mark property definitions for custom legend
#'   styling. Should be an object created by \code{\link{legend_props}}, with
#'   properties for title, label, symbols, gradient, legend.
#' @export
#' @examples
#' mtcars %>% ggvis(x = ~wt, y = ~mpg, fill = ~cyl) %>%
#'   layer_points() %>%
#'   add_legend("fill", title = "Cylinders")
#'
#' # Suppress legend with hide_legend
#' mtcars %>% ggvis(x = ~wt, y = ~mpg, fill = ~cyl) %>%
#'   layer_points() %>%
#'   hide_legend("fill")
#'
#' # Combining two properties in one legend
#' mtcars %>%
#'   ggvis(x = ~wt, y = ~mpg, fill = ~factor(cyl), shape = ~factor(cyl)) %>%
#'   layer_points() %>%
#'   add_legend(c("fill", "shape"))
#'
#' # Control legend properties with a continuous legend, with x and y position
#' # in pixels.
#' mtcars %>% ggvis(x = ~wt, y = ~mpg, fill = ~cyl) %>%
#'   layer_points() %>%
#'   add_legend("fill", title = "Cylinders",
#'     properties = legend_props(
#'       title = list(fontSize = 16),
#'       labels = list(fontSize = 12, fill = "#00F"),
#'       gradient = list(stroke = "red", strokeWidth = 2),
#'       legend = list(x = 500, y = 50)
#'     )
#'   )
#'
#' # Control legend properties with a categorical legend, with x and y position
#' # in the scaled data space.
#' mtcars %>% ggvis(x = ~wt, y = ~mpg, fill = ~factor(cyl)) %>%
#'   layer_points() %>%
#'   add_legend("fill", title = "Cylinders",
#'     properties = legend_props(
#'       title = list(fontSize = 16),
#'       labels = list(fontSize = 14, dx = 5),
#'       symbol = list(stroke = "black", strokeWidth = 2,
#'         shape = "square", size = 200),
#'       legend = list(
#'         x = scaled_value("x", 4.5),
#'         y = scaled_value("y", 30)
#'       )
#'     )
#'   )
#'
#' # Control legend position using x_rel and y_rel which specify relative
#' # position, going from 0 to 1. (0, 0) is the bottom-left corner, and
#' # (1, 1) is the upper-right corner. The values control the position of
#' # the upper-left corner of the legend.
#' mtcars %>% ggvis(x = ~wt, y = ~mpg, fill = ~cyl) %>%
#'   layer_points() %>%
#'   add_relative_scales() %>%
#'   add_legend("fill", title = "Cylinders",
#'     properties = legend_props(
#'       legend = list(
#'         x = scaled_value("x_rel", 0.8),
#'         y = scaled_value("y_rel", 1)
#'       )
#'     )
#'   )
add_legend <- function(vis, scales = NULL, orient = "right", title = NULL,
                       format = NULL, values = NULL, properties = NULL) {
  assert_that(!is.null(scale))

  # Create an unfortified legend
  legend <- structure(compact(list(
    scales = scales, orient = orient, title = title, format = format,
    values = values, properties = properties
  )), class = "ggvis_legend")

  append_ggvis(vis, "legends", legend)
}

#' @rdname add_legend
#' @export
hide_legend <- function(vis, scales) {
  legend <- structure(list(scales = scales, hide = TRUE), class = "ggvis_legend")
  append_ggvis(vis, "legends", legend)
}

#' Defunct function for adding a legend
#'
#' This function has been replaced with \code{\link{add_legend}}.
#' @param ... Other arguments.
#' @export
add_guide_legend <- function(...) {
  stop("add_guide_legend() has been replaced by add_legend().")
}

# Given a ggvis object, find all the unfortified legend and fortify them.
# The fortification process requires examining the scales in the ggvis object.
fortify_legends <- function(vis) {
  all_scales <- collapse_scales(gather_scales(vis))
  # Get a named vector where names are scales, values are properties
  scales_props <- vapply(all_scales, function(s) s$property, character(1))

  vis$legends <- lapply(vis$legends, fortify_legend, scales_props)
  vis
}

# Create a fortified legend object. An unfortified legend has only the names
# of the scales. A fortified legends maps those names of scales to property
# names.
# scales_props is a named list, where names are scales, values are properties
fortify_legend <- function(legend, scales_props) {
  if (inherits(legend, "fortified_legend")) return(legend)

  # Get scales and props that are actually used by this legend
  scales_props <- scales_props[legend$scales]

  assert_that(!is.null(scales_props) && length(scales_props) > 0)
  assert_that(is.null(legend$properties) || is.legend_props(legend$properties))

  legend <- structure(compact(list(
      orient = legend$orient, title = legend$title, format = legend$format,
      values = legend$values, properties = legend$properties,
      hide = legend$hide
  )), class = c("fortified_legend", "ggvis_legend"))

  legend[scales_props] <- names(scales_props)
  legend
}

add_missing_legends <- function(vis) {
  legends <- vis$legends
  scales <- vis$scales

  legs <- c("size", "shape", "fill", "stroke")
  # Get scales that are in some legend
  present <- unlist(lapply(legends, function(x) x$scales))
  # Find scales that don't have legend
  missing <- setdiff(intersect(names(scales), legs), present)

  for (scale in missing) {
    vis <- add_legend(vis, scale)
  }

  vis
}

# Some legend settings require examining the scale
apply_legends_defaults <- function(vis) {
  legends <- vis$legends
  scales <- vis$scales

  legs <- c("size", "shape", "fill", "stroke")

  legends <- lapply(legends, function(legend) {
    if (isTRUE(legend$hide)) return(legend)

    present <- unlist(legend[legs])
    present_scales <- scales[present]

    # Use [[-indexing to avoid partial name matching of "titleOffset". (#269)
    if (is.null(legend[["title"]])) {
      legend$title <- present_scales[[1]]$label
    }

    legend
  })

  # Replace the original legends with the new ones
  vis$legends <- legends
  vis
}

#' @export
format.ggvis_legend <- format.ggvis_axis

#' @export
print.ggvis_legend <- print.ggvis_axis
