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
#' @param scale The name of one or more scales for which to add a legend.
#' @param size,shape,fill,stroke The name of the scale that determines the
#'   legends for the properties size, shape, fill and stroke.
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
#'   add_legend(fill = "fill", title = "Cylinders")
#'
#' # Suppress legend with hide_legend
#' mtcars %>% ggvis(x = ~wt, y = ~mpg, fill = ~cyl) %>%
#'   layer_points() %>%
#'   hide_legend("fill")
#'
#' # Control legend properties with a continuous legend, with x and y position
#' # in pixels.
#' mtcars %>% ggvis(x = ~wt, y = ~mpg, fill = ~cyl) %>%
#'   layer_points() %>%
#'   add_legend(fill = "fill", title = "Cylinders",
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
#'   add_legend(fill = "fill", title = "Cylinders",
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
#'   add_legend(fill = "fill", title = "Cylinders",
#'     properties = legend_props(
#'       legend = list(
#'         x = scaled_value("x_rel", 0.8),
#'         y = scaled_value("y_rel", 1)
#'       )
#'     )
#'   )
add_legend <- function(vis, size = NULL, shape = NULL, fill = NULL,
                       stroke = NULL, orient = "right", title = NULL,
                       format = NULL, values = NULL, properties = NULL) {

  legend <- create_legend(size, shape, fill, stroke, orient, title, format,
                          values, properties)

  register_legend(vis, legend)
}

#' @rdname add_legend
#' @export
hide_legend <- function(vis, scale) {
  legend <- structure(list(scale = scale, hide = TRUE), class = "ggvis_legend")
  register_legend(vis, legend)
}

#' Defunct function for adding a legend
#'
#' This function has been replaced with \code{\link{add_legend}}.
#' @param ... Other arguments.
#' @export
add_guide_legend <- function(...) {
  stop("add_guide_legend() has been replaced by add_legend().")
}

# Create a legend object.
create_legend <- function(size = NULL, shape = NULL, fill = NULL,
                          stroke = NULL, orient = "right", title = NULL,
                          format = NULL, values = NULL, properties = NULL) {

  orient <- match.arg(orient, c("right", "left"))

  assert_that(is.null(properties) || is.legend_props(properties))

  structure(compact(list(
      size = size, shape = shape, fill = fill, stroke = stroke,
      orient = orient, title = title, format = format, values = values,
      properties = properties
  )), class = "ggvis_legend")
}

add_missing_legends <- function(vis) {
  legends <- vis$legends
  scales <- vis$scales

  legs <- c("size", "shape", "fill", "stroke")
  # Get scales that are in some legend
  present <- unlist(lapply(legends, function(x) x[legs]))
  # Ignore scales with hidden legend
  hidden <- unlist(lapply(legends, function(x) if (isTRUE(x$hide)) x$scale))
  # Find scales that don't have legend
  missing <- setdiff(intersect(names(scales), legs), c(present, hidden))

  for (scale in missing) {
    args <- list()
    args[[scale]] <- scales[[scale]]$property
    vis <- do_call(add_legend, quote(vis), .args = args)
  }

  vis
}

# Some legend settings require examining the scale
apply_legends_defaults <- function(vis) {
  legends <- vis$legends
  scales <- vis$scales

  legs <- c("size", "shape", "fill", "stroke")

  legends <- lapply(legends, function(legend) {
    present <- unlist(legend[legs])
    present_scales <- scales[present]

    if (is.null(legend$title)) {
      # Default title for each legend consists of the fields pasted together
      fields <- vpluck(present_scales, "label", character(1))
      legend$title <- paste(fields, collapse = ".")
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
