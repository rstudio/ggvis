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
#' @param size,shape,fill,stroke The name of the scale that determines the
#'   legends size, shape, fill and stroke.
#' @param orient The orientation of the legend. One of "left" or "right". This
#'   determines how the legend is positioned within the scene. The default is
#'   "right".
#' @param title A title for the legend. By default, it uses the name the fields
#'   used in the legend. Use \code{""} to suppress the title.
#' @param format The formatting pattern for axis labels. Vega uses D3's format
#'   pattern: \url{https://github.com/mbostock/d3/wiki/Formatting}
#' @param values  Explicitly set the visible legend values.
#' @param properties Optional mark property definitions for custom legend
#'   styling. Should be a named list (title, label, symbols, gradient, legend)
#'   of \code{\link{props}}.
#' @export
#' @examples
#' mtcars %>% ggvis(x = ~wt, y = ~mpg, fill = ~cyl) %>%
#'   layer_point() %>%
#'   set_guide_legend(fill = "fill", title = "Cylinders")
set_guide_legend <- function(vis, size = NULL, shape = NULL, fill = NULL,
                         stroke = NULL, orient = "right", title = NULL,
                         format = NULL, values = NULL, properties = NULL) {

  legend <- guide_legend(size, shape, fill, stroke, orient, title, format,
                         values, properties)

  add_legend(vis, legend)
}

# Create a legend object.
guide_legend <- function(size = NULL, shape = NULL, fill = NULL,
                         stroke = NULL, orient = "right", title = NULL,
                         format = NULL, values = NULL, properties = NULL) {

  orient <- match.arg(orient, c("right", "left"))

  structure(compact(list(
      size = size, shape = shape, fill = fill, stroke = stroke,
      orient = orient, title = title, format = format, values = values,
      properties = properties
  )), class = "vega_legend")
}

add_default_legends <- function(vis) {
  legends <- vis$legends
  scales <- vis$scales

  legs <- c("size", "shape", "fill", "stroke")
  present <- unlist(lapply(legends, function(x) x[legs]))

  missing <- setdiff(intersect(names(scales), legs), present)

  for (scale in missing) {
    args <- list(vis)
    args[[scales[[scale]]$name]] <- scale
    vis <- do.call(set_guide_legend, args)
  }

  vis
}

# Some legend settings require examining the scale
apply_legends_defaults <- function(vis) {
  legends <- vis$legends
  scales <- vis$scales

  legs <- c("size", "shape", "fill", "stroke")

  lapply(legends, function(legend) {
    present <- unlist(legend[legs])
    present_scales <- scales[present]

    if (is.null(legend$title)) {
      # Default title for each legend consists of the fields pasted together
      fields <- vapply(present_scales, function(scale) {
        # scale$domain can be a numeric vector, in which case return ""
        if (!is.list(scale$domain)) return("")

        field <- scale$domain$fields[[1]]$field
        sub("^data\\.", "", field)
      }, FUN.VALUE = character(1))

      fields <- fields[fields != ""]
      legend$title <- paste(fields, collapse = ".")
    }

    legend
  })

  # Replace the original legends with the new ones
  vis$legends <- legends
  vis
}

#' @export
format.vega_legend <- format.vega_axis

#' @export
print.vega_legend <- print.vega_axis
