#' Generate a vega legend specification
#'
#' @param size,shape,fill,stroke The name of the scale that determines the
#'   legends size, shape, fill and stroke.
#' @param orient The orientation of the legend. One of "left" or "right". This
#'   determines how the legend is positioned within the scene. The default is
#'   "right".
#' @param title The title for the legend (none by default).
#' @param format The formatting pattern for axis labels. Vega uses D3's format
#'   pattern: \url{https://github.com/mbostock/d3/wiki/Formatting}
#' @param values  Explicitly set the visible legend values.
#' @param properties Optional mark property definitions for custom legend
#'   styling. Should be a named list (title, label, symbols, gradient, legend)
#'   of \code{\link{props}}.
#' @export
#' @examples
#' guide_legend(size = "size")
guide_legend <- function(size = NULL, shape = NULL, fill = NULL, stroke = NULL,
                   orient = "right", title = NULL, format = NULL, values = NULL,
                   properties = NULL) {

  orient <- match.arg(orient, c("right", "left"))

  structure(compact(list(
      size = size, shape = shape, fill = fill, stroke = stroke,
      orient = orient, title = title, format = format, values = values,
      properties = properties
  )), class = "vega_legend")
}

add_default_legends <- function(legends, scales) {
  legs <- c("size", "shape", "fill", "stroke")
  present <- unlist(lapply(legends, function(x) x[legs]))

  missing <- setdiff(intersect(names(scales), legs), present)

  for (scale in missing) {
    args <- setNames(list(scale), scales[[scale]]$name)
    legends[[scale]] <- do.call(guide_legend, args)
  }

  unname(legends)
}
