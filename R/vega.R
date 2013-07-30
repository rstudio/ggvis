#' Create a vega specification.
#'
#' @export
vega_spec <- function(x, nodes, data_table,
                      width = 600, height = 400, padding = NULL) {

  data_names <- ls(data_table, all = TRUE)
  if (x$dynamic) {
    datasets <- lapply(data_names, function(name) {
      # Don't provide data now, just the name
      list(name = name)
    })
  } else {
    datasets <- lapply(data_names, function(name) {
      data <- isolate(data_table[[name]]())
      out <- d3df(data)
      out$name <- name
      out
    })
  }
  
  scales <- find_scales(x, nodes, data_table)
  spec <- list(
    data = datasets,
    scales = scales,
    marks = lapply(nodes, vega_mark),
    width = width,
    height = height,
    legends = vega_legends(scales),
    axes = vega_axes(scales)
  )

  if (!is.null(padding)) {
    spec$padding <- c(
      top = padding[1],
      right = padding[2],
      bottom = padding[3],
      left = padding[4]
    )
  }

  spec
}
