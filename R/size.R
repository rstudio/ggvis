#' Define size of plot.
#'
#' @param width,height Width and height of plot, in pixels.
#' @export
size <- function(width = 600, height = 400) {
  structure(
    list(width = width, height = height),
    class = "ggvis_size"
  )
}

#' @S3method as.vega ggvis_size
as.vega.ggvis_size <- function(x) {
  unclass(x)
}
