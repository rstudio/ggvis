
mark <- function(type, ...) {
  structure(
    list(type = type, ...),
    class = c(paste0("mark_", type), "mark", "gigvis_node")
  )
}

print.mark <- function(x, ...) str(x)

#' @export
mark_point <- function(x = NULL, y = NULL, opacity = NULL,
                       fill = NULL, fillOpacity = NULL,
                       stroke = NULL, strokeWidth = NULL, strokeOpacity = NULL,
                       size = NULL, shape = NULL) {

  mark("point", x = x, y = y, opacity = opacity,
    fill = fill, fillOpacity = fillOpacity,
    stroke = stroke, strokeWidth = strokeWidth, strokeOpacity = strokeOpacity,
    size = size, shape = shape)

}

#' @export
mark_line <- function(x = NULL, y = NULL, opacity = NULL,
                      fill = NULL, stroke = NULL, strokeWidth = NULL,
                      strokeOpacity = NULL) {

  mark("line", x = x, y = y, opacity = opacity, fill = fill,
    stroke = stroke, strokeWidth = strokeWidth, strokeOpacity = strokeOpacity)
}

#' @export
mark_ribbon <- function(x = NULL, y = NULL, y2 = NULL, opacity = NULL,
                        fill = NULL, fillOpacity = NULL,
                        stroke = NULL, strokeWidth = NULL, strokeOpacity = NULL) {

  mark("ribbon", x = x, y = y, y2 = y2, opacity = opacity,
    fill = fill, fillOpacity = fillOpacity,
    stroke = stroke, strokeWidth = strokeWidth, strokeOpacity = strokeOpacity)
}

#' @export
mark_rect <- function(x = NULL, x2 = NULL, y = NULL, y2 = NULL, width = NULL,
                      opacity = NULL,
                      fill = NULL, fillOpacity = NULL,
                      stroke = NULL, strokeWidth = NULL, strokeOpacity = NULL) {
  mark("rect", x = x, x2 = x2, y = y, y2 = y2, width = width,
    opacity = opacity,
    fill = fill, fillOpacity = fillOpacity,
    stroke = stroke, strokeWidth = strokeWidth, strokeOpacity = strokeOpacity)
}
