vega_axes <- function(scales) {
  names <- vapply(scales, "[[", "name", FUN.VALUE = character(1))
  axes <- intersect(names, c("x", "y"))
  axis_spec <- list(
    x = list(type = "x", scale = "x"), 
    y = list(type = "y", scale = "y")
  )
  unname(axis_spec[axes])
}

