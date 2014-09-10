#' @export
set_default_scale <- function(...) {
  stop("set_default_scale has been removed, and has been replaced by scale_numeric, ",
       "scale_nominal, and so on. See ?scales for more information")
}
#' @export
set_dscale <- set_default_scale

# Given a ggvis_scale object, apply defaults and then return the modified object.
apply_scale_defaults <- function(x) UseMethod("apply_scale_defaults")

#' @export
apply_scale_defaults.ggvis_scale <- function(x) x

#' @export
apply_scale_defaults.scale_numeric <- function(x) {
  x$zero <- x$zero %||% FALSE
  x$nice <- x$nice %||% FALSE
  x$clamp <- x$clamp %||% FALSE

  if (is.null(x$range)) {
    x$range <- switch(propname_to_scale(x$property),
      x = "width",
      y = "height",
      stroke = c("#132B43", "#56B1F7"),
      fill = c("#132B43", "#56B1F7"),
      size = c(20, 100),
      fontSize = c(10, 20),
      opacity = c(0, 1),
      angle = c(0, 2 * pi),
      radius = c(0, 50),
      stop("Don't know how to automatically set range for ", x$property, ".")
    )
  }
  if (propname_to_scale(x$property) %in% c("x", "y") && is.null(x$expand)) {
    x$expand <- 0.05
  }

  x
}

#' @export
apply_scale_defaults.scale_datetime <- function(x) {
  x$type <- x$type %||% "time"
  x$clamp <- x$clamp %||% FALSE

  if (is.null(x$range)) {
    x$range <- switch(propname_to_scale(x$property),
      x = "width",
      y = "height",
      stroke = c("#132B43", "#56B1F7"),
      fill = c("#132B43", "#56B1F7"),
      size = c(20, 100),
      fontSize = c(10, 20),
      opacity = c(0, 1),
      angle = c(0, 2 * pi),
      radius = c(0, 50),
      stop("Don't know how to automatically set range for ", x$property, ".")
    )
  }
  if (propname_to_scale(x$property) %in% c("x", "y") && is.null(x$expand)) {
    x$expand <- 0.05
  }

  x
}

#' @export
apply_scale_defaults.scale_ordinal <- function(x) {
  x$points <- x$points %||% TRUE
  x$sort <- x$sort %||% FALSE

  if (is.null(x$range)) {
    x$range <- switch(propname_to_scale(x$property),
      x = "width",
      y = "height",
      stroke = "category10",
      strokeDash = list(c(100000, 1), c(8, 6), c(2, 2), c(3, 4, 10, 4),
                        c(15, 3), c(5, 2, 10, 2)),
      fill = "category10",
      size = c(10, 100),
      stop("Don't know how to automatically set range for ", x$property, ".")
    )
  }
  if (is.null(x$padding) && x$property %in% c("x", "y")) {
    if (isTRUE(x$points)) x$padding <- 0.5
    else x$padding <- 0.1
  }
  x
}

#' @export
apply_scale_defaults.scale_nominal <- function(x) {
  x$points <- x$points %||% TRUE
  x$sort <- x$sort %||% FALSE

  if (is.null(x$range)) {
    x$range <- switch(propname_to_scale(x$property),
      x = "width",
      y = "height",
      stroke = "category10",
      strokeDash = list(c(100000, 1), c(8, 6), c(2, 2), c(3, 4, 10, 4),
                        c(15, 3), c(5, 2, 10, 2)),
      fill = "category10",
      shape = "shapes"
    )
  }
  if (is.null(x$padding) && x$property %in% c("x", "y")) {
    if (isTRUE(x$points)) x$padding <- 0.5
    else x$padding <- 0.1
  }
  x
}
