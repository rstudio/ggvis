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
  if (is.null(x$range)) {
    x$range <- switch(x$property,
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
  x
}

#' @export
apply_scale_defaults.scale_datetime <- function(x) {
  if (is.null(x$range)) {
    x$range <- switch(x$property,
      x = "width",
      y = "height",
      stop("Don't know how to automatically set range for ", x$property, ".")
    )
  }
  x
}

#' @export
apply_scale_defaults.scale_ordinal <- function(x) {
  if (is.null(x$range)) {
    x$range <- switch(x$property,
      x = "width",
      y = "height",
      stroke = "category10",
      fill = "category10",
      size = c(10, 100),
      stop("Don't know how to automatically set range for ", x$property, ".")
    )
  }
  if (is.null(x$padding)) {
    x$padding <- switch(x$property,
      x = 0.5,
      y = 0.5,
      NULL
    )
  }
  x
}

apply_scale_defaults.scale_nominal <- function(x) {
  if (is.null(x$range)) {
    x$range <- switch(x$property,
      x = "width",
      y = "height",
      stroke = "category10",
      fill = "category10",
      shape = "shapes"
    )
  }
  if (is.null(x$padding)) {
    x$padding <- switch(x$property,
      x = 0.5,
      y = 0.5,
      NULL
    )
  }
  x
}
