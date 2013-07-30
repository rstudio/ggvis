#' Create a default scale for a given property and variable type.
#' 
#' Default scales depend on both the property (e.g. fill, x, opacity) and
#' the type of variable (e.g. numeric, nominal, ordinal). For this reason
#' \code{default_scale} implements a crude S3-like double dispatch mechanism,
#' looking for a function called \code{dscale_prop_type}.
#' 
#' @param prop A vega property name, converted into a scale name by
#'   \code{\link{prop_to_scale}}
#' @param type A variable type, as returned by \code{\link{prop_type}}
#' @param ... other arguments passed to the scale function
#' @export
#' @examples
#' default_scale("x", "numeric")
#' default_scale("fillOpacity", "ordinal")
#' default_scale("stroke", "nominal")
default_scale <- function(prop, type, ...) { 
  assert_that(is.string(prop), is.string(type))
  if (type == "NULL") return()
  
  scale <- prop_to_scale(prop)
  
  # This is an interim design choice making it clear that user can not
  # modify the default scales.
  env <- asNamespace("gigvis")
  
  f_name <- paste("dscale", scale, type, sep = "_")
  if (!exists(f_name, mode = "function", envir = env)) {
    stop("Don't know how to make default scale for ", prop, 
      " with variable of type ", type, call. = FALSE)
  }
  
  f <- get(f_name, mode = "function", envir = env)
  f(name = scale, ...)
}


dscale_x_numeric  <- function(...) scale_quantitative(..., range = "width")
dscale_x_ordinal  <- function(...) scale_ordinal(..., range = "width")
dscale_x_nominal  <- function(...) scale_ordinal(..., range = "width")
dscale_x_datetime <- function(...) scale_time(..., range = "width")

dscale_y_numeric  <- function(...) scale_quantitative(..., range = "height")
dscale_y_ordinal  <- function(...) scale_ordinal(..., range = "height")
dscale_y_nominal  <- function(...) scale_ordinal(..., range = "height")
dscale_y_datetime <- function(...) scale_time(..., range = "width")

dscale_stroke_numeric  <- function(...) scale_quantitative(..., range = c("#132B43", "#56B1F7"))
dscale_stroke_ordinal  <- function(...) scale_ordinal(..., range = c("#132B43", "#56B1F7"))
dscale_stroke_nominal <- function(...) scale_ordinal(..., range = "category10")

# Fill colours should really be a little more saturated
dscale_fill_numeric  <- function(...) scale_quantitative(..., range = c("#132B43", "#56B1F7"))
dscale_fill_ordinal  <- function(...) scale_ordinal(..., range = c("#132B43", "#56B1F7"))
dscale_fill_nominal <- function(...) scale_ordinal(..., range = "category10")

dscale_shape_nominal <- function(...) scale_ordinal(..., range = "shapes")

dscale_size_numeric <- function(...) scale_quantiative(..., range = c(1, 20))
dscale_size_ordinal <- function(...) scale_ordinal(..., range = c(1, 20))

dscale_fontSize_numeric <- function(...) scale_quantiative(..., range = c(10, 20))
dscale_fontSize_ordinal <- function(...) scale_ordinal(..., range = c(10, 20))

dscale_opacity_numeric <- function(...) scale_quantitative(..., range = c(0, 1))
dscale_opacity_ordinal <- function(...) scale_ordinal(..., range = c(0, 1))

dscale_angle_numeric <- function(...) scale_quantitative(..., range = c(0, 2 * pi))
dscale_angle_ordinal <- function(...) scale_ordinal(..., range = c(0, 2 * pi))

dscale_radius_numeric <- function(...) scale_quantitative(..., range = c(0, 50))
dscale_radius_ordinal <- function(...) scale_ordinal(..., range = c(0, 50))

#' Convert the name of a property to the name of it's default scale.
#' 
#' This is mainly used to ensure that similar properties share the same
#' scale by default - e.g. \code{x} and \code{x2} should use the same
#' scale.
#' 
#' @param prop character vector of property names. Any unrecognised names
#'   are left unchanged.
#' @return character vector of default scale names.
#' @keywords internal
#' @export
#' @examples
#' prop_to_scale(c("x", "x2"))
#' prop_to_scale(c("foo", "bar"))
#' prop_to_scale(c("opacity", "fillOpacity", "strokeOpacity"))
prop_to_scale <- function(prop) {
  simplify <- c(
    "x2" = "x",
    "y2" = "y",
    "fillOpacity" = "opacity",
    "strokeOpacity" = "opacity",
    "innerRadius" = "radius",
    "outerRadius" = "radius",
    "startAngle" = "angle",
    "endAngle" = "angle"
  )
  
  matches <- match(prop, names(simplify))
  prop[!is.na(matches)] <- simplify[prop[!is.na(matches)]]
  prop
}
