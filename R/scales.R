#' Create a scale
#'
#' @param scale The name of a scale, such as "x", "y", "fill", "stroke", etc.
#' @param type The type of scale. One of "numeric", "nominal", "ordinal", "logical", "datetime".
#'
#'
#' @export
scale_numeric <- function(vis, scale, ..., name = NULL) {
  add_scale(vis, default_vega_scale(scale, "numeric", ..., name = name))
}
#' @export
scale_nominal <- function(vis, scale, ..., name = NULL) {
  add_scale(vis, default_vega_scale(scale, "nominal", ..., name = name))
}
#' @export
scale_ordinal <- function(vis, scale, ..., name = NULL) {
  add_scale(vis, default_vega_scale(scale, "ordinal", ..., name = name))
}
#' @export
scale_logical <- function(vis, scale, ..., name = NULL) {
  add_scale(vis, default_vega_scale(scale, "logical", ..., name = name))
}
#' @export
scale_datetime <- function(vis, scale, ..., name = NULL) {
  add_scale(vis, default_vega_scale(scale, "datetime", ..., name = name))
}

# Given a ggvis object, add all needed vega scales, with correct domain
# values set.
add_missing_scales <- function(vis) {
  scales <- vis$scales

  # Add in scales not already specified in spec
  needed <- setdiff(names(vis$scale_info), names(scales))
  for (scale_n in needed) {
    info <- vis$scale_info[[scale_n]]
    scale <- default_scale(scale_n, info$type)
    vis <- add_scale(vis, scale)
  }

  vis
}
