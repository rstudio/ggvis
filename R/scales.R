# Given a ggvis object, add all needed vega scales, with correct domain
# values set.
add_default_scales <- function(vis) {
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
