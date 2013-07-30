# Given scales, return a data structure for vega legends
vega_legends <- function(scales) {
  scale_names <- vapply(scales, function(x) x$name, character(1))

  # Keep only particular scales for the legend
  keep_scales <- c("size", "shape", "fill", "stroke")
  scales <- scales[scale_names %in% keep_scales]

  legends <- lapply(scales, vega_legend)

  legends
}

# Given one scale, return data structure for a vega legend
vega_legend <- function(scale) {
  legend <- list()
  legend[[scale$name]] <- scale$name

  legend
}
