# Given a gigvis scales object, a named vector of mappings, and the name of the
# source data, return a vega scales object.
vega_scales <- function(scales, mapping, data) {
  scales <- lapply(names(scales), function(name) {
    vega_scale(scales[[name]], mapping[[name]], data)
  })

  unname(scales)
}


# Given a gigvis scale, domain (the name of a source column, like 'mpg'), and
# name of data set, return a vega scale specification.
vega_scale <- function(scale, domain, data) {

  if (scale$name == "x") {
    list(
      name   = scale$name,
      type   = scale$type,
      range  = "width",
      domain = list(data = data, field = paste("data", domain, sep = ".")),
      zero   = FALSE,
      nice   = FALSE
    )

  } else if (scale$name == "y") {
    list(
      name   = scale$name,
      type   = scale$type,
      range  = "height",
      domain = list(data = data, field = paste("data", domain, sep = ".")),
      zero   = FALSE,
      nice   = FALSE
    )

  } else if (scale$name == "color") {
    list(
      name   = scale$name,
      type   = scale$type,
      range  = "category10"
    )

  } else if (scale$name == "fill") {
    list(
      name   = scale$name,
      type   = scale$type,
      range  = "category10"
    )
  }
}
