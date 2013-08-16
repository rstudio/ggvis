#' A band
#'
#' Bands are used to set the width or height on categorical scales.
#'
#' @export
band <- function(offset = NULL, mult = NULL) {
  prop("band", offset = offset, mult = mult)
}

#' @S3method format band
format.band <- function(x, ...) {
  paste0("<band>")
}

#' @S3method print band
print.band <- function(x, ...) cat(format(x, ...), "\n", sep = "")

#' @rdname band
#' @param x object to for band-ness
is.band <- function(x) inherits(x, "band")

#' @S3method prop_value band
prop_value.band <- function(x, data, processed = FALSE) {
  NULL
}

#' @S3method prop_name band
prop_name.band <- function(x) {
  ""
}

#' @S3method prop_scale band
prop_scale.band <- function(x, default_scale) {
  switch(default_scale, width = "x", height = "y")
}

#' @S3method prop_domain band
prop_domain.band <- function(x, data) {
  NULL
}

#' @S3method prop_vega band
prop_vega.band <- function(x, default_scale) {
  compact(list(
    scale = prop_scale(x, default_scale),
    mult = x$mult,
    offset = x$offset,
    band = TRUE
  ))
}
