#' A band
#'
#' Bands are used to set the width or height on categorical scales.
#'
#' @export
band <- function(offset = NULL, mult = NULL) {
  structure(
    list(type = "band", offset = offset, mult = mult),
    class = c("band", "prop")
  )
}

#' @export
format.band <- function(x, ...) {
  paste0("<band>")
}

#' @export
print.band <- function(x, ...) cat(format(x, ...), "\n", sep = "")

#' @rdname band
#' @param x object to for band-ness
is.band <- function(x) inherits(x, "band")

#' @export
prop_value.band <- function(x, data) {
  NULL
}

#' @export
prop_name.band <- function(x) {
  ""
}

#' @export
prop_scale.band <- function(x, default_scale) {
  switch(default_scale, width = "x", height = "y")
}

#' @export
prop_domain.band <- function(x, data) {
  NULL
}

#' @export
prop_vega.band <- function(x, default_scale) {
  compact(list(
    scale = prop_scale(x, default_scale),
    mult = x$mult,
    offset = x$offset,
    band = TRUE
  ))
}
