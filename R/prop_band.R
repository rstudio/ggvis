#' A band
#'
#' Bands are used to set the width or height on categorical scales - a band
#' represent the height or width allocated for one level of a factor.
#'
#' @param offset,mult Additive and multiplicate offsets used to adjust the
#'   band size. For example, use \code{mult = 0.9} to make a bar take up
#'   90\% of the space allocated for its category.
#' @export
#' @examples
#' df <- data.frame(label = c("a", "b", "c"), n = c(10, 9, 4))
#'
#' base <- df %>% ggvis(~label, y2 = 0, y := ~n)
#' base %>% layer_rects(width := band())
#' base %>% layer_rects(width := band(offset = -1))
#' base %>% layer_rects(width := band(mult = 0.9))
band <- function(offset = NULL, mult = NULL) {
  structure(
    list(type = "band", offset = offset, mult = mult, scale = FALSE),
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
#' @param x object to test for band-ness
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
