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
#' base <- df %>% ggvis(~label, y2 = 0, y = ~n)
#' base %>% layer_rects(width = band())
#' base %>% layer_rects(width = band(offset = -1))
#' base %>% layer_rects(width = band(mult = 0.9))
#'
#' # A nominal scale with padding is more symmetrical than band with a mult
#' base %>% layer_rects(width = band(mult = 0.75))
#' base %>% layer_rects(width = band()) %>%
#'   scale_nominal("x", padding = 0.25, points = FALSE)
band <- function(offset = NULL, mult = NULL) {
  structure(
    list(offset = offset, mult = mult),
    class = c("band")
  )
}

#' @export
new_prop.band <- function(x, property, scale, offset, mult, env, event,
                          label) {
  if (!(property %in% c("width", "height"))) {
    stop("band() can only be used for width and height properties.")
  }
  if (identical(scale, FALSE)) stop("band() must be scaled.")

  structure(
    list(
      property = property,
      scale = decide_scale(scale %||% TRUE, property),
      offset = offset,
      mult = mult,
      event = event,
      env = NULL
    ),
    class = c("prop_band", "prop")
  )
}

#' @export
as.character.prop_band <- function(x, ...) ""

#' @rdname band
#' @param x object to test for band-ness
is.prop_band <- function(x) inherits(x, "prop_band")

#' @export
prop_value.prop_band <- function(x, data) {
  NULL
}

#' @export
prop_label.prop_band <- function(x) {
  ""
}

#' @export
prop_domain.prop_band <- function(x, data) {
  NULL
}

#' @export
prop_vega.prop_band <- function(x, default_scale) {
  compact(list(
    scale = x$scale,
    mult = x$mult,
    offset = x$offset,
    band = TRUE
  ))
}
