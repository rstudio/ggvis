#' Groupwise property
#'
#' \code{groupwise()} is used to refer to properties of the plot or
#' subvisualisation, typically the height or the width.
#' @param prop Name of the group property. Typically "height" or "width".
#' @param offset,mult Additive and multiplicate offsets.
#' @export
#' @examples
#' # In the following example, we draw a rectangle running vertically
#' # across the plot by referring to its total height
#' ggvis(faithful) %>%
#'   layer_histograms(~eruptions) %>%
#'   emit_rects(props(
#'     x = ~mean(eruptions), width := 40,
#'     y := 0, y2 := groupwise("height"),
#'     fill := "red"
#'   ))
#'
#' # The mult factor is useful to draw elements at fixed fractions
#' # of the plot:
#' ggvis(mtcars, ~mpg) %>%
#'   layer_histograms() %>%
#'   layer_axial_lines(y := groupwise("height", mult = 0.5),
#'     stroke := "red", strokeWidth := 4)
groupwise <- function(prop, offset = NULL, mult = NULL) {
  structure(
    list(group_prop = prop, offset = offset, mult = mult),
    class = c("groupwise")
  )
}

#' @export
new_prop.groupwise <- function(x, property, scale, offset, mult, env, event,
                               label) {
  structure(
    list(
      property = property,
      scale = decide_scale(scale %||% FALSE, property),
      offset = x$offset,
      mult = x$mult,
      event = event,
      env = NULL,
      group_prop = x$group_prop
    ),
    class = c("prop_group", "prop")
  )
}

#' @export
as.character.prop_group <- function(x, ...) ""

#' @rdname groupwise
#' @param x object to test for group-ness
is.prop_group <- function(x) inherits(x, "prop_group")

#' @export
prop_value.prop_group <- function(x, data) {
  NULL
}

#' @export
prop_label.prop_group <- function(x) {
  ""
}

#' @export
prop_domain.prop_group <- function(x, data) {
  NULL
}

#' @export
prop_vega.prop_group <- function(x, default_scale) {
  compact(list(
    scale = x$scale,
    mult = x$mult,
    offset = x$offset,
    group = x$group_prop
  ))
}
