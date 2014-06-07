#' Transformation: density estimate
#'
#' \code{transform_density} is a data transformation that computes a kernel
#' density estimate from a dataset. \code{layer_density} combines
#' \code{transform_density} with \code{mark_path} and \code{mark_area}
#' to display a smooth line and its standard errror.
#'
#' @param vis The visualisation to modify
#' @param ... Visual properties, passed on to \code{\link{props}}.
#' @param adjust Multiple the default bandwidth by this amount. Useful for
#'   controlling wiggliness of density.
#' @inheritParams compute_density
#' @param density_args Other arguments passed on to
#'   \code{\link{compute_density}} and thence to \code{\link{density}}.
#' @param area Should there be a shaded region drawn under the curve?
#' @export
#' @examples
#' # Basic density estimate
#' faithful %>% ggvis(~waiting) %>% layer_densities()
#' faithful %>% ggvis(~waiting) %>% layer_densities(area = FALSE)
#'
#' # Control bandwidth with adjust
#' faithful %>% ggvis(~waiting) %>% layer_densities(adjust = .25)
#' faithful %>% ggvis(~waiting) %>%
#'   layer_densities(adjust = input_slider(0.1, 5))
#'
#' # Control stroke and fill
#' faithful %>% ggvis(~waiting) %>%
#'   layer_densities(stroke := "red", fill := "red")
#'
#' # With groups
#' PlantGrowth %>% ggvis(~weight, fill = ~group) %>% group_by(group) %>%
#'   layer_densities()
#' PlantGrowth %>% ggvis(~weight, stroke = ~group) %>% group_by(group) %>%
#'   layer_densities(strokeWidth := 3, area = FALSE)
layer_densities <- function(vis, ..., kernel = "gaussian", adjust = 1,
                            density_args = list(), area = TRUE) {

  x_var <- find_prop_var(cur_props(vis), "x.update")

  vis <- set_scale_label(vis, "x", prop_label(cur_props(vis)$x.update))
  vis <- set_scale_label(vis, "y", "density")

  props <- stroke_fill_defaults(props(...),
    stroke = props(~pred_, ~resp_),
    fill   = props(~pred_, ~resp_, y2 = 0, fillOpacity := 0.2)
  )

  pipeline <- function(x) {
    x <- do_call(compute_density, quote(x), x_var = x_var, kernel = kernel,
      adjust = adjust, .args = density_args)

    if (identical(area, TRUE)) {
      x <- emit_ribbons(x, props$fill)
    }
    x <- emit_paths(x, props$stroke)
    x
  }
  layer_f(vis, pipeline)
}
