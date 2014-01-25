#' Transformation: density estimate
#'
#' \code{transform_density} is a data transformation that computes a kernel
#' density estimate from a dataset. \code{layer_density} combines
#' \code{transform_density} with \code{mark_line} and \code{mark_area}
#' to display a smooth line and its standard errror.
#'
#' @section Input:
#' Currently \code{transform_density} only works with data frames and requires
#' the following properties:
#'
#' \itemize{
#'   \item \code{x}, numeric, horizontal position
#' }
#'
#' @section Ouput:
#'
#' \code{transform_density} creates a data frame with columns:
#'
#' \itemize{
#'  \item \code{x}: regularly spaced grid of \code{n} locations
#'  \item \code{y}: density estimate
#' }
#'
#' @param adjust Bandwidth adjustment. See \code{\link{density}} for details.
#' @param kernel Smoothing kernel. See \code{\link{density}} for details.
#' @param trim If \code{TRUE}, the default, density estimates are trimmed to the
#'   actual range of the data.  If \code{FALSE}, they are extended by the
#'   default 3 bandwidths (as specified by the \code{cut} parameter to
#'   \code{\link{density}}).
#' @param n Number of points (along x) to use in the density estimate.
#' @param na.rm If \code{TRUE} missing values will be silently removed,
#'   otherwise they will be removed with a warning.
#' @export
#' @examples
#' # Basic density estimate
#' ggvis(faithful, props(x = ~waiting), layer_density())
#'
#' # Smaller bandwidth
#' ggvis(faithful, props(x = ~waiting, fill :="lightblue"),
#'   layer_density(adjust = .25)
#' )
#'
#' # Control stroke and fill
#' ggvis(faithful, props(x = ~waiting),
#'   layer_density(props(stroke := "#cc3333", strokeWidth := 3,
#'     fill := "#666699", fillOpacity := 0.6))
#' )
#'
#' # With groups
#' ggvis(PlantGrowth, by_group(group),
#'   props(x = ~weight, stroke = ~group, fill = ~group, fillOpacity := 0.2),
#'   layer_density()
#' )
#'
#' # Using various arguments: adjust na.rm, n, area, kernel
#' mtc <- mtcars
#' mtc$mpg[5:10] <- NA
#' ggvis(mtc,
#'   props(x = ~mpg, y = ~mpg),
#'   layer_density(adjust = 0.3, n = 100, area = FALSE, kernel = "rectangular",
#'     props(stroke := "#cc0000"))
#' )
#'
#'
transform_density <- function(..., adjust = 1, kernel = "gaussian",
                             trim = FALSE, n = 200L, na.rm = FALSE) {
  # Drop unnamed arguments
  dots <- list(...)
  dots <- dots[named(dots)]

  transform("density", adjust = adjust, kernel = kernel, trim = trim, n = n,
    na.rm = na.rm, dots = dots)
}

#' @rdname transform_density
#' @export
#' @param area Should there be a shaded region drawn under the curve?
#' @param ... Named arguments are passed on to the transform, unnamed
#'   arguments are passed on to the layer.
layer_density <- function(..., area = TRUE) {
  comps <- parse_components(..., drop_named = TRUE)

  line_props <- merge_props(props(x = ~x, y = ~y), comps$props)
  area_props <- merge_props(props(x = ~x, y = ~y, y2 = 0, fillOpacity := 0.2),
    comps$props)

  # Line shouldn't get fill-related props, and area shouldn't get
  # stroke-related props.
  line_props <- drop_props(line_props, c("fill", "fillOpacity"))
  area_props <- drop_props(area_props, c("stroke", "strokeOpacity"))

  layer(
    transform_density(...),
    layer(
      comps$data,
      comps$marks,
      if (area) mark_area(area_props),
      mark_line(line_props)
    )
  )
}

#' @export
format.transform_density <- function(x, ...) {
  paste0(" -> density()", param_string(x[c("adjust", "kernel")]))
}

#' @export
compute.transform_density <- function(x, props, data) {
  check_prop(x, props, data, "x.update", "numeric")

  output <- compute_density(data, x, x_var = props$x.update, y_var = props$y.update)
  preserve_constants(data, output)
}

compute_density <- function(data, trans, x_var, y_var) UseMethod("compute_density")

#' @export
compute_density.split_df <- function(data, trans, x_var, y_var) {
  data[] <- lapply(data, compute_density, trans = trans, x_var = x_var,
    y_var = y_var)
  data
}

#' @export
compute_density.data.frame <- function(data, trans, x_var, y_var) {
  assert_that(is.numeric(trans$adjust), length(trans$adjust) == 1)
  assert_that(length(trans$n) == 1, trans$n >= 0)
  assert_that(is.flag(trans$na.rm))

  x_vals <- remove_missing(prop_value(x_var, data), warn_na = !trans$na.rm)

  # Build up argument list for density()
  args <- c(list(x_vals, adjust = trans$adjust, kernel = trans$kernel,
    n = trans$n), trans$dots)
  if (trans$trim) {
    args$from <- min(x_vals)
    args$to   <- max(x_vals)
  }

  dens <- do.call(density, args)
  as.data.frame(dens[c("x", "y")])
}
