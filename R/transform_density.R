#' Transformation: density estimate
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
transform_density <- function(..., adjust = 1, kernel = "gaussian",
                             trim = FALSE, n = 200L, na.rm = FALSE) {
  # Drop unnamed arguments
  dots <- list(...)
  dots <- dots[named(dots)]

  transform("density", adjust = adjust, kernel = kernel, trim = trim, n = n,
    na.rm = na.rm, dots = dots)
}

#' @export
#' @param area Should there be a shaded region drawn under the curve?
#' @param ... Named arguments are passed on to the transform, unnamed
#'   arguments are passed on to the branch.
branch_density <- function(area = TRUE, ...) {
  comps <- parse_components(..., drop_named = TRUE)

  line_props <- merge_props(props(x = ~x, y = ~y), comps$props)
  area_props <- merge_props(props(x = ~x, y = ~y, y2 = 0, fillOpacity := 0.2),
    comps$props)

  # Line shouldn't get fill-related props, and area shouldn't get
  # stroke-related props.
  line_props <- line_props[setdiff(names(line_props), c("fill", "fillOpacity"))]
  area_props <- area_props[setdiff(names(area_props), c("stroke", "strokeOpacity"))]

  branch(
    transform_density(...),
    branch(
      comps$data,
      comps$marks,
      if (area) mark_area(area_props),
      mark_line(line_props)
    )
  )
}

#' @S3method format transform_density
format.transform_density <- function(x, ...) {
  paste0(" -> density()", param_string(x[c("adjust", "kernel")]))
}

#' @S3method compute transform_density
compute.transform_density <- function(x, props, data) {
  check_prop(x, props, data, "x", "numeric")

  output <- compute_density(data, x, x_var = props$x, y_var = props$y)
  preserve_constants(data, output)
}

compute_density <- function(data, trans, x_var, y_var) UseMethod("compute_density")

#' @S3method compute_density split_df
compute_density.split_df <- function(data, trans, x_var, y_var) {
  data[] <- lapply(data, compute_density, trans = trans, x_var = x_var,
    y_var = y_var)
  data
}

#' @S3method compute_density data.frame
compute_density.data.frame <- function(data, trans, x_var, y_var) {
  assert_that(is.numeric(trans$adjust), length(trans$adjust) == 1)
  assert_that(length(trans$n) == 1, trans$n >= 0)
  assert_that(is.flag(trans$na.rm))

  x_vals <- prop_value(x_var, data)

  missing <- is.na(x_vals)
  x_vals <- x_vals[!missing]
  if (any(missing) && !trans$na.rm) {
    warning("Removed ", sum(missing), " rows containing missing values.")
  }

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
