#' Transformation: density estimate
#'
#' \code{transform_density} is a data transformation that computes a kernel
#' density estimate from a dataset. \code{layer_density} combines
#' \code{transform_density} with \code{mark_path} and \code{mark_area}
#' to display a smooth line and its standard errror.
#'
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
#'   layer_densities(
#'      stroke := "#cc3333",
#'      strokeWidth := 3,
#'      fill := "#666699",
#'      fillOpacity := 0.6
#'    )
#'
#' # With groups
#' PlantGrowth %>% ggvis(~weight, stroke = ~group) %>% group_by(group) %>%
#'   layer_densities()
layer_densities <- function(vis, ..., kernel = "gaussian", adjust = 1,
                                    density_args = list(), area = TRUE) {

  x_var <- as.character(vis$cur_props$x.update$value)

  # Line shouldn't get fill-related props, and area shouldn't get
  # stroke-related props.
  props <- props(...)
  line_props <- merge_props(props(x = ~pred_, y = ~resp_), props)
  line_props <- drop_props(line_props, c("fill", "fillOpacity"))

  area_props <- merge_props(props(x = ~pred_, y = ~resp_, y2 = 0, fillOpacity := 0.2),
    props)
  area_props <- drop_props(area_props, c("stroke", "strokeOpacity"))

  pipeline <- function(x) {
    x <- do_call("compute_density", quote(x), x_var = x_var, kernel = kernel,
      adjust = adjust, .args = density_args)

    if (identical(area, TRUE)) {
      x <- emit_ribbons(x, area_props)
    }
    x <- emit_paths(x, line_props)
    x
  }
  branch_f(vis, pipeline)
}

#' Compute density of data.
#'
#' @return A data frame with columns:
#'  \item \code{pred_}: regularly spaced grid of \code{n} locations
#'  \item \code{resp_}: density estimate
#' @param kernel Smoothing kernel. See \code{\link{density}} for details.
#' @param trim If \code{TRUE}, the default, density estimates are trimmed to the
#'   actual range of the data.  If \code{FALSE}, they are extended by the
#'   default 3 bandwidths (as specified by the \code{cut} parameter to
#'   \code{\link{density}}).
#' @param n Number of points (along x) to use in the density estimate.
#' @param na.rm If \code{TRUE} missing values will be silently removed,
#'   otherwise they will be removed with a warning.
#' @examples
#' mtcars %>% compute_density("mpg", n = 5)
#' mtcars %>% group_by(cyl) %>% compute_density("mpg", n = 5)
#' mtcars %>% ggvis(~mpg) %>% compute_density("mpg", n = 5)
compute_density <- function(x, x_var, weight_var = NULL, kernel = "gaussian",
                            trim = FALSE, n = 256L, na.rm = FALSE, ...) {

  UseMethod("compute_density")
}

#' @export
compute_density.data.frame <- function(x, x_var, weight_var = NULL,
                                       kernel = "gaussian",
                                       trim = FALSE, n = 256L,
                                       na.rm = FALSE, ...) {

  # Extract variables from data frame
  x_vals <- x[[x_var]]
  if (!is.null(weight_var)) {
    w_vals <- x[[weight_var]]
  } else {
    w_vals <- NULL
  }

  # Build call to density()
  call <- make_call("density", quote(x_vals), weights = quote(w_vals),
    kernel = kernel, n = n, na.rm = na.rm, ...)

  if (trim) {
    call$from <- min(x_vals)
    call$to   <- max(x_vals)
  }
  dens <- eval(call)

  # Standardise output
  data.frame(pred_ = dens$x, resp_ = dens$y)
}

#' @export
compute_density.grouped_df <- function(x, x_var, weight_var = NULL,
                                       kernel = "gaussian", trim = FALSE,
                                       n = 256L, na.rm = FALSE, ...) {
  dplyr::do(x, compute_density(., x_var = x_var, weight_var = weight_var,
    kernel = kernel, trim = trim, n = n, na.rm = na.rm, ...))
}

#' @export
compute_density.ggvis <- function(x, x_var, weight_var = NULL,
                                  kernel = "gaussian", trim = FALSE,
                                  n = 256L, na.rm = FALSE, ...) {
  args <- list(x_var = x_var, weight_var = weight_var, kernel = kernel,
    trim = trim, n = n, na.rm = na.rm, ...)
  x <- register_reactives(x, args)

  new_data <- reactive({
    data <- x$cur_data()
    output <- do_call("compute_density", quote(data), .args = values(args))
    preserve_constants(data, output)
  })

  register_data(x, new_data, "compute_density")
}
