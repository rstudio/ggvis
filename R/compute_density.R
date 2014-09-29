#' Compute density of data.
#'
#' @param x Dataset (data frame, \code{grouped_df} or ggvis) object to work
#'   with.
#' @param x_var,w_var Names of variables to use for x position, and for
#'   weights.
#' @param kernel Smoothing kernel. See \code{\link{density}} for details.
#' @param trim If \code{TRUE}, the default, density estimates are trimmed to the
#'   actual range of the data.  If \code{FALSE}, they are extended by the
#'   default 3 bandwidths (as specified by the \code{cut} parameter to
#'   \code{\link{density}}).
#' @param ... Additional arguments passed on to \code{\link{density}}.
#' @param n Number of points (along x) to use in the density estimate.
#' @param na.rm If \code{TRUE} missing values will be silently removed,
#'   otherwise they will be removed with a warning.
#' @return A data frame with columns:
#'  \item{pred_}{regularly spaced grid of \code{n} locations}
#'  \item{resp_}{density estimate}
#' @export
#' @examples
#' mtcars %>% compute_density(~mpg, n = 5)
#' mtcars %>% group_by(cyl) %>% compute_density(~mpg, n = 5)
#' mtcars %>% ggvis(~mpg) %>% compute_density(~mpg, n = 5) %>%
#'   layer_points(~pred_, ~resp_)
compute_density <- function(x, x_var, w_var = NULL, kernel = "gaussian",
                            trim = FALSE, n = 256L, na.rm = FALSE, ...) {

  UseMethod("compute_density")
}

#' @export
compute_density.data.frame <- function(x, x_var, w_var = NULL,
                                       kernel = "gaussian",
                                       trim = FALSE, n = 256L,
                                       na.rm = FALSE, ...) {

  assert_that(is.formula(x_var))

  # Extract variables from data frame
  x_val <- eval_vector(x, x_var)

  # Special case zero-row input
  if (length(x_val) == 0) {
    return(data.frame(pred_ = numeric(0), resp_ = numeric(0)))
  }

  if (is.null(w_var)) {
    w_val <- NULL
  } else {
    w_val <- eval_vector(x, w_var)
  }

  # Build call to density()
  call <- make_call("density", quote(x_val), weights = quote(w_val),
    kernel = kernel, n = n, na.rm = na.rm, ...)

  if (trim) {
    call$from <- min(x_val)
    call$to   <- max(x_val)
  }
  dens <- eval(call)

  # Standardise output
  data.frame(pred_ = dens$x, resp_ = dens$y)
}

#' @export
compute_density.grouped_df <- function(x, x_var, w_var = NULL,
                                       kernel = "gaussian", trim = FALSE,
                                       n = 256L, na.rm = FALSE, ...) {
  dplyr::do(x, compute_density(., x_var = x_var, w_var = w_var,
    kernel = kernel, trim = trim, n = n, na.rm = na.rm, ...))
}

#' @export
compute_density.ggvis <- function(x, x_var, w_var = NULL,
                                  kernel = "gaussian", trim = FALSE,
                                  n = 256L, na.rm = FALSE, ...) {
  args <- list(x_var = x_var, w_var = w_var, kernel = kernel,
    trim = trim, n = n, na.rm = na.rm, ...)

  register_computation(x, args, "density", function(data, args) {
    output <- do_call(compute_density, quote(data), .args = args)
    preserve_constants(data, output)
  })
}
