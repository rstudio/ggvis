#' Count data at each location of a continuous variable
#'
#' @param x Dataset-like object to count. Built-in methods for data frames,
#'   grouped data frames and ggvis visualisations.
#' @param x_var,w_var Names of x and weight variables.
#' @seealso \code{\link{compute_bin}} For counting cases within ranges of
#'   a continuous variable.
#' @seealso \code{\link{compute_tabulate}} For counting cases at each value
#'   of a categorical variable.
#' @export
#' @return A data frame with columns:
#'  \item{count_}{the number of points}
#'  \item{x_}{mid-point of bin}
#'  \item{xmin_}{left boundary of bin}
#'  \item{xmax_}{right boundary of bin}
#'  \item{width_}{width of bin}
#'
#' The width of each "bin" is set to the resolution of the data -- that is, the
#' smallest difference between two x values.
#'
#' @examples
#' mtcars %>% compute_count(~cyl)
#'
#' # Weight the counts by car weight value
#' mtcars %>% compute_count(~cyl, ~wt)
#'
#' # If there's one weight value at each x, it effectively just renames columns.
#' pressure %>% compute_count(~temperature, ~pressure)
#'
#' # It doesn't matter whether you transform inside or outside of a vis
#' mtcars %>% compute_count(~cyl, ~wt) %>%
#'   ggvis(x = ~xmin_, x2 = ~xmax_, y = ~count_, y2 = 0) %>%
#'   layer_rects() %>%
#'   set_dscale("y", "numeric", domain = c(0, 60))
#' # FIXME: Add support for domainMin and domainMax
#'
#' mtcars %>%
#'   ggvis(x = ~xmin_, x2 = ~xmax_, y = ~count_, y2 = 0) %>%
#'   compute_count(~cyl, ~wt) %>%
#'   layer_rects() %>%
#'   set_dscale("y", "numeric", domain = c(0, 60))
compute_count <- function(x, x_var, w_var = NULL) {
  UseMethod("compute_count")
}

#' @export
compute_count.data.frame <- function(x, x_var, w_var = NULL) {
  assert_that(is.formula(x_var))

  x_val <- eval_vector(x, x_var)
  if (vector_countable(x_val)) {
    stop("compute_count requires continuous data.")
  }

  if (is.null(w_var)) {
    w_val <- NULL
  } else {
    w_val <- eval_vector(x, w_var)
  }

  count_vector(x_val, weight = w_val)
}

#' @export
compute_count.grouped_df <- function(x, x_var, w_var = NULL) {
  dplyr::do(x, compute_count(., x_var, w_var = w_var))
}

#' @export
compute_count.ggvis <- function(x, x_var, w_var = NULL) {
  args <- list(x_var = x_var, w_var = w_var)

  register_computation(x, args, "count", function(data, args) {
    output <- do_call(compute_count, quote(data), .args = args)
    preserve_constants(data, output)
  })
}

# Count individual vector ------------------------------------------------------

count_vector <- function(x, weight = NULL, ...) {
  res <- tabulate_vector(x, weight, ...)
  bin_out(res$count_, res$x_, width = resolution(res$x_))
}
