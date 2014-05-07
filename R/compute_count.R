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
#' @examples
#' mtcars %>% compute_count(~cyl)
#'
#' # Weight by car weight
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
  if (is.null(weight)) {
    weight <- rep.int(1, length(x))
  }
  counts <- unname(tapply(weight, x, sum, na.rm = TRUE))
  # Need to get unique values this way instead of using names(counts), because names
  # are strings but the x values aren't always strings.
  values <- unname(tapply(x, x, unique, na.rm = TRUE))

  width <- resolution(values)

  data.frame(
    count_ = counts,
    x_ = values,
    xmin_ = values - width/2,
    xmax_ = values + width/2,
    width_ = width,
    stringsAsFactors = FALSE
  )
}

bin_out <- function(count = numeric(0), x = numeric(0), width = numeric(0),
                    xmin = x - width / 2, xmax = x + width / 2) {
  data.frame(
    count_ = count,
    x_ = x,
    xmin_ = xmin,
    xmax_ = xmax,
    width_ = width,
    stringsAsFactors = FALSE
  )
}
