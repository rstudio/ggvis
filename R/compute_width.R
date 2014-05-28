#' Add "width" of data
#'
#' This compute function is generally used in conjunction with
#' \code{\link{compute_count}}, when used on data with a continuous x variable.
#' By default, the computed width will be equal to the resolution of the data,
#' or, in other words the smallest difference between two values in the data.
#'
#' An absolute width for each x can be specified by using the \code{width}
#' argument. If \code{width} is NULL (the default), it will use the resolution
#' of the data as the width.
#'
#' @param x Dataset-like object to count. Built-in methods for data frames,
#'   grouped data frames and ggvis visualisations.
#' @param x_var Name of variable to compute width of.
#' @param width An absolute width to use. If \code{NULL} (the default), the
#'   width will be equivalent to the resolution of the data.
#' @seealso \code{\link{compute_bin}} For counting cases within ranges of
#'   a continuous variable.
#' @seealso \code{\link{compute_count}} For counting cases at specific values
#'   of a variable.
#' @export
#' @return The original data frame, with additional columns:
#'  \item{xmin_}{left boundary of bin}
#'  \item{xmax_}{right boundary of bin}
#'  \item{width_}{width of bin}
#'
#' The width of each "bin" is set to the resolution of the data -- that is, the
#' smallest difference between two x values.
#'
#' @examples
#' mtcars %>% compute_count(~disp) %>% compute_width(~x_)
#' mtcars %>% compute_count(~mpg) %>% compute_width(~x_)
#'
#' # Use a specific width
#' pressure %>% compute_count(~temperature) %>% compute_width(~x_)
#' pressure %>% compute_count(~temperature) %>% compute_width(~x_, width = 5)
#'
#' # It doesn't matter whether you transform inside or outside of a vis
#' mtcars %>% compute_count(~cyl, ~wt) %>%
#'   compute_width(~x_, width = .5) %>%
#'   ggvis(x = ~xmin_, x2 = ~xmax_, y = ~count_, y2 = 0) %>%
#'   layer_rects() %>%
#'   scale_numeric("y", domain = c(0, NA))
#'
#' mtcars %>%
#'   ggvis(x = ~xmin_, x2 = ~xmax_, y = ~count_, y2 = 0) %>%
#'   compute_count(~cyl, ~wt) %>%
#'   compute_width(~x_) %>%
#'   layer_rects() %>%
#'   scale_numeric("y", domain = c(0, NA))
compute_width <- function(x, x_var, width = NULL) {
  UseMethod("compute_width")
}

#' @export
compute_width.data.frame <- function(x, x_var, width = NULL) {
  assert_that(is.formula(x_var))

  x_val <- eval_vector(x, x_var)
  if (vector_countable(x_val)) {
    stop("compute_width requires continuous data.")
  }

  if (is.null(width)) {
    width <- resolution(x_val)
  }

  # Use mutate instead of cbind so that we don't drop grouped_df class
  dplyr::mutate(x,
    xmin_ = x_val - width/2,
    xmax_ = x_val + width/2,
    width_ = width
  )

}

#' @export
compute_width.grouped_df <- function(x, x_var, width = NULL) {
  # Save groups and remove; mutate needs to happen on ungrouped data
  old_groups <- dplyr::groups(x)
  x <- dplyr::ungroup(x)
  x <- compute_width(x, x_var, width)
  dplyr::regroup(x, old_groups)
}

#' @export
compute_width.ggvis <- function(x, x_var, width = NULL) {
  args <- list(x_var = x_var, width = width)

  register_computation(x, args, "width", function(data, args) {
    output <- do_call(compute_width, quote(data), .args = args)
    preserve_constants(data, output)
  })
}
