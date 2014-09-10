#' Align positions using length.
#'
#' This compute function is often used in conjunction with
#' \code{\link{compute_count}}, when used on data with a continuous x variable.
#' By default, the computed width will be equal to the resolution of the data,
#' or, in other words the smallest difference between two values in the data.
#'
#' An absolute width for each x can be specified by using the \code{width}
#' argument. If \code{width} is NULL (the default), it will use the resolution
#' of the data as the width.
#'
#' @param x Dataset-like object to align. Built-in methods for data frames,
#'   grouped data frames and ggvis visualisations.
#' @param var Name of variable to compute width of.
#' @param length An absolute length to use. If \code{NULL} (the default), the
#'   width will be equivalent to the resolution of the data.
#' @param dir Direction, i.e. \code{"x"} or \code{"y"}. Used to generate
#'   variable names in output.
#' @param align Where does the existing variable fall on the new bins?
#'   0 = left edge, 0.5 = center, 1 = right edge.
#' @seealso \code{\link{compute_bin}} For counting cases within ranges of
#'   a continuous variable.
#' @seealso \code{\link{compute_count}} For counting cases at specific values
#'   of a variable.
#' @export
#' @return The original data frame, with additional columns:
#'  \item{'dir'min_}{left boundary of bin}
#'  \item{'dir'max_}{right boundary of bin}
#'  \item{'dir'len_}{width of bin}
#'
#' @examples
#' mtcars %>% compute_count(~disp) %>% compute_align(~x_)
#' mtcars %>% compute_count(~mpg) %>% compute_align(~x_)
#'
#' # Use a specific width
#' pressure %>% compute_count(~temperature) %>% compute_align(~x_)
#' pressure %>% compute_count(~temperature) %>% compute_align(~x_, length = 5)
#'
#' # It doesn't matter whether you transform inside or outside of a vis
#' mtcars %>% compute_count(~cyl, ~wt) %>%
#'   compute_align(~x_, length = .5) %>%
#'   ggvis(x = ~xmin_, x2 = ~xmax_, y = ~count_, y2 = 0) %>%
#'   layer_rects()
#'
#' mtcars %>%
#'   ggvis(x = ~xmin_, x2 = ~xmax_, y = ~count_, y2 = 0) %>%
#'   compute_count(~cyl, ~wt) %>%
#'   compute_align(~x_) %>%
#'   layer_rects()
#'
#' # Varying align
#' mtcars %>%
#'   ggvis(x = ~xmin_, x2 = ~xmax_, y = ~count_, y2 = 0) %>%
#'   compute_count(~cyl, ~wt) %>%
#'   compute_align(~x_, length = 0.5, align = input_slider(0, 1)) %>%
#'   layer_rects()
compute_align <- function(x, var, length = NULL, align = 0.5, dir = "x") {
  UseMethod("compute_align")
}

#' @export
compute_align.data.frame <- function(x, var, length = NULL, align = 0.5,
                                     dir = "x") {
  assert_that(is.formula(var))

  val <- eval_vector(x, var)
  if (vector_countable(val)) {
    stop("compute_align requires continuous data.")
  }

  if (is.null(length)) {
    length <- resolution(val)
  }

  x[paste0(dir, "min_")] <- val - length * align
  x[paste0(dir, "max_")] <- val + length * (1 - align)
  x[paste0(dir, "len_")] <- length

  x
}

#' @export
compute_align.ggvis <- function(x, var, length = NULL, align = 0.5, dir = "x") {
  args <- list(var = var, length = length, align = align, dir = dir)

  register_computation(x, args, "align", function(data, args) {
    output <- do_call(compute_align, quote(data), .args = args)
    preserve_constants(data, output)
  })
}
