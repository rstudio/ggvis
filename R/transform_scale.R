#' Transform scale: a simple transform for testing
#'
#' @param add constant to add to x
#' @param mult constant to multiply x by
#' @export
#' @keywords internal
#' @examples
#' neg <- transform_scale(mult = -1)
#' neg
#' compute(neg, props(x ~ mpg, y ~ disp), mtcars)
transform_scale <- function(add = 0, mult = 1) {
  transform("scale", add = add, mult = mult)
}

#' @S3method format transform_scale
format.transform_scale <- function(x, ...) {
  paste0(" -> scale", param_string(x[c("add", "mult")]))
}

#' @S3method compute transform_scale
compute.transform_scale <- function(x, props, data) {
  x_val <- prop_value(props$x, data)
  y_val <- prop_value(props$y, data)

  assert_that(is.numeric(x_val))
  assert_that(is.numeric(y_val))
  assert_that(is.numeric(x$add), length(x$add) == 1)
  assert_that(is.numeric(x$mult), length(x$mult) == 1)

  data.frame(x = x_val * x$mult + x$add, y = y_val)
}
