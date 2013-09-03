#' Transformation: sort the data
#' @export
transform_sort <- function(...) {
  # Drop unnamed arguments
  dots <- list(...)
  dots <- dots[named(dots)]

  transform("sort", dots = dots)
}

#' @S3method format transform_sort
format.transform_sort <- function(x, ...) {
  paste0(" -> sort()")
}

#' @S3method compute transform_sort
compute.transform_sort <- function(x, props, data) {
  check_prop(x, props, data, "x", "numeric")

  output <- sort(data, x_var = props$x)
  preserve_constants(data, output)
}

sort <- function(data, x_var) UseMethod("sort")

#' @S3method sort split_df
sort.split_df <- function(data, x_var) {
  data[] <- lapply(data, sort, x_var = x_var)
  data
}

#' @S3method sort data.frame
sort.data.frame <- function(data, x_var) {
  data[order(prop_value(x_var, data)), ]
}
