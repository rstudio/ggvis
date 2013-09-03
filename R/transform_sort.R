#' Transformation: sort the data
#' @param var The variables to sort on. This is the variable name after mapping.
#'   For example, with \code{props(x = ~ mpg)}, you would use \code{"x"}, not
#'   \code{"mpg"}. Multiple variables can be used, as in \code{c("x", "y")}.
#' @param ... Named arguments for sort function. Unnamed arguments will
#'   be dropped.
#' @examples
#'
#' # Sort on mpg column
#' sluice(pipeline(mtcars, transform_sort()), props(x = ~ mpg))
#' # Same effect, but this time mpg is mapped to y
#' sluice(pipeline(mtcars, transform_sort(var = "y")), props(y = ~ mpg))
#'
#' # Sort on multiple columns
#' sluice(pipeline(mtcars, transform_sort(var = c("x", "y"))),
#'   props(x = ~ cyl, y = ~ mpg))
#' @export
transform_sort <- function(..., var = "x") {
  # Drop unnamed arguments
  dots <- list(...)
  dots <- dots[named(dots)]

  transform("sort", var = var, dots = dots)
}

#' @S3method format transform_sort
format.transform_sort <- function(x, ...) {
  paste0(" -> sort()", param_string(x["var"]))
}

#' @S3method compute transform_sort
compute.transform_sort <- function(x, props, data) {
  good_vars <- x$var %in% names(props)
  if (!all(good_vars)) {
    stop("Variable ", paste(x$var[!good_vars], collapse = ", "),
         " not in the specified list of props.")
  }

  output <- sort(data, var = props[x$var])
  preserve_constants(data, output)
}

sort <- function(data, vars) UseMethod("sort")

#' @S3method sort split_df
sort.split_df <- function(data, vars) {
  data[] <- lapply(data, sort, vars = vars)
  data
}

#' @S3method sort data.frame
sort.data.frame <- function(data, vars) {
  cols <- lapply(vars, prop_value, data)
  idx <- do.call(order, args = cols)
  data[idx, ]
}
