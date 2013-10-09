#' Transformation: sort the data
#'
#' \code{transform_sort} is a data transformation that sorts a data object based
#' on one or more columns in the data.
#'
#' @section Input:
#' \code{transform_sort} takes a data frame or a split_df as input.
#'
#' @section Ouput:
#'
#' \code{transform_sort} returns a sorted data frame or split_df with the same
#'   columns as the input. In the case of a split_df, each of the data frames
#'   contained within is sorted.
#'
#' @param var The variables to sort on. This is the variable name after mapping.
#'   For example, with \code{props(x = ~mpg)}, you would use \code{"x"}, not
#'   \code{"mpg"}. Multiple variables can be used, as in \code{c("x", "y")}.
#' @param ... Named arguments, which are passed along to the \code{order()}
#'   function used for sorting. Unnamed arguments will be dropped.
#' @examples
#'
#' # Compare the output from the unsorted vs. sorted data
#' # Unsorted
#' ggvis(mtcars,
#'   props(x = ~wt, y = ~mpg),
#'   mark_line(),
#'   mark_symbol(props(fill := NA))
#' )
#'
#' # Sorted
#' ggvis(mtcars,
#'   transform_sort(),
#'   props(x = ~wt, y = ~mpg),
#'   mark_line(),
#'   mark_symbol(props(fill := NA))
#' )
#'
#' # With grouping
#' ggvis(mtcars,
#'   by_group(factor(cyl)),
#'   transform_sort(),
#'   props(x = ~wt, y = ~mpg, stroke = ~factor(cyl)),
#'   mark_line(),
#'   mark_symbol(props(fill := NA))
#' )
#'
#' # Sort on mpg column
#' sluice(pipeline(mtcars, transform_sort()), props(x = ~mpg))
#' # Same effect, but this time mpg is mapped to y
#' sluice(pipeline(mtcars, transform_sort(var = "y")), props(y = ~mpg))
#'
#' # Sort on multiple columns
#' sluice(pipeline(mtcars, transform_sort(var = c("x", "y"))),
#'   props(x = ~cyl, y = ~mpg))
#'
#' # Use `decreasing` argument, which passed along to order()
#' sluice(pipeline(mtcars, transform_sort(var = "x", decreasing = TRUE)),
#'   props(x = ~mpg))
#'
#' # Sort on a calculated column (mpg mod 10)
#' sluice(pipeline(mtcars, transform_sort()), props(x = ~mpg %% 10) )
#'
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
  prop_names <- paste0(x$var, ".update")
  vapply(prop_names,
    function(prop_name) check_prop(x, props, data, prop_name),
    logical(1)
  )

  output <- compute_sort(data, x, var = props[prop_names])
  preserve_constants(data, output)
}

compute_sort <- function(data, trans, vars) UseMethod("compute_sort")

#' @S3method compute_sort split_df
compute_sort.split_df <- function(data, trans, vars) {
  data[] <- lapply(data, compute_sort, trans = trans, vars = vars)
  data
}

#' @S3method compute_sort data.frame
compute_sort.data.frame <- function(data, trans, vars) {
  cols <- lapply(vars, prop_value, data)
  idx <- do.call(order, args = c(cols, trans$dots))
  data[idx, ]
}
