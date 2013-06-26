#' Split data by group
#'
#' @param ... Variables to split on.
#' @export
#' @examples
#' pl <- pipeline(mtcars, by_group(variable(quote(cyl))), transform_bin())
#' flow(pl, props(x ~ disp))
by_group <- function(...) {
  variables <- list(...)
  stopifnot(all(vapply(variables, is.variable, logical(1))))

  pipe(c("split_by_group", "split"), variables = variables)
}

#' @S3method format split_by_group
format.split_by_group <- function(x, ...) {
  paste0(" -> split_by", param_string(x))
}

#' @S3method flow split_by_group
flow.split_by_group <- function(x, props, data) {
  split_df(data, x$variables)
}

#' @S3method pipe_id split
pipe_id.split <- function(x, props) {
  paste("split", digest(x, algo = "crc32"), sep = "_")
}


# Given a data object like a data frame or split_df, return the variables that
# the data is split on.
split_vars <- function(x) UseMethod("split_vars")

#' @S3method split_vars default
split_vars.default <- function(x) NULL

#' @S3method split_vars split_by_group
split_vars.split_by_group <- function(x) x$variables
