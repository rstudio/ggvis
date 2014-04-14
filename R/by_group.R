# Given a data object like a data frame or split_df, return the variables that
# the data is split on.
split_vars <- function(x) UseMethod("split_vars")

#' @export
split_vars.default <- function(x) NULL

#' @export
split_vars.split_by_group <- function(x) x$variables
