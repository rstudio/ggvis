# Find the range of a particular variable in a data object, for discrete and
# continuous variables.
# @param data The data object.
# @param var The variable (column) to find the range of.
# @param drop For factors, if drop==FALSE, then return all levels, even those
#   that aren't use; if drop==TRUE, then return only the levels that actually
#   are present.
find_var_range <- function(data, var, drop = FALSE) UseMethod("find_var_range")

#' @S3method find_var_range data.frame
find_var_range.data.frame <- function(data, var, drop = FALSE) {
  col <- data[[var]]
  if (is.numeric(col))
    range(col)
  else if (is.factor(col) && !drop)
    levels(col)
  else
    unique(col)
}

#' @S3method find_var_range split_df
find_var_range.split_df <- function(data, var, drop = FALSE) {
  # Find range of each data frame in the list
  ranges <- lapply(data, find_var_range, var = var, drop = drop)

  # Verify that all ranges are the same type
  types <- vapply(data, function(d) mode(d[[var]]), FUN.VALUE = character(1))
  if (!is_constant(types))
    stop("Data frames in split_df object do not all have same type for column '",
      var, "'.")

  if (types[1] == "numeric") {
    range(unlist(ranges))
  } else if (types[1] == "character") {
    unique(unlist(ranges))
  } else {
    stop("Unknown column type: ", types[1])
  }
}
