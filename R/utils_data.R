merge_df <- function(a, b) {
  if (is.null(a) || nrow(a) == 0 || ncol(a) == 0) return(b)
  if (is.null(b) || nrow(b) == 0 || ncol(b) == 0) return(a)

  cbind(a[setdiff(names(a), names(b))], b)
}

# Convenience function to remove missing values from a data.frame
# Remove all non-complete rows, with a warning if \code{warn_na = FALSE}.
#
# @param x An input data frame or split_df
# @param warn_na Warn when rows are removed?
# @param finite When FALSE, remove NA and NaN. When TRUE, also remove Inf and -Inf.
# @param vars Columns to check for missing values (for data.frame and split_df)
remove_missing <- function(x, warn_na = TRUE, finite = FALSE, ...) {
  UseMethod("remove_missing")
}

#' @S3method remove_missing default
remove_missing.default <- function(x, warn_na = TRUE, finite = FALSE) {
  if (!is.atomic(x)) {
    stop("Input to remove_missing.default must be a vector.")
  }

  if (finite) {
    missing <- !is.finite(x) | is.na(x)
    str <- "non-finite"
  } else {
    missing <- is.na(x)
    str <- "missing"
  }

  if (any(missing)) {
    x <- x[!missing]
    if (warn_na) warning("Removed ", sum(missing), " rows containing ", str,
      " values", ".", call. = FALSE)
  }
  x
}

#' @S3method remove_missing data.frame
remove_missing.data.frame <- function(x, warn_na = TRUE, finite = FALSE,
    vars = names(x)) {

  vars <- intersect(vars, names(x))

  if (finite) {
    missing <- !finite.cases(x[, vars, drop = FALSE])
    str <- "non-finite"
  } else {
    missing <- !complete.cases(x[, vars, drop = FALSE])
    str <- "missing"
  }

  if (any(missing)) {
    x <- x[!missing, , drop = FALSE]
    if (warn_na) warning("Removed ", sum(missing), " rows containing ", str,
      " values", ".", call. = FALSE)
  }
  x
}

#' @S3method remove_missing split_df
remove_missing.split_df <- function(x, warn_na = TRUE, finite = FALSE,
    vars = names(x)) {
  x[] <- lapply(x, remove_missing, warn_na, vars, finite)
  x
}


# Returns a logical vector of same length as nrow(x). If all data on a row
# is finite (not NA, NaN, Inf, or -Inf) return TRUE; otherwise FALSE.
finite.cases <- function(x) {
  finite_cases <- vapply(x, is.finite, logical(nrow(x)))

  # Need a special case test when x has exactly one row, because rowSums
  # doesn't respect dimensions for 1x1 matrices. vapply returns a vector (not
  # a matrix when the input has one row.
  if (is.vector(finite_cases)) {
    all(finite_cases)
  } else {
    # Find all the rows where all are TRUE
    rowSums(as.matrix(finite_cases)) == ncol(x)
  }
}
