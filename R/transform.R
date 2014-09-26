# Given an two data objects, input and output, this will return output cbind'ed
# with the columns in input that are constant, that is where all the values in
# a column have the same value within each group (if grouped). For any columns
# names that exist in both input and output, the value from output will
# supersede the value from input.
preserve_constants <- function(input, output) UseMethod("preserve_constants")

#' @export
preserve_constants.data.frame <- function(input, output) {
  # Merge data frame 'a' into data frame 'b'. If a and b share any columns, use
  # the column from b. If the second data frame has zero rows, return a zero-row
  # data frame.
  merge_df <- function(a, b) {
    if (is.null(a) || ncol(a) == 0) return(b)
    if (is.null(b) || ncol(b) == 0) return(a)

    a_new <- a[setdiff(names(a), names(b))]
    if (nrow(b) == 0) {
      a_new <- a_new[0, , drop = FALSE]
    }
    cbind(a_new, b)
  }

  is_constant <- constant_vars(input)
  constants <- input[1, is_constant, drop = FALSE]
  rownames(constants) <- NULL

  merge_df(constants, output)
}

#' @export
preserve_constants.grouped_df <- function(input, output) {
  is_constant <- constant_vars(input)

  # Get data frame of constants with one row per group
  constants <- dplyr::do(input, `[`(., 1, is_constant, drop = FALSE))

  group_vars <- unlist(lapply(dplyr::groups(constants), as.character))
  # From input, drop any columns that also exist in output, except grouping
  # vars. This is so that can later do a join without duplicate columns.
  keep_vars <- setdiff(names(constants), setdiff(names(output), group_vars))

  constants <- constants[, keep_vars, drop = FALSE]

  dplyr::inner_join(constants, output, by = group_vars)
}


# Returns a logical vector reporting which columns are constant - that is, for
# that column, all rows have the same value. If the data is grouped, this
# reports whether all rows have the same value _within each group_.
constant_vars <- function(data) UseMethod("constant_vars")

#' @export
constant_vars.data.frame <- function(data) {
  vapply(data, all_same, logical(1), USE.NAMES = FALSE)
}

#' @export
constant_vars.grouped_df <- function(data) {
  # Get number of groups
  n <- length(dplyr::group_size(data))

  # Get a list of boolean vectors
  vecs <- dplyr::do(data, constant_var__ = constant_vars(.))
  vecs <- vecs[["constant_var__"]]

  # Don't create ridiculously long column names
  names(vecs) <- seq_len(length(vecs))
  # Convert to matrix for faster computation
  mat <- as.matrix(as.data.frame(vecs))

  rowSums(mat) == n
}

# Returns a string representing the transform type. For example, if it has
# class "transform_smooth", then this returns "smooth".
transform_type <- function(transform) {
  classes <- class(transform)
  type <- classes[grep("^transform_", classes)][1]
  sub("^transform_", "", type)
}
