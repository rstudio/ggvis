#' Split data by group
#'
#' @param variables A character vector of variable names to split on.
#' @export
by_group <- function(variables) {
  # TODO: Check that 'variables' is a character vector (or list of strings?)
  structure(
    variables,
    class = c("split_by_group", "split")
  )
}


split_data <- function(data, split) UseMethod("split_data")

#' @S3method split_data data.frame
split_data.data.frame <- function(data, split) {
  structure(
    unname(split(data, data[split])),
    class = c("split_data_dflist", "split_data")
  )
}
