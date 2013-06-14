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
  if (is.null(split))
    data <- list(data)
  else
    data <- unname(split(data, data[split]))

  structure(
    data,
    class = c("split_data_dflist", "split_data")
  )
}

#' @S3method split_data split_data_dflist
split_data.split_data_dflist <- function(data, split) {
  if (is.null(split))
    return(data)

  data <- structure(
    unlist(lapply(data, split_data, split), recursive = TRUE),
    class = c("split_data_dflist", "split_data")
  )
}
