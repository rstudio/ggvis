#' Create a data frame split up by specified variables
#'
#' @export
#' @examples
#' split_df(mtcars, NULL)
#' split_df(mtcars, "cyl")
#' split_df(mtcars, c("cyl", "vs"))
#' split_df(split_df(mtcars, "cyl"), "vs")
split_df <- function(data, split) {
  if (is.null(data)) return(data)

  UseMethod("split_df")
}

#' @S3method split_df data.frame
split_df.data.frame <- function(data, split) {
  if (is.null(split)) return(data)

  stopifnot(is.character(split))
  pieces <- unname(split(data, data[split], drop = TRUE))
  structure(pieces, class = "split_df")
}

#' @S3method split_df split_df
split_df.split_df <- function(data, split) {
  if (is.null(split)) return(data)

  stopifnot(is.character(split))
  splits <- lapply(data, function(x) split(x, x[split], drop = TRUE))
  pieces <- unlist(splits, recursive = FALSE, use.names = FALSE)
  structure(pieces, class = "split_df")
}
