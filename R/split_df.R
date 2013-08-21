#' Create a data frame split up by specified variables
#'
#' @export
#' @param data A data frame.
#' @param split A quoted expression which, when evaluated with \code{data} (and
#'   optionally \code{env}), specifies the split groups.
#' @param env An enclosing environment in which to evaluate \code{split}.
#' @keywords internal
split_df <- function(data, split, env = NULL) {
  if (is.null(data)) return(data)

  UseMethod("split_df")
}

#' @S3method split_df data.frame
split_df.data.frame <- function(data, split, env) {
  if (is.null(split)) return(data)

  stopifnot(all(vapply(split, is.quoted, logical(1))))

  split_values <- lapply(split, eval, data, enclos = env)
  pieces <- unname(split(data, split_values, drop = TRUE))
  structure(pieces, class = "split_df", variables = split)
}

#' @S3method split_df split_df
split_df.split_df <- function(data, split, env) {
  if (is.null(split)) return(data)

  stopifnot(all(vapply(split, is.quoted, logical(1))))

  splits <- lapply(data, function(x) split_df(x, split, env))
  pieces <- unlist(splits, recursive = FALSE, use.names = FALSE)
  structure(pieces, class = "split_df", variables = c(data$variables, split))
}

#' @export
#' @rdname split_df
#' @param x object to test for split_df-ness
is.split_df <- function(x) inherits(x, "split_df")

#' @S3method split_vars split_df
split_vars.split_df <- function(x) attr(x, "variables")
