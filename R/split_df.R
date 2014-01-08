#' Create a "split_df" object.
#' 
#' An split_df object represents a data frame that has been split into multiple
#' pieces according to the value of some variable (or variables, or functions 
#' of variables).
#' 
#' This function is designed to create \code{split_df} objects by hand:
#' generally, users of ggvis should create these objects with the helper 
#' \code{\link{by_group}} function.
#'
#' @export
#' @param data A data frame.
#' @param split A quoted expression or list of quoted expressions which, when
#'   evaluated with \code{data} (and optionally \code{env}), specifies the split
#'   groups.
#' @param env An enclosing environment in which to evaluate \code{split}.
#' @keywords internal
split_df <- function(data, split, env = NULL) {
  if (is.null(data)) return(data)

  UseMethod("split_df")
}

#' @export
split_df.data.frame <- function(data, split, env) {
  if (is.null(split)) return(data)

  stopifnot(all(vapply(split, is.quoted, logical(1))))

  split_values <- lapply(split, eval, data, enclos = env)
  pieces <- unname(split(data, split_values, drop = TRUE))
  structure(pieces, class = "split_df", variables = split)
}

#' @export
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

#' @export
split_vars.split_df <- function(x) attr(x, "variables")

#' @export
as.data.frame.split_df <- function(x, row.names, optional, ...) {
  do.call(rbind, x)
}
