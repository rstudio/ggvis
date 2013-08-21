#' Split data by group
#'
#' @param ... Variables to split on.  These are coerced to variable
#' \code{\link{prop}} objects with \code{as.prop}.
#'
#' @export
#' @examples
#' by_group("cyl")
#' by_group(quote(cyl))
#'
#' pl <- pipeline(mtcars, by_group("cyl"), transform_bin())
#' sluice(pl, props(x ~ disp))
by_group <- function(..., env = parent.frame()) {
  vars <- list(...)
  is_char <- vapply(vars, is.character, logical(1))
  vars[is_char] <- lapply(vars[is_char], as.name)

  pipe(c("split_by_group", "split"), variables = vars, env = env)
}

#' @S3method format split_by_group
format.split_by_group <- function(x, ...) {
  paste0(" -> split_by", param_string(x))
}

#' @S3method connect split_by_group
connect.split_by_group <- function(x, props, source = NULL, session = NULL) {
  source <- as.reactive(source)
  reactive(split_df(source(), x$variables, env = x$env))
}

#' @S3method pipe_id split
#' @importFrom digest digest
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
