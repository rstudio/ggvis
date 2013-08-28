#' Visualise data by group.
#'
#' Use \code{by_group} to split up a dataset into multiple pieces, and have
#' each piece rendered by the same mark.
#' 
#' @param ... Unquoted variable names or expressions describe how to split
#'   up the dataset.
#' @param .vars a list of quoted expressions.
#' @param .env environment in which to evalute expressions. In ordinary use,
#'   the default is adequate.
#' @export
#' @examples
#' by_group(cyl)
#' by_group(vs, am)
#' by_group(vs + am)
#'
#' # One line
#' ggvis(mtcars, props(x = ~ disp, y = ~ mpg), mark_line())
#' # One line for each level of cyl
#' ggvis(mtcars, by_group(cyl), props(x = ~ disp, y = ~ mpg), mark_line())
#' 
#' # Special evaluation -------------------
#' 
#' # If you have previously quoted variables, use .vars
#' v <- quote(cyl)
#' by_group(.vars = list(v))
#' 
#' # If you have the name of a variable as a string, use as.name
#' var <- "cyl"
#' by_group(.vars = list(as.name(var)))
by_group <- function(..., .vars = list(), .env = parent.frame()) {
  vars <- unname(c(dots(...), .vars))

  pipe(c("split_by_group", "split"), variables = vars, env = .env)
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
