#' Automatically split data by groups
#'
#' Use \code{auto_split} to split up a dataset on all categorical variables
#' specified by props, and have each piece rendered by the same mark.
#' 
#' @export
#' @seealso To manually specify grouping variables, see \code{\link{by_group}}.
#' @examples
#' # One line
#' ggvis(mtcars, props(x = ~disp, y = ~mpg), mark_line())
#' # One line for each level of cyl
#' ggvis(mtcars, auto_split(cyl), props(x = ~disp, y = ~mpg), mark_line())
#'
#' # This shows the data generated using by_group
#' sluice(pipeline(mtcars, by_group(cyl)), props(x = ~disp, y = ~mpg))
#' # Note that the props aren't used for splitting, but sluice() needs
#' # props to be present to work.
auto_split <- function() {
  pipe(c("auto_split", "split"))
}

#' @S3method format auto_split
format.auto_split <- function(x, ...) {
  " -> auto_split"
}

#' @S3method connect auto_split
connect.auto_split <- function(x, props, source = NULL, session = NULL) {
  source <- as.reactive(source)
  reactive({
    split_vars <- vapply(props, prop_countable, data = source(),
      FUN.VALUE = logical(1))

    # Get quoted expressions to split on
    split_vars <- unname(lapply(props[split_vars], function(prop) prop$value))

    split_df(source(), split_vars, env = x$env)
  })
}

#' @S3method empty auto_split
empty.auto_split <- function(x) FALSE
