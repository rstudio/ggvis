#' Automatically split data by groups
#'
#' Use \code{auto_split} to split up a dataset on all categorical variables
#' specified by props, and have each piece rendered by the same mark.
#'
#' @export
#' @seealso To manually specify grouping variables, see \code{\link{by_group}}.
#' @examples
#' # Make cyl a factor (as it really should be)
#' mtcars2 <- mtcars
#' mtcars2$cyl <- factor(mtcars2$cyl)
#'
#' # One line
#' ggvis(mtcars2, props(x = ~disp, y = ~mpg, stroke = ~cyl)) +
#'   mark_path()
#' # One line for each level of cyl
#' ggvis(mtcars2, auto_split(), props(x = ~disp, y = ~mpg, stroke = ~cyl)) +
#'   mark_path()
#'
#' # This shows the data generated using by_group
#' sluice(pipeline(mtcars, by_group(cyl)), props(x = ~disp, y = ~mpg))
#' # Note that the props aren't used for splitting, but sluice() needs
#' # props to be present to work.
auto_split <- function() {
  pipe(c("auto_split", "split"))
}

#' @export
format.auto_split <- function(x, ...) {
  " -> auto_split"
}

#' @export
connect.auto_split <- function(x, props, source = NULL, session = NULL) {
  source <- as.reactive(source)
  reactive({
    # Get quoted expressions from props which are both variable and countable
    data <- source()
    countable <- vapply(props,
      function(prop) prop$type == "variable" && prop_countable(data, prop),
      logical(1)
    )
    if (!any(countable)) {
      stop("No categorical variables", call. = FALSE)
    }

    split_vars <- lapply(unname(props[countable]), "[[", "value")

    split_df(data, split_vars, env = x$env)
  })
}

#' @export
empty.auto_split <- function(x) FALSE
