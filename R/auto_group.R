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
auto_group <- function(vis) {
  parent_data <- vis$cur_data
  parent_props <- vis$cur_props

  new_data <- reactive({
    # Get quoted expressions from props which are both variable and countable
    data <- parent_data()
    countable <- vapply(parent_props,
      function(prop) prop$type == "variable" && prop_countable(data, prop),
      logical(1)
    )
    if (!any(countable)) {
      return(data)
    }

    group_vars <- lapply(unname(parent_props[countable]), "[[", "value")

    regroup(data, group_vars)
  })

  register_data(vis,
    new_data,
    prefix = paste0(get_data_id(parent_data), "_auto_group")
  )
}
