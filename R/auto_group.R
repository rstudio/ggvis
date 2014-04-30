#' Automatically split data by groups
#'
#' Use \code{auto_split} to split up a dataset on all categorical variables
#' specified by props, and have each piece rendered by the same mark.
#'
#' @export
#' @param vis The ggvis visualisation to modify.
#' @seealso To manually specify grouping variables, see \code{\link{group_by}}.
#' @examples
#' # Make cyl a factor (as it really should be)
#' mtcars2 <- mtcars
#' mtcars2$cyl <- factor(mtcars2$cyl)
#'
#' # One line
#' mtcars2 %>% ggvis(~disp,  ~mpg, stroke = ~cyl) %>% layer_paths()
#' # One line for each level of cyl
#' mtcars2 %>% ggvis(~disp,  ~mpg, stroke = ~cyl) %>% group_by(cyl) %>%
#'   layer_paths()
#' mtcars2 %>% ggvis(~disp,  ~mpg, stroke = ~cyl) %>% auto_group() %>%
#'   layer_paths()
auto_group <- function(vis) {
  parent_data <- vis$cur_data
  parent_props <- vis$cur_props

  new_data <- shiny::reactive({
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

    dplyr::regroup(data, group_vars)
  })

  register_data(vis,
    new_data,
    prefix = paste0(get_data_id(parent_data), "_auto_group")
  )
}
