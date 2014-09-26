#' Automatically group data by grouping variables
#'
#' Use \code{auto_group} to group up a dataset on all categorical variables
#' specified by props, and have each piece rendered by the same mark.
#'
#' @export
#' @param vis The ggvis visualisation to modify.
#' @param exclude A vector containing names of props to exclude from auto grouping.
#'   It is often useful to exclude \code{c("x", "y")}, when one of those variables
#'   is categorical.
#' @seealso To manually specify grouping variables, see \code{\link{group_by}}.
#' @examples
#' # One line
#' mtcars %>% ggvis(~disp, ~mpg, stroke = ~factor(cyl)) %>% layer_paths()
#' # One line for each level of cyl
#' mtcars %>% ggvis(~disp, ~mpg, stroke = ~factor(cyl)) %>% group_by(cyl) %>%
#'   layer_paths()
#' mtcars %>% ggvis(~disp, ~mpg, stroke = ~factor(cyl)) %>% auto_group() %>%
#'   layer_paths()
#'
#' # The grouping column can already be stored as a factor
#' mtcars2 <- mtcars
#' mtcars2$cyl <- factor(mtcars2$cyl)
#' mtcars2 %>% ggvis(~disp, ~mpg, stroke = ~cyl) %>% auto_group() %>%
#'   layer_paths()
auto_group <- function(vis, exclude = NULL) {

  # Figure out grouping variable
  data <- cur_data(vis)
  props <- cur_props(vis)

  # Drop props named in exclude
  pnames <- trim_prop_event(names(props))
  props <- props[!(pnames %in% exclude)]

  countable <- vapply(props,
    function(prop) is.prop_variable(prop) && prop_countable(data, prop),
    logical(1)
  )
  if (!any(countable)) return(vis)

  group_vars <- lapply(unname(props[countable]), "[[", "value")
  dplyr::group_by_(vis, .dots = group_vars)
}
