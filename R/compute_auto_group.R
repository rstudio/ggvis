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

  # Figure out grouping variable
  data <- cur_data(vis)
  props <- cur_props(vis)

  countable <- vapply(props,
    function(prop) prop$type == "variable" && prop_countable(data, prop),
    logical(1)
  )
  if (!any(countable)) return(vis)

  group_vars <- lapply(unname(props[countable]), "[[", "value")
  dplyr::regroup(vis, group_vars)
}
