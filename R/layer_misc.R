#' A layer to add a line.
#'
#' @seealso \code{link{mark_path}}
#' @export
#' @param sort Should the data be sorted? If \code{TRUE} (the default), a
#'   \code{\link{transform_sort}} will be used. By default, this will sort the
#'   data on the x variable.
#' @param vis Visualisation to modify.
#' @param ... Named arguments are passed on to \code{\link{transform_sort}};
#'   unnamed arguments are not used.
#' @examples
#' ggvis(mtcars,
#'   props(x = ~wt, y = ~mpg, stroke = ~factor(cyl)),
#'   layer_line(),
#'   layer_point()
#' )
layer_line <- function(vis, ..., sort = TRUE) {
  comps <- parse_components(..., drop_named = TRUE)

  vis %>%
    branch(
      auto_group() %>%
      function(vis, ...) {
        if (sort) vis %>% transform_sort(...)
        else vis
      } %>%
      mark_path(comps$props)
    )
}
