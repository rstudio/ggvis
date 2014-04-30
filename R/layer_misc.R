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
#' mtcars %>% ggvis(~wt, ~mpg, stroke = ~factor(cyl)) %>% layer_line()
layer_line <- function(vis, ..., sort = TRUE) {
  branch_f(vis, function(x) {
    x <- auto_group(x)
    if (sort) x <- transform_sort(x)
    emit_paths(x, props(...))
  })
}
