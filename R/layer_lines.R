#' Layer lines on a plot.
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
#' mtcars2 <- dplyr::mutate(mtcars, cyl = factor(cyl))
#' mtcars2 %>% ggvis(~wt, ~mpg, stroke = ~cyl) %>% layer_lines()
#'
#' # Equivalent to
#' mtcars2 %>% ggvis(~wt, ~mpg, stroke = ~cyl) %>%
#'   group_by(cyl) %>% arrange(wt) %>% layer_paths()
layer_lines <- function(vis, ..., sort = TRUE) {

  x_var <- vis$cur_props$x$value

  branch_f(vis, function(x) {
    x <- auto_group(x)
    if (sort) x <- do_call(quote(dplyr::arrange), quote(x), x_var)
    emit_paths(x, props(...))
  })
}
