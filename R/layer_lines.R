#' Layer lines on a plot.
#'
#' @seealso \code{link{mark_path}}
#' @export
#' @param vis Visualisation to modify.
#' @param ... Visual properties.
#' @examples
#' mtcars2 <- dplyr::mutate(mtcars, cyl = factor(cyl))
#' mtcars2 %>% ggvis(~wt, ~mpg, stroke = ~cyl) %>% layer_lines()
#'
#' # Equivalent to
#' mtcars2 %>% ggvis(~wt, ~mpg, stroke = ~cyl) %>%
#'   group_by(cyl) %>% dplyr::arrange(wt) %>% layer_paths()
layer_lines <- function(vis, ...) {

  x_var <- vis$cur_props$x$value

  layer_f(vis, function(x) {
    x <- auto_group(x, exclude = c("x", "y"))
    x <- do_call(dplyr::arrange, quote(x), x_var)
    emit_paths(x, props(...))
  })
}
