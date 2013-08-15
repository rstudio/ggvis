last_vis_e <- new.env(parent = emptyenv())

#' Retrieve the last visualisation created by ggvis.
#'
#' Visualisations are saved whenever they are created or plotted.
#'
#' @export
#' @examples
#' ggvis(mtcars, props(x ~ mpg, y ~ cyl), mark_symbol())
#' str(last_vis())
last_vis <- function() last_vis_e$x

set_last_vis <- function(x) {
  old <- last_vis_e$x
  last_vis_e$x <- x
  old
}
