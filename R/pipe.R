#' Pipe graphics
#'
#' Like dplyr, ggvis also uses the pipe function, \code{\%>\%} to turn
#' function composition into a series of imperative statements.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
#' @param lhs,rhs A visualisation and a function to apply to it
#' @examples
#' # Instead of
#' layer_points(ggvis(mtcars, ~mpg, ~wt))
#' # you can write
#' mtcars %>% ggvis(~mpg, ~wt) %>% layer_points()
NULL
