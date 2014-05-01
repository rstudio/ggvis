#' Dplyr verbs for ggvis
#'
#' @name dplyr-ggvis
#' @keywords internal
#' @examples
#' library(dplyr)
#' base <- mtcars %>% ggvis(~mpg, ~cyl) %>% layer_points()
#' base %>% group_by(cyl) %>% summarise(mpg = mean(mpg)) %>%
#'   layer_points(fill := "red", size := 100)
#'
#' base %>% filter(mpg > 25) %>% layer_points(fill := "red")
#'
#' base %>% mutate(cyl = jitter(cyl)) %>% layer_points(fill := "red")
NULL

#' @rdname dplyr-ggvis
#' @export
summarise.ggvis <- function(.data, ...) {
  dots <- dots(...)
  register_computation(.data, list(), "summarise", function(data, args) {
    do_call(quote(dplyr::summarise), quote(data), .args = dots)
  })
}

#' @rdname dplyr-ggvis
#' @export
mutate.ggvis <- function(.data, ...) {
  dots <- dots(...)
  register_computation(.data, list(), "mutate", function(data, args) {
    do_call(quote(dplyr::mutate), quote(data), .args = dots)
  })
}

#' @rdname dplyr-ggvis
#' @export
arrange.ggvis <- function(.data, ...) {
  dots <- dots(...)
  register_computation(.data, list(), "arrange", function(data, args) {
    do_call(quote(dplyr::arrange), quote(data), .args = dots)
  })
}

#' @rdname dplyr-ggvis
#' @export
select.ggvis <- function(.data, ...) {
  dots <- dots(...)
  register_computation(.data, list(), "select", function(data, args) {
    do_call(quote(dplyr::select), quote(data), .args = dots)
  })
}

#' @importFrom dplyr filter
#' @method filter ggvis
#' @rdname dplyr-ggvis
#' @export
filter.ggvis <- function(.data, ...) {
  dots <- dots(...)
  register_computation(.data, list(), "filter", function(data, args) {
    do_call(quote(dplyr::filter), quote(data), .args = dots)
  })
}

