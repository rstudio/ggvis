#' Divide data into groups.
#'
#'
#' @param x a visualisation
#' @param ... variables to group by.
#' @param add By default, when \code{add = FALSE}, \code{group_by} will
#'   override existing groups. To instead add to the existing groups,
#'   use \code{add = FALSE}
#' @export
group_by <- dplyr::group_by

#' Dplyr verbs for ggvis.
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


#' @export
#' @rdname dplyr-ggvis
groups.ggvis <- function(x) {
  shiny::isolate(dplyr::groups(x$cur_data()))
}

#' @export
#' @rdname dplyr-ggvis
regroup.ggvis <- function(x, value) {
  register_computation(x, list(), "regroup", function(data, args) {
    do_call(quote(dplyr::regroup), quote(data), quote(value))
  })
}

#' @export
#' @rdname dplyr-ggvis
ungroup.ggvis <- function(x) {
  register_computation(x, list(), "ungroup", function(data, args) {
    do_call(quote(dplyr::ungroup), quote(data))
  })
}


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

