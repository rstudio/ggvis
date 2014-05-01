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
