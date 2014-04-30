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
groups.ggvis <- function(x) {
  isolate(dplyr::groups(x$cur_data()))
}

#' @export
regroup.ggvis <- function(x, value) {
  parent_data <- x$cur_data
  new_data <- reactive(dplyr::regroup(parent_data(), value))

  register_data(x,
    new_data,
    prefix = paste0(get_data_id(parent_data), "_group")
  )
}

#' @export
ungroup.ggvis <- function(x) {
  parent_data <- x$cur_data
  new_data <- reactive(dplyr::ungroup(parent_data()))

  register_data(x,
    new_data,
    prefix = paste0(get_data_id(parent_data), "_ungroup")
  )
}
