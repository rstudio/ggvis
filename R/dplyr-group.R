#' @export
groups.ggvis <- function(x) {
  isolate(groups(x$cur_data()))
}

#' @export
regroup.ggvis <- function(x, value) {
  parent_data <- x$cur_data
  new_data <- reactive(regroup(parent_data(), value))

  register_data(x,
    new_data,
    prefix = paste0(get_data_id(parent_data), "_group"),
    update_current = TRUE
  )
}

#' @export
ungroup.ggvis <- function(x) {
  parent_data <- x$cur_data
  new_data <- reactive(ungroup(parent_data()))

  register_data(x,
    new_data,
    prefix = paste0(get_data_id(parent_data), "_ungroup"),
    update_current = TRUE
  )
}
