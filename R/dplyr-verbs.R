#' @export
summarise.ggvis <- function(.data, ...) {
  parent_data <- .data$cur_data
  new_data <- reactive(summarise(parent_data(), ...))

  register_data(.data,
    new_data,
    prefix = paste0(get_data_id(parent_data), "_summarise"),
    update_current = TRUE
  )
}

#' @export
mutate.ggvis <- function(.data, ...) {
  parent_data <- .data$cur_data
  new_data <- reactive(mutate(parent_data(), ...))

  register_data(.data,
    new_data,
    prefix = paste0(get_data_id(parent_data), "_mutate"),
    update_current = TRUE
  )
}

#' @export
arrange.ggvis <- function(.data, ...) {
  parent_data <- .data$cur_data
  new_data <- reactive(arrange(parent_data(), ...))

  register_data(.data,
    new_data,
    prefix = paste0(get_data_id(parent_data), "_arrange"),
    update_current = TRUE
  )
}

#' @export
select.ggvis <- function(.data, ...) {
  parent_data <- .data$cur_data
  new_data <- reactive(select(parent_data(), ...))

  register_data(.data,
    new_data,
    prefix = paste0(get_data_id(parent_data), "_select"),
    update_current = TRUE
  )
}

#' @export
filter.ggvis <- function(.data, ...) {
  parent_data <- .data$cur_data
  new_data <- reactive(filter(parent_data(), ...))

  register_data(.data,
    new_data,
    prefix = paste0(get_data_id(parent_data), "_filter"),
    update_current = TRUE
  )
}

