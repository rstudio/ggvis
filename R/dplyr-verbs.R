#' @export
summarise.ggvis <- function(.data, ...) {
  parent_data <- .data$cur_data
  new_data <- reactive(summarise(parent_data(), ...))

  register_data(.data,
    new_data,
    prefix = paste0(get_data_id(parent_data), "_summarise")
  )
}

#' @export
mutate.ggvis <- function(.data, ...) {
  parent_data <- .data$cur_data
  new_data <- reactive(mutate(parent_data(), ...))

  register_data(.data,
    new_data,
    prefix = paste0(get_data_id(parent_data), "_mutate")
  )
}

#' @export
arrange.ggvis <- function(.data, ...) {
  parent_data <- .data$cur_data
  new_data <- reactive(arrange(parent_data(), ...))

  register_data(.data,
    new_data,
    prefix = paste0(get_data_id(parent_data), "_arrange")
  )
}

#' @export
select.ggvis <- function(.data, ...) {
  parent_data <- .data$cur_data
  new_data <- reactive(select(parent_data(), ...))

  register_data(.data,
    new_data,
    prefix = paste0(get_data_id(parent_data), "_select"),
  )
}

#' @export
filter.ggvis <- function(.data, ...) {
  parent_data <- .data$cur_data
  new_data <- reactive(filter(parent_data(), ...))

  register_data(.data,
    new_data,
    prefix = paste0(get_data_id(parent_data), "_filter")
  )
}

