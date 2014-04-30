#' Dplyr verbs for ggvis
#'
#' @name dplyr-ggvis
#' @keywords internal
NULL

#' @rdname dplyr-ggvis
#' @export
summarise.ggvis <- function(.data, ...) {
  parent_data <- .data$cur_data
  new_data <- shiny::reactive(dplyr::summarise(parent_data(), ...))

  register_data(.data,
    new_data,
    prefix = paste0(get_data_id(parent_data), "_summarise")
  )
}

#' @rdname dplyr-ggvis
#' @export
mutate.ggvis <- function(.data, ...) {
  parent_data <- .data$cur_data
  new_data <- shiny::reactive(dplyr::mutate(parent_data(), ...))

  register_data(.data,
    new_data,
    prefix = paste0(get_data_id(parent_data), "_mutate")
  )
}

#' @rdname dplyr-ggvis
#' @export
arrange.ggvis <- function(.data, ...) {
  parent_data <- .data$cur_data
  new_data <- shiny::reactive(dplyr::arrange(parent_data(), ...))

  register_data(.data,
    new_data,
    prefix = paste0(get_data_id(parent_data), "_arrange")
  )
}

#' @rdname dplyr-ggvis
#' @export
select.ggvis <- function(.data, ...) {
  parent_data <- .data$cur_data
  new_data <- shiny::reactive(dplyr::select(parent_data(), ...))

  register_data(.data,
    new_data,
    prefix = paste0(get_data_id(parent_data), "_select"),
  )
}

#' @rdname dplyr-ggvis
#' @export
filter.ggvis <- function(.data, ...) {
  parent_data <- .data$cur_data
  new_data <- shiny::reactive(dplyr::filter(parent_data(), ...))

  register_data(.data,
    new_data,
    prefix = paste0(get_data_id(parent_data), "_filter")
  )
}

