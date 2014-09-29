#' Divide data into groups.
#'
#' @param x a visualisation
#' @param ... variables to group by.
#' @param add By default, when \code{add = FALSE}, \code{group_by} will
#'   override existing groups. To instead add to the existing groups,
#'   use \code{add = FALSE}
#' @importFrom dplyr group_by
#' @name group_by
#' @export
NULL

#' Dplyr verbs for ggvis.
#'
#' Reactive components must be wrapped in \code{eval} - this makes it
#' possible to separate out the non-standard evaluation of dplyr and ggvis.
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
#'
#' \dontrun{
#' # Dynamically restrict range using filter
#' mtcars %>% ggvis(~disp, ~mpg) %>%
#'    filter(cyl > eval(input_slider(0, 10))) %>%
#'    layer_points()
#'
#' # Dynamically compute box-cox transformation with mutate
#' bc <- function(x, lambda) {
#'   if (abs(lambda) < 1e-6) log(x) else (x ^ lambda - 1) / lambda
#' }
#' bc_slider <- input_slider(-2, 2, 1, step = 0.1)
#' mtcars %>%
#'  ggvis(~disp, ~mpg) %>%
#'  mutate(disp = bc(disp, eval(bc_slider))) %>%
#'  layer_points()
#' }
NULL

#' @export
#' @rdname dplyr-ggvis
groups.ggvis <- function(x) {
  shiny::isolate(dplyr::groups(x$cur_data()))
}

#' @export
#' @rdname dplyr-ggvis
group_by_.ggvis <- function(.data, ..., .dots, add = FALSE) {
  dots <- lazyeval::all_dots(.dots, ...)
  pieces <- extract_inputs(dots)

  register_computation(.data, pieces$inputs, "group_by", function(data, args) {
    dplyr::group_by_(data, .dots = pieces$ldots, add = add)
  })
}

#' @export
#' @rdname dplyr-ggvis
ungroup.ggvis <- function(x) {
  register_computation(x, list(), "ungroup", function(data, args) {
    dplyr::ungroup(data)
  })
}


#' @rdname dplyr-ggvis
#' @export
summarise_.ggvis <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  pieces <- extract_inputs(dots)

  register_computation(.data, pieces$inputs, "summarise", function(data, args) {
    dplyr::summarise_(data, .dots = pieces$ldots)
  })
}

#' @rdname dplyr-ggvis
#' @export
mutate_.ggvis <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  pieces <- extract_inputs(dots)

  register_computation(.data, pieces$inputs, "mutate", function(data, args) {
    dplyr::mutate_(data, .dots = pieces$ldots)
  })
}

#' @rdname dplyr-ggvis
#' @export
arrange_.ggvis <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  pieces <- extract_inputs(dots)

  register_computation(.data, pieces$inputs, "arrange", function(data, args) {
    dplyr::arrange_(data, .dots = pieces$ldots)
  })
}

#' @rdname dplyr-ggvis
#' @export
select_.ggvis <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  pieces <- extract_inputs(dots)

  register_computation(.data, pieces$inputs, "select", function(data, args) {
    dplyr::select_(data, .dots = pieces$ldots)
  })
}

# Need to re-export dplyr::filter and filter_ to avoid problems with R CMD check.
#' Filter
#'
#' This is the same as \code{dplyr::\link[dplyr]{filter}} function.
#' See \code{dplyr::\link[dplyr]{filter}} for more information.
#'
#' @param .data A tbl.
#' @param ... Logical predicates. Multiple conditions are combined with &.
#' @param .dots Used to work around non-standard evaluation. See
#'   vignette("nse", package="dplyr") for details.
#' @importFrom dplyr filter
#' @name filter
#' @export
NULL

#' @name filter_
#' @rdname filter
#' @importFrom dplyr filter_
#' @export
NULL

#' @method filter_ ggvis
#' @rdname dplyr-ggvis
#' @export
filter_.ggvis <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  pieces <- extract_inputs(dots)

  register_computation(.data, pieces$inputs, "filter", function(data, args) {
    dplyr::filter_(data, .dots = pieces$ldots)
  })
}

# FIXME: need to properly extract inputs
# Given a lazy_dots object, return a list with two items:
# * ldots: a lazy_dots object
# * inputs: a list of input (broker) objects
extract_inputs <- function(ldots) {
  list(
    ldots = ldots,
    inputs = NULL
  )
}


#' @rdname dplyr-ggvis
#' @export
groups.reactive <- function(x) reactive(dplyr::groups(x()))
#' @rdname dplyr-ggvis
#' @export
ungroup.reactive <- function(x) reactive(dplyr::ungroup(x()))
#' @rdname dplyr-ggvis
#' @export
group_by_.reactive <- function(.data, ..., .dots, add = FALSE) {
  reactive(dplyr::group_by_(.data(), ..., .dots = .dots, add = add))
}
#' @rdname dplyr-ggvis
#' @export
summarise_.reactive <- function(.data, ..., .dots) {
  reactive(dplyr::summarise_(.data(), ..., .dots = .dots))
}
#' @rdname dplyr-ggvis
#' @export
mutate_.reactive <- function(.data, ..., .dots) {
  reactive(dplyr::mutate_(.data(), ..., .dots = .dots))
}
#' @rdname dplyr-ggvis
#' @export
arrange_.reactive <- function(.data, ..., .dots) {
  reactive(dplyr::arrange_(.data(), ..., .dots = .dots))
}
#' @rdname dplyr-ggvis
#' @export
select_.reactive <- function(.data, ..., .dots) {
  reactive(dplyr::select_(.data(), ..., .dots = .dots))
}
#' @rdname dplyr-ggvis
#' @export
filter_.reactive <- function(.data, ..., .dots) {
  reactive(dplyr::filter_(.data(), ..., .dots = .dots))
}
