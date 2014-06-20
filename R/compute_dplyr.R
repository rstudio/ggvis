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
    do_call(dplyr::regroup, quote(data), quote(value))
  })
}

#' @export
#' @rdname dplyr-ggvis
ungroup.ggvis <- function(x) {
  register_computation(x, list(), "ungroup", function(data, args) {
    do_call(dplyr::ungroup, quote(data))
  })
}


#' @rdname dplyr-ggvis
#' @export
summarise.ggvis <- function(.data, ...) {
  pieces <- extract_inputs(dots(...))

  register_computation(.data, pieces$inputs, "summarise", function(data, args) {
    do_call(dplyr::summarise, quote(data), .args = pieces$expr)
  })
}

#' @rdname dplyr-ggvis
#' @export
mutate.ggvis <- function(.data, ...) {
  pieces <- extract_inputs(dots(...))

  register_computation(.data, pieces$inputs, "mutate", function(data, args) {
    do_call(dplyr::mutate, quote(data), .args = pieces$expr)
  })
}

#' @rdname dplyr-ggvis
#' @export
arrange.ggvis <- function(.data, ...) {
  pieces <- extract_inputs(dots(...))

  register_computation(.data, pieces$inputs, "arrange", function(data, args) {
    do_call(dplyr::arrange, quote(data), .args = pieces$expr)
  })
}

#' @rdname dplyr-ggvis
#' @export
select.ggvis <- function(.data, ...) {
  pieces <- extract_inputs(dots(...))

  register_computation(.data, pieces$inputs, "select", function(data, args) {
    do_call(dplyr::select, quote(data), .args = pieces$expr)
  })
}

# Need to re-export dplyr::filter to avoid problems with R CMD check.
#' Filter
#'
#' This is the same as \code{dplyr::\link[dplyr]{filter}} function.
#' See \code{dplyr::\link[dplyr]{filter}} for more information.
#'
#' @param .data A tbl.
#' @param ... variables interpreted in the context of the data.
#' @importFrom dplyr filter
#' @name filter
#' @export
NULL

#' @method filter ggvis
#' @rdname dplyr-ggvis
#' @export
filter.ggvis <- function(.data, ...) {
  pieces <- extract_inputs(dots(...))

  register_computation(.data, pieces$inputs, "filter", function(data, args) {
    do_call(dplyr::filter, quote(data), .args = pieces$expr)
  })
}

extract_inputs <- function(x, env = parent.frame()) {
  # Base case
  if (is.name(x) || is.atomic(x)) {
    return(list(expr = x, inputs = NULL))
  }

  # If it's a call to eval, it's an input and should be evaluated
  if (is.call(x) && identical(x[[1]], quote(eval))) {
    stopifnot(length(x) == 2)
    input <- eval(x[[2]], env)

    stopifnot(is.broker(input))
    nm <- names(attr(input, "broker", TRUE)$controls)

    return(list(
      expr = substitute(args$nm, list(nm = as.name(nm))),
      inputs = setNames(list(input), nm)
    ))
  }

  # Otherwise it must be another call, or a list, in which case recurse
  if (is.list(x)) {
    args_out <- lapply(x, extract_inputs, env = env)
  } else if (is.call(x)) {
    args_out <- lapply(x[-1], extract_inputs, env = env)
  } else {
    stop("Unknown input type: ", paste0(class(x), collapse = "/"),
      call. = FALSE)
  }

  expr <- lapply(args_out, "[[", "expr")
  if (is.call(x)) expr <- as.call(c(x[[1]], expr))

  inputs <- unlist(lapply(unname(args_out), "[[", "inputs"), recursive = FALSE)
  inputs <- inputs[!duplicated(names(inputs))]

  list(
    expr = expr,
    inputs = inputs
  )
}
