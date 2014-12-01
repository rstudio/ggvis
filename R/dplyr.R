#' Divide data into groups.
#'
#' @param x a visualisation
#' @param ... variables to group by.
#' @param add By default, when \code{add = FALSE}, \code{group_by} will
#'   override existing groups. To instead add to the existing groups,
#'   use \code{add = TRUE}
#' @importFrom dplyr group_by
#' @name group_by
#' @export
NULL


#' Dplyr verbs for ggvis.
#'
#' Applying a dplyr verb to a ggvis object creates a reactive transformation:
#' whenever the underlying data changes the transformation will be recomputed.
#'
#' @section Non-standard evaluation:
#' Both dplyr and shiny do non-standard evaluation, so to help each package
#' figure out when it should evaluate its code, reactive components in
#' these functions must be wrapped in \code{eval()}.
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

# Methods for ggvis objects ----------------------------------------------------

#' @export
#' @rdname dplyr-ggvis
groups.ggvis <- function(x) {
  shiny::isolate(dplyr::groups(x$cur_data()))
}

#' @export
#' @rdname dplyr-ggvis
group_by_.ggvis <- function(.data, ..., .dots, add = FALSE) {
  dots <- lazyeval::all_dots(.dots, ...)
  pieces <- extract_lazy_inputs(dots)

  register_computation(.data, pieces$inputs, "group_by", function(data, args) {
    dplyr::group_by_(data, .dots = lapply(pieces$lazy, add_args, args),
      add = add)
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
  pieces <- extract_lazy_inputs(dots)

  register_computation(.data, pieces$inputs, "summarise", function(data, args) {
    dplyr::summarise_(data, .dots = lapply(pieces$lazy, add_args, args))
  })
}

#' @rdname dplyr-ggvis
#' @export
mutate_.ggvis <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  pieces <- extract_lazy_inputs(dots)

  register_computation(.data, pieces$inputs, "mutate", function(data, args) {
    dplyr::mutate_(data, .dots = lapply(pieces$lazy, add_args, args))
  })
}

#' @rdname dplyr-ggvis
#' @export
arrange_.ggvis <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  pieces <- extract_lazy_inputs(dots)

  register_computation(.data, pieces$inputs, "arrange", function(data, args) {
    dplyr::arrange_(data, .dots = lapply(pieces$lazy, add_args, args))
  })
}

#' @rdname dplyr-ggvis
#' @export
select_.ggvis <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  pieces <- extract_lazy_inputs(dots)

  register_computation(.data, pieces$inputs, "select", function(data, args) {
    dplyr::select_(data, .dots = lapply(pieces$lazy, add_args, args))
  })
}

#' @rdname dplyr-ggvis
#' @export
filter_.ggvis <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  pieces <- extract_lazy_inputs(dots)

  register_computation(.data, pieces$inputs, "filter", function(data, args) {
    dplyr::filter_(data, .dots = lapply(pieces$lazy, add_args, args))
  })
}

#' @rdname dplyr-ggvis
#' @export
distinct_.ggvis <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  pieces <- extract_lazy_inputs(dots)

  register_computation(.data, pieces$inputs, "distinct", function(data, args) {
    dplyr::distinct_(data, .dots = lapply(pieces$lazy, add_args, args))
  })
}

#' @rdname dplyr-ggvis
#' @export
slice_.ggvis <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  pieces <- extract_lazy_inputs(dots)

  register_computation(.data, pieces$inputs, "slice", function(data, args) {
    dplyr::slice_(data, .dots = lapply(pieces$lazy, add_args, args))
  })
}

#' @rdname dplyr-ggvis
#' @export
rename_.ggvis <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  pieces <- extract_lazy_inputs(dots)

  register_computation(.data, pieces$inputs, "rename", function(data, args) {
    dplyr::rename_(data, .dots = lapply(pieces$lazy, add_args, args))
  })
}

#' @rdname dplyr-ggvis
#' @export
transmute_.ggvis <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  pieces <- extract_lazy_inputs(dots)

  register_computation(.data, pieces$inputs, "transmute", function(data, args) {
    dplyr::transmute_(data, .dots = lapply(pieces$lazy, add_args, args))
  })
}


#' Extract reactive inputs from a lazy dots.
#'
#' This works by replacing each reactive expression (which must be wrapped
#' in \code{eval}), with a reference to \code{args$xyz}. Then this function
#' returns both the modified lazy dots, and a list of reactives that need
#' to be created.
#'
#' @noRd
#' @examples
#' # extract_inputs() works with language objects ------------------------------
#'
#' # Simple expressions are returned as is
#' extract_inputs(quote(1))
#' extract_inputs(quote(x))
#' extract_inputs(quote(x + y))
#'
#' # If the call contains eval, then the subexpression is evalutes, the
#' # original is replaced with a reference to args, and inputs gains
#' # a reactive broker
#' extract_inputs(quote(eval(input_slider(0, 100))))
#'
#' slider <- input_slider(0, 100)
#' extract_inputs(quote(eval(slider) + eval(slider)))
#'
#' # extract_lazy_inputs() works with lazy objects -----------------------------
#' library(lazyeval)
#' extract_lazy_inputs(lazy(x + y))
#' extract_lazy_inputs(lazy(x + eval(input_slider(0, 100))))
#'
#' s1 <- input_slider(0, 100)
#' s2 <- input_slider(0, 200)
#' extract_lazy_inputs(lazy_dots(x + eval(s1), eval(s2), eval(s1) / eval(s2)))
extract_lazy_inputs <- function(x) {
  if (inherits(x, "lazy_dots")) {
    pieces <- lapply(x, extract_lazy_inputs)

    lazy <- lazyeval::as.lazy_dots(pluck(pieces, "lazy"))
    inputs <- unlist(unname(pluck(pieces, "inputs")), recursive = FALSE)
    inputs <- inputs[!duplicated(names(inputs))]

    list(
      lazy = lazy,
      inputs = inputs
    )
  } else if (inherits(x, "lazy")) {
    new <- extract_inputs(x$expr, x$env)

    x$expr <- new$expr
    list(lazy = x, inputs = new$inputs)
  } else {
    stop("Unknown input type: ", paste0(class(x), collapse = "/"),
      call. = FALSE)
  }
}

extract_inputs <- function(expr, env = parent.frame()) {
  if (is.name(expr) || is.atomic(expr)) {
    # Base case
    list(expr = expr, inputs = list())
  } else if (is.call(expr) && identical(expr[[1]], quote(eval))) {
    # If it's a call to eval, it's an input and should be evaluated
    stopifnot(length(expr) == 2)

    input <- eval(expr[[2]], env)
    stopifnot(is.broker(input))
    nm <- names(attr(input, "broker", TRUE)$controls)

    list(
      expr = substitute(args$nm, list(nm = as.name(nm))),
      inputs = setNames(list(input), nm)
    )
  } else if (is.call(expr)) {
    # Recursive over arguments and join back together again
    args_out <- lapply(expr[-1], extract_inputs, env = env)

    args <- pluck(args_out, "expr")
    inputs <- unlist(pluck(args_out, "inputs"), recursive = FALSE)
    inputs <- inputs[!duplicated(names(inputs))]

    list(
      expr = as.call(c(expr[[1]], args)),
      inputs = inputs
    )
  } else {
    stop("Don't know how to deal with input of type: ", class(expr)[[1]],
      call. = FALSE)
  }
}

# Given a lazy object, modify it so it's environment also gets to
# access the args list.
add_args <- function(x, args) {
  e <- new.env(parent = x$env)
  e$args <- args
  x$env <- e

  x
}



# Methods for reactive data frames --------------------------------------------

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
#' @rdname dplyr-ggvis
#' @export
distinct_.reactive <- function(.data, ..., .dots) {
  reactive(dplyr::distinct_(.data(), ..., .dots = .dots))
}
#' @rdname dplyr-ggvis
#' @export
slice_.reactive <- function(.data, ..., .dots) {
  reactive(dplyr::slice_(.data(), ..., .dots = .dots))
}
#' @rdname dplyr-ggvis
#' @export
rename_.reactive <- function(.data, ..., .dots) {
  reactive(dplyr::rename_(.data(), ..., .dots = .dots))
}
#' @rdname dplyr-ggvis
#' @export
transmute_.reactive <- function(.data, ..., .dots) {
  reactive(dplyr::transmute_(.data(), ..., .dots = .dots))
}
