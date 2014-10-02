#' Create a new "mark" object.
#'
#' A mark object is a close mapping to a vega mark object. Vega marks
#' are documented in \url{https://github.com/trifacta/vega/wiki/Marks}.
#'
#' This function is designed to be used by authors of new types of mark.
#'
#' @param type A string with the vega type.
#' @param props A list of properties, created by \code{\link{props}}.
#' @param data A reactive data object.
#' @keywords internal
#' @export
mark <- function(type, props, data) {
  if (!is.ggvis_props(props)) stop("props must be a ggvis_props object")
  if (is.null(data)) stop("No data supplied to mark.", call. = FALSE)
  if (!is.function(data)) stop("data object must be a reactive or a function.")

  # Check that names are correct
  check_mark_props(type, names(props))

  # Some marks need more detailed validity checks of their props
  check_valid_props <- mark_props_validity_checks[[type]]
  if (!is.null(check_valid_props)) {
    check_valid_props(props)
  }

  # Merge in defaults
  props <- merge_props(default_props(type), props, inherit = TRUE)

  # FIXME: check that the variables in the prop can be found in data
  structure(list(type = type, data = data, props = props), class = "mark")
}

mark_group <- function(props, data, marks = list(), scales = list(),
                       axes = list(), legends = list()) {

  check_mark_props("rect", names(props))
  structure(
    list(
      type = "group",
      data = data,
      props = props,
      marks = marks,
      scales = scales,
      axes = axes,
      legends = legends
    ),
    class = c("mark_group", "mark")
  )
}

#' @rdname mark
#' @export
is.mark <- function(x) inherits(x, "mark")

is.mark_group <- function(x) inherits(x, "mark_group")

check_mark_props <- function(type, props) {
  props <- trim_prop_event(props)
  valid <- valid_props[[type]]

  invalid <- setdiff(props, valid)
  if (length(invalid) == 0) return(invisible(TRUE))

  ldist <- utils::adist(invalid, valid, ignore.case = TRUE, partial = FALSE,
    costs = c(ins = 0.5, sub = 1, del = 2))

  closest <- apply(ldist, 1, min)
  possible_match <- closest < 5
  if (any(possible_match)) {
    best <- apply(ldist, 1, which.min)

    matches <- valid[best][possible_match]
    suggest <- paste0("Did you mean: ", paste0(matches, collapse = ", "), "?")
  } else {
    suggest <- ""
  }

  stop("Unknown properties: ", paste0(invalid, collapse = ", "), ".\n", suggest,
    call. = FALSE)
}

#' @export
format.mark <- function(x, ...) {
  paste0("<", class(x)[1], ">",
    " (Data: ", data_id(x$data), ")",
    "\n",
    indent(format(x$props), 2)
  )
}

#' @export
print.mark <- function(x, ...) cat(format(x), "\n")
