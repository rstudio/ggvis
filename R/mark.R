  #' Create a new "mark" object.
#'
#' A mark object is a close mapping to a vega mark object. Vega marks
#' are documented in \url{https://github.com/trifacta/vega/wiki/Marks}.
#' 
#' This function is designed to be used by authors of new types of mark.
#' If you are a ggvis user, please use one of the more specific mark
#' functions starting with the \code{mark_}.
#'
#' @param type vega mark list
#' @param props list of properties
#' @param data optional data pipeline
#' @export
#' @keywords internal
mark <- function(type, props_id, data_id) {

  m <- structure(
    compact(list(
      type = type,
      data_id = data_id,
      props_id = props_id
    )),
    class = c(paste0("mark_", type), "mark", "layer")
  )

  check_mark_props(m, names(m$props))
  m
}

#' @export
#' @rdname mark
is.mark <- function(x) inherits(x, "mark")

#' @importFrom utils adist
check_mark_props <- function(mark, props) {
  props <- trim_propset(props)
  valid <- valid_mark_properties(mark)

  invalid <- setdiff(props, valid)
  if (length(invalid) == 0) return(invisible(TRUE))

  ldist <- adist(invalid, valid, ignore.case = TRUE, partial = FALSE,
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
    if (!is.null(x$pipeline_id)) paste0(" (ID: ", x$pipeline_id, ")"),
    "\n",
    format(x$props))
}

#' @export
print.mark <- function(x, ...) cat(format(x), "\n")
