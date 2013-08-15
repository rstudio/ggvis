#' S3 mark object.
#'
#' This object is used by all \code{\link{marks}} - you should not have to call
#' it directly, unless you are creating a new R mark object to represent a
#' vega mark that is not currently available in ggvis.
#'
#' @param type vega mark list
#' @param props list of properties
#' @param data optional data pipeline
#' @export
#' @keywords internal
mark <- function(type, props, data = NULL) {
  m <- structure(
    compact(list(
      type = type,
      data = as.pipeline(data),
      props = props
    )),
    class = c(paste0("mark_", type), "mark", "ggvis_node")
  )

  check_mark_props(m, names(m$props))
  m
}

#' @export
#' @rdname mark
is.mark <- function(x) inherits(x, "mark")

#' @importFrom utils adist
check_mark_props <- function(mark, props) {
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


#' @S3method format mark
format.mark <- function(x, ...) {
  paste0("<", class(x)[1], ">",
    if (!is.null(x$pipeline_id)) paste0(" (ID: ", x$pipeline_id, ")"),
    "\n",
    format(x$props))
}

#' @S3method print mark
print.mark <- function(x, ...) cat(format(x), "\n")
