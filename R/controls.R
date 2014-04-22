# Extract a UI control from an object
extract_controls <- function(x) UseMethod("extract_controls")

#' @export
extract_controls.ggvis <- function(x) {
  compact(lapply(x$reactives, extract_controls))
}

#' @export
extract_controls.reactive <- function(x) {
  if (!has_controls(x)) return(NULL)

  attr(x, "controls")
}

# Reports whether an object has controls
has_controls <- function(x) !is.null(attr(x, "controls"))
