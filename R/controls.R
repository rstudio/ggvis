# Extract a UI control from an object
extract_controls <- function(x) UseMethod("extract_controls")

#' @export
extract_controls.ggvis <- function(x) {
  compact(lapply(x$reactives, extract_controls))
}

#' @export
extract_controls.broker <- function(x) {
  broker <- attr(x, "broker")

  broker$controls
}

#' @export
extract_controls.default <- function(x) NULL
