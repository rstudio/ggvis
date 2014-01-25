# Extract a UI control from an object
controls <- function(x, session = NULL, ...) UseMethod("controls")

# Assumes that controls have id status set - delayed reactive will do this

#' @export
controls.layer <- function(x, session = NULL, ...) {
  t_controls <- unlist(unname(lapply(x$data, controls)), recursive = FALSE)
  p_controls <- unlist(unname(lapply(x$props, controls)), recursive = FALSE)
  c_controls <- unlist(unname(lapply(x$children, controls)), recursive = FALSE)

  all <- compact(c(t_controls, p_controls, c_controls))
  all[!duplicated(names(all))]
}

#' @export
controls.list <- function(x, session = NULL, ...) {
  inp <- vapply(x, is.input, logical(1))
  # Remove top-level name (method, n, etc), but preserve second-level name,
  # which is the id of the input.
  ctrls <- lapply(x[inp], controls, session)
  unlist(unname(ctrls), recursive = FALSE, use.names = TRUE)
}
#' @export
controls.ggvis_props <- controls.list
#' @export
controls.prop <- controls.list

#' @export
controls.transform <- function(x) {
  c(controls.list(x), controls.list(x$dots))
}

#' @export
controls.default <- function(x, session = NULL, ...) NULL
