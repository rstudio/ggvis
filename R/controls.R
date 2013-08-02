controls <- function(x, ...) UseMethod("controls")

# Assumes that controls have id status set - delayed reactive will do this

#' @S3method controls gigvis_node
controls.gigvis_node <- function(x, ...) {
  t_controls <- unlist(lapply(x$data, controls), recursive = FALSE)
  p_controls <- unlist(lapply(x$props, controls), recursive = FALSE)
  c_controls <- unlist(lapply(x$children, controls), recursive = FALSE)
  
  all <- compact(c(t_controls, p_controls, c_controls))
  ids <- vapply(all, function(x) attr(x, "id"), character(1))
  all[!duplicated(ids)]
}

#' @S3method controls list
controls.list <- function(x, ...) {
  dr <- vapply(x, is.delayed_reactive, logical(1))
  unlist(lapply(x[dr], controls), recursive = FALSE, use.names = FALSE)
}
#' @S3method controls gigvis_props
controls.gigvis_props <- controls.list
#' @S3method controls transform
controls.transform <- controls.list

#' @S3method controls delayed_reactive
controls.delayed_reactive <- function(x, ...) {
  # Assuming each reactive only provides one control
  list(x$controls)
}

#' @S3method controls default
controls.default <- function(x, ...) NULL