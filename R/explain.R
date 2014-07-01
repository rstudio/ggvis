#' Explain details of an object
#'
#' This is a generic function which gives more details about an object than
#' print, and is more focussed on human readable output than str.
#'
#' @export
#' @seealso \code{dplyr::\link[dplyr]{explain}} for more information.
#' @inheritParams dplyr::explain
#' @importFrom dplyr explain
#' @name explain
#' @examples
#' p <- mtcars %>% ggvis(x = ~cyl) %>% layer_bars()
#' explain(p)
NULL

#' Print out the structure of a ggvis object in a friendly format
#'
#' @param x Visualisation to explain
#' @param ... Needed for compatibility with generic. Ignored by this method.
#' @method explain ggvis
#' @export
explain.ggvis <- function (x, ...) {
  cat("Marks:\n")
  for (mark in x$marks) {
    cat(indent(format(mark), 2))
  }
  cat("Data objects:\n")
  for (dat in x$data) {
    cat(indent(data_id(dat), 2), "\n")
  }
  cat("Reactives:\n")
  for (reactive in x$reactives) {
    cat(indent(reactive_id(reactive), 2))
    if (is.broker(reactive)) {
      cat(" <Broker>\n")
    } else {
      cat("\n")
    }
  }
  cat("Scales:\n")
  for (scale_name in names(x$scales)) {
    cat(indent(paste0(scale_name, ":\n"), 2))
    for (scale in x$scales[[scale_name]]) {
      cat(indent(format(scale), 4))
      cat("\n")
    }
  }
  cat("Axes:\n")
  for (axis in x$axes) {
    cat(indent(format(axis), 2))
    cat("\n")
  }
  cat("Legends:\n")
  for (legend in x$legends) {
    cat(indent(format(legend), 2))
    cat("\n")
  }
  cat("HTML controls:\n")
  for (control_name in names(x$controls)) {
    cat(indent(control_name, 2))
    cat("\n")
  }
  cat("Client-side handlers:\n")
  for (handler in x$handlers) {
    cat(indent(paste0("<", handler$type, "> ", handler$id), 2))
    cat("\n")
  }
  cat("Connector functions:\n")
  for (connector in x$connectors) {
    cat(indent(connector_label(connector), 2))
    cat("\n")
  }
  cat("Options:\n")
  if (length(x$options) > 0) {
    params <- param_string(x$options, collapse = FALSE)
    cat(paste0("  ", format(paste0(names(params), ":")), " ", format(params),
      collapse = "\n"))
    cat("\n")
  }
}
