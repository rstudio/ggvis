#' Create a broker object
#'
#' A broker is a subclass of reactive. It can hold extra information to
#' facilitate (or broker) communication between the client and the server.
#' For example, an input broker may contain HTML controls to be emitted on the
#' client web page, as well as a function to connect the inputs from the client
#' to the reactive expression.
#'
#' Other types of brokers are possible. Another broker may create reactive
#' observers and add information to the Vega spec, instead of having HTML
#' controls. In this case, a reactive expression is still needed, although
#' it can be a dummy value, like \code{reactive(NULL)}.
#'
#' @param r A reactive expression.
#' @param controls An HTML control, or a list of HTML controls.
#' @param connect A function to run at render time. This function takes the
#'   Shiny \code{session} object as its only argument, and is used to connect
#'   the session with the broker object.
#' @param spec Object to put in the Vega spec.
#' @export
#' @keywords internal
create_broker <- function(r, controls = NULL, connect = NULL, spec = NULL) {
  if (!shiny::is.reactive(r)) stop("r must be a reactive expression.")

  # If passed a bare control, wrap it into a list
  if (!is.null(controls) && inherits(controls, "shiny.tag")) {
    controls  <- list(controls)
  }

  class(r) <- c("broker", class(r))

  attr(r, "broker") <- structure(list(
    controls = controls,
    connect = connect,
    spec = spec
  ))

  r
}


#' Determine if an object is a broker object
#'
#' @param x An object to test.
#' @export
is.broker <- function(x) inherits(x, "broker")

#' @export
format.broker <- function(x) {
  b <- attr(x, "broker")
  str <- ""
  if (!is.null(b$controls)) {
    str <- paste0(str, "  Controls: ", length(b$controls), "\n")
  }
  if (!is.null(b$connect)) {
    str <- paste0(str, "  Connect function: yes\n")
  }
  if (!is.null(b$spec)) {
    str <- paste0(str, "  Spec object: ", length(b$spec), "\n")
  }

  str
}


# Given a list of reactives, extract the broker objects from attributes
extract_brokers <- function(reactives) {
  compact(lapply(reactives, function(x) attr(x, "broker")))
}

# Extract the spec portion of a broker
#' @export
as.vega.broker <- function(x) attr(x, "broker")$spec
