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
    names(controls) <- paste0("unnamed input ", seq_len(length(controls)))
  }

  class(r) <- c("broker", class(r))

  attr(r, "broker") <- structure(list(
    controls = controls,
    connect = connect,
    spec = spec
  ))

  if (is.null(reactive_id(r))) {
    reactive_id(r) <- rand_id("reactive_")
  }

  r
}

#' Determine if an object is a broker object
#'
#' @param x An object to test.
#' @export
is.broker <- function(x) inherits(x, "broker")

# Get the label of a connector function
connector_label <- function(x) attr(x, "label", TRUE)

# Set the label of a connector function
`connector_label<-` <- function(x, value) {
  attr(x, "label") <- value
  x
}
