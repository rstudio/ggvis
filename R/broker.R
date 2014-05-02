#' Create a broker object
#'
#' A broker is a subclass of reactive. It can hold extra information to
#' facilitate (or broker) communication between the client and the server.
#' For example, an input broker may contain HTML controls to be emitted on the
#' client web page, as well as a reactivevalues object and an input_id, which
#' are used by the server to connect the input values sent from the client with
#' the reactive expression.
#'
#' Other types of brokers are possible. Another broker may create reactive
#' observers and add information to the Vega spec, instead of having HTML
#' controls. In this case, a reactive expression is still needed, although
#' it can be a dummy value, like \code{reactive(NULL)}.
#'
#' @param r A reactive expression.
#' @param vals A reactiveValues object.
#' @param input_ids A character vector of input IDs. These input IDs will be
#'   present in session$input, as well as in val, and in the as.vega stage,
#'   observers will be created which push values from session$input to val,
#'   for each ID.
#' @param observers An observer, or a list of observers.
#' @param controls An HTML control, or a list of HTML controls.
#' @param spec Object to put in the Vega spec.
#' @export
#' @keywords internal
create_broker <- function(r, vals = NULL, input_ids = NULL, observers = NULL,
                          controls = NULL, spec = NULL) {
  if (!shiny::is.reactive(r)) stop("r must be a reactive expression.")
  if (!shiny::is.reactivevalues(vals)) stop("vals must be a reactivevalues object.")

  # If passed bare observer or control, wrap them into list
  if (!is.null(observers) && inherits(observers, "Observer")) {
    observers <- list(observers)
  }
  if (!is.null(controls) && inherits(controls, "shiny.tag")) {
    controls  <- list(controls)
  }

  class(r) <- c("broker", class(r))

  attr(r, "broker") <- structure(list(
    vals = vals,
    input_ids = input_ids,
    observers = observers,
    controls = controls,
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
  if (!is.null(b$input_ids)) {
    str <- paste0(str, "  Inputs: ", paste(b$input_ids, collapse = ", "), "\n")
  }
  if (!is.null(b$observers)) {
    str <- paste0(str, "  Observers: ", length(b$observers), "\n")
  }
  if (!is.null(b$controls)) {
    str <- paste0(str, "  Controls: ", length(b$controls), "\n")
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
