# * val
# * reactives (list)
#   * [default] reactive 1 + input_id
#   * reactive 2 + input_id
# * observers (list)
#   * observer 1 + input_id
#   * observer 2 + input_id
# * controls (list)
#   * Control 1
#   * control 2
# * stuff to put in spec

# @param r A reactive expression
# @param vals A reactiveValues object
# @param input_ids A character vector of input IDs. These input IDs will be
#   present in session$input, as well as in val, and in the as.vega stage,
#   observers will be created which push values from session$input to val,
#   for each ID.
# @param observers An observer, or a list of observers
# @param controls An HTML control, or a list of HTML controls
# @param spec Object to put in the Vega spec
create_broker <- function(r, vals = NULL, input_ids = NULL, observers = NULL,
                          controls = NULL, spec = NULL) {
  if (!shiny::is.reactive(r)) stop("r must be a reactive expression.")
  if (!shiny::is.reactivevalues(vals)) stop("vals must be a reactivevalues object.")

  # If passed bare observer or control, wrap them into list
  if (!is.null(observers) && inherits(obs, "Observer")) {
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
  str <- ""
  if (!is.null(input_ids)) {
    str <- paste0(str, "\n", paste(input_ids, collapse = ", "))
  }
  if (!is.null(observers)) {


  }

}


# Given a list of reactives, extract the broker objects from attributes
extract_brokers <- function(reactives) {
  compact(lapply(reactives, function(x) attr(x, "broker")))
}
