#' Create an interactive input.
#'
#' An interactive input represents a reactive value bound to a UI control.
#' Interactive inputs were previously called delayed reactives because they
#' represent an reactive value that would be created when the plot was
#' drawn.
#'
#' @param class The name of a class to be used in addition to
#'   "input". This is useful for functions that generate controls
#'   from the object.
#' @param fun Must always return a value - see \code{from_input} one way of
#'   ensuring the function yields a value even before the reactiveValues have
#'   be initialised for the first time by user input. It is passed either no
#'   arguments, or if it has a single argument called session, the shiny
#'   session object (from which input and output can be extract)
#' @param controls a control object
#' @param id a unique identifier for this reactive - used to de-duplicate the
#'  controls when the same interactive input is used in multiple places in a
#'  visualisation
#' @export
#' @keywords internal
input <- function(subclass, fun, controls = NULL, id = rand_id()) {
  stopifnot(is.function(fun))

  structure(list(fun = fun, controls = controls, id = id),
    class = c(subclass, "input"))
}

#' @export
#' @rdname input
#' @param x object to test for "input"-ness
is.input <- function(x) inherits(x, "input")

# Returns a function that takes a session object and returns
# session$input[[id]], or, if it's not present, a default value.
# @param id The id of something in the input object, like input[["foo"]].
# @param default A default value that is returned when session$input[[id]] is null.
# @param map A function that takes the raw input$foo value as input, and
#   returns a value. This is useful when the raw input value needs to be
#   massaged before passing it on to the next stage of processing.
from_input <- function(id, default, map = identity) {

  call <- substitute(function(session) {
    map(session$input[[id]] %||% default)
  }, list(id = id, default = default))

  eval(call)
}

#' @S3method print input
print.input <- function(x, ...) {
  cat("<input>\n")
  print(body(x$fun))
}

#' @S3method as.reactive input
as.reactive.input <- function(x, session = NULL, ...) {
  if ("session" %in% names(formals(x$fun))) {
    reactive(x$fun(session = session))
  } else {
    reactive(x$fun())
  }
}


# Convert delayed reactives to regular reactives
init_inputs <- function(x, session) {
  UseMethod("init_inputs")
}

#' @S3method init_inputs default
init_inputs.default <- function(x, session) x

#' @S3method init_inputs NULL
init_inputs.NULL <- function(x, session) NULL

#' @S3method init_inputs list
init_inputs.list <- function(x, session) {
  drs <- vapply(x, is.input, logical(1))
  x[drs] <- lapply(x[drs], as.reactive, session = session)
  x
}
#' @S3method init_inputs transform
init_inputs.transform <- init_inputs.list

#' @importFrom shiny is.reactive
#' @S3method init_inputs prop
init_inputs.prop <- function(x, session) {
  if (x$type != "reactive") return(x)
  if (is.reactive(x$value)) stop("Delayed reactive has already been advanced.")

  x$value <- as.reactive(x$dr, session)
  x
}

#' @S3method init_inputs ggvis_props
init_inputs.ggvis_props <- function(x, session) {
  x[] <- lapply(x, init_inputs, session = session)
  x
}


# Convert reactives to values
eval_reactives <- function(x) {
  is_function <- vapply(x, is.function, logical(1))
  x[is_function] <- lapply(x[is_function], function(f) f())
  x
}
