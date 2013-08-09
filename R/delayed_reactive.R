#' Create a delayed reactive.
#' 
#' A delayed reactive allows us to specify reactive values outside of a shiny
#' app. They are used to specify interactive behaviour 
#' (e.g. \code{\link{input_slider}}) that is instantiated when the visualisation
#' is launched.
#' 
#' @param fun Must always return a value - see \code{from_input} one way of
#'   ensuring the function yields a value even before the reactiveValues have
#'   be initialised for the first time by user input. It is passed either no
#'   arguments, or if it has a single argument called session, the shiny
#'   session object (from which input and output can be extract)
#' @param controls a control object
#' @param id a unique identifier for this reactive - used to de-duplicate the
#'  controls when the same delayed reactive is used in multiple places in a
#'  visualisation
#' @export
#' @keywords internal
delayed_reactive <- function(fun, controls = NULL, id = rand_id()) {
  stopifnot(is.function(fun))

  structure(list(fun = fun, controls = controls, id = id),
    class = "delayed_reactive")
}

is.delayed_reactive <- function(x) inherits(x, "delayed_reactive")

# Returns a function that takes a session object and returns
# session$input[[id]], or, if it's not present, a default value.
# @param id The id of something in the input object, like input[["foo"]].
# @param default A default value that is returned when session$input[[id]] is null.
# @param wrapfun A function that takes the raw input$foo value as input, and
#   returns a value. This is useful when the raw input value needs to be
#   massaged before passing it on to the next stage of processing.
from_input <- function(id, default, wrapfun = identity) {

  call <- substitute(function(session) {
    wrapfun(session$input[[id]] %||% default)
  }, list(id = id, default = default))

  eval(call)
}

#' @S3method print delayed_reactive
print.delayed_reactive <- function(x, ...) {
  cat("<delayed_reactive>\n")
  print(body(x$fun))
}

#' @S3method as.reactive delayed_reactive
as.reactive.delayed_reactive <- function(x, session = NULL, ...) {
  if ("session" %in% names(formals(x$fun))) {
    reactive(x$fun(session = session))
  } else {
    reactive(x$fun())
  }
}
