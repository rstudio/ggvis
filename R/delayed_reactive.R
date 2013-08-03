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
  
  if (!is.null(controls)) {
    attr(controls, "id") <- id  
  }
  
  structure(list(fun = fun, controls = controls), class = "delayed_reactive")
}

is.delayed_reactive <- function(x) inherits(x, "delayed_reactive")

# Returns a function that takes a session object and returns
# session$input[[id]], or, if it's not present, a default value.
from_input <- function(id, default) {
  call <- substitute(function(session) {
      session$input[[id]] %||% default
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
