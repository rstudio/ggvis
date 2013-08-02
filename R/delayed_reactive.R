#' @param fun Must always return a value - see \code{from_input} one way of
#'   ensuring the function yields a value even before the reactiveValues have
#'   be initialised for the first time by user input.
delayed_reactive <- function(fun, controls = NULL, id = rand_id()) {
  stopifnot(is.function(fun))
  attr(controls, "id") <- id

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
