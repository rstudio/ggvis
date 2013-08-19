#' Create a delayed reactive.
#'
#' A delayed reactive allows us to specify reactive values outside of a shiny
#' app. They are used to specify interactive behaviour
#' (e.g. \code{\link{input_slider}}) that is instantiated when the visualisation
#' is launched.
#'
#' @param class The name of a class to be used in addition to
#'   "delayed_reactive". This is useful for functions that generate controls
#'   from the object.
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
delayed_reactive <- function(class = NULL, fun, controls = NULL,
                             id = rand_id()) {
  stopifnot(is.function(fun))

  structure(list(fun = fun, controls = controls, id = id),
    class = c(class, "delayed_reactive"))
}

is.delayed_reactive <- function(x) inherits(x, "delayed_reactive")

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


# Convert delayed reactives to regular reactives
advance_delayed_reactives <- function(x, session) {
  UseMethod("advance_delayed_reactives")
}

#' @S3method advance_delayed_reactives default
advance_delayed_reactives.default <- function(x, session) x

#' @S3method advance_delayed_reactives NULL
advance_delayed_reactives.NULL <- function(x, session) NULL

#' @S3method advance_delayed_reactives list
advance_delayed_reactives.list <- function(x, session) {
  drs <- vapply(x, is.delayed_reactive, logical(1))
  x[drs] <- lapply(x[drs], as.reactive, session = session)
  x
}
#' @S3method advance_delayed_reactives transform
advance_delayed_reactives.transform <- advance_delayed_reactives.list

#' @importFrom shiny is.reactive
#' @S3method advance_delayed_reactives prop
advance_delayed_reactives.prop <- function(x, session) {
  if (!x$reactive) return(x)
  if (is.reactive(x$value)) stop("Delayed reactive has already been advanced.")

  x$value <- as.reactive(x$dr, session)
  x
}

#' @S3method advance_delayed_reactives ggvis_props
advance_delayed_reactives.ggvis_props <- function(x, session) {
  x[] <- lapply(x, advance_delayed_reactives, session = session)
  x
}


# Convert reactives to values
eval_reactives <- function(x) {
  is_function <- vapply(x, is.function, logical(1))
  x[is_function] <- lapply(x[is_function], function(f) f())
  x
}
