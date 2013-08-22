#' Create an interactive input.
#'
#' An interactive input represents a reactive value bound to a UI control.
#' Interactive inputs were previously called delayed reactives because they
#' represent an reactive value that would be created when the plot was
#' drawn.
#'
#' @param subclass The name of a class to be used in addition to
#'   "input". Automatically prefixed with "input_"
#' @param control_args a list of arguments passed to \code{control_f}
#' @param value the default value of the input
#' @param map a function with a singe argument that takes the value returned
#'   from the input control and converts it to an argument useful for ggvis.
#'   Defaults to \code{identity}, leaving the output unchanged.
#' @param id a unique identifier for this interactive input - used to 
#'  de-duplicate the controls when the same input is used in multiple places 
#'  in a visualisation
#' @param scale default scale value when this input is used in a prop
#' @param control_f The name of a function used to create an html control.
#' @export
#' @keywords internal
input <- function(subclass, control_args = list(), value = NULL, 
                  map = identity, id = rand_id(), scale = FALSE,
                  control_f = NULL) {
  if (missing(subclass)) {
    stop("Input is a virtual class: you must provide a subclass name")
  }
  
  if (is.null(control_f)) {
    control_f <- camelCase(paste0(subclass, "Input"))
  }
  
  assert_that(is.string(subclass), is.string(control_f), 
    is.list(control_args), is.function(map), is.string(id),
    is.string(scale) || is.flag(scale))

  structure(list(
      control_f = control_f, 
      control_args = control_args, 
      default = value,
      map = map, 
      id = id,
      scale = scale
    ), class = c(paste0("input_", subclass), "input")
  )
}

#' @export
#' @rdname input
#' @param x object to test for "input"-ness
is.input <- function(x) inherits(x, "input")

#' @S3method controls input
controls.input <- function(x, session = NULL) {
  control <- do.call(x$control_f, x$control_args)
  setNames(list(control), x$id)
}

#' @S3method format input
format.input <- function(x, ...) {
  control <- as.call(c(as.name(x$control_f), x$control_args))
  control_s <- paste0(deparse(control), collapse = "\n")
  
  paste0("<input> ", x$id, "\n",
    control_s, "\n")
}

#' @S3method print input
print.input <- function(x, ...) {
  cat(format(x), "\n", sep = "")
}

#' @S3method as.reactive input
as.reactive.input <- function(x, session = NULL, ...) {
  f <- from_input(x$id, x$default, x$map)
    
  if ("session" %in% names(formals(f))) {
    reactive(f(session = session))
  } else {
    reactive(f())
  }
}

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
