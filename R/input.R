#' Create a new interactive "input" object.
#'
#' An interactive input object is a reactive expression which wraps a reactive
#' value. When the plot is rendered, an observer is created which pushes values
#' into the reactive value in response to changes of an input object. Those
#' changes invalidate the reactive expression, which will return the value,
#' optionally passed through a mapping function.
#'
#' This function is designed to be used by authors of new types of interactive
#' inputs. If you are a ggvis user, please use one of the more specific input
#' functions starting with the \code{input_}.
#'
#' @param id The name of the input object in the Shiny app, such as
#'   "slider_1338869".
#' @param default The default (starting) value for the input.
#' @param map A mapping function. Defaults to \code{identity}, which simply
#'   returns the value unchanged.
#' @param controls A Shiny HTML tag object representing the UI for the controls.
#' @export
#' @keywords internal
create_input <- function(id = rand_id("input_"), default = default,
                         map = identity, controls = NULL) {

  # Create a reactivevalues object to store the value. When a plot is rendered,
  # an observer will be set up to push values into val$x.
  val <- shiny::reactiveValues(x = default)
  # A reactive to wrap the reactive value
  res <- reactive({
    map(val$x)
  })

  # The input_id will be used to connect input$foo to this object's val$x
  attr(res, "input_id") <- id
  attr(res, "val") <- val
  # HTML controls
  attr(res, "controls") <- controls
  class(res) <- c("input", class(res))

  res
}


#' Determine if an object is an input object
#'
#' @param x An object to test.
#' @export
is.input <- function(x) inherits(x, "input")

# Given a list of reactives, return a list of input val objects from the
# reactives.
extract_input_vals <- function(reactives) {
  vals <- lapply(reactives, extract_input_val)
  # Get the input IDs, which are different from the reactive IDs
  names(vals) <- lapply(reactives, input_id)
  compact(vals)
}

extract_input_val <- function(x) {
  attr(x, "val")
}

input_id <- function(x) {
  attr(x, "input_id")
}

`input_id<-` <- function(x, value) {
  attr(x, "input_id") <- value
  x
}
