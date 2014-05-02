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
create_input <- function(id = rand_id("input_"), default = NULL,
                         map = identity, controls = NULL) {

  # Create a reactivevalues object to store the value. When a plot is rendered,
  # an observer will be set up to push values into val$x.
  vals <- shiny::reactiveValues()
  vals[[id]] <- default

  # A reactive to wrap the reactive value
  res <- reactive({
    map(vals[[id]])
  })

  create_broker(res, vals = vals, input_ids = id, controls = controls)
}


input_id <- function(x) {
  attr(x, "input_id")
}

`input_id<-` <- function(x, value) {
  attr(x, "input_id") <- value
  x
}
