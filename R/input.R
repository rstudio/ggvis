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
  vals$x <- default

  # A reactive to wrap the reactive value
  res <- reactive({
    map(vals$x)
  })

  # This function is run at render time. It takes values from session$input$foo
  # and pushes them into val$foo.
  connect <- function(session, plot_id) {
    shiny::observe({
      value <- session$input[[id]]
      if (is.null(value)) {
        # Need to explicitly set it to default when input is NULL, because some
        # inputs give NULL when they're cleared. (#272)
        vals$x <- default
      } else {
        vals$x <- value
      }
    })
  }
  connector_label(connect) <- paste0("<", id, ">")

  # Wrap the shiny tag object into a list
  controls_l <- list()
  controls_l[[id]] <- controls

  create_broker(res, connect = connect, controls = controls_l)
}
