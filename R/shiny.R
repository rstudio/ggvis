#' Add a Gigvis plot to the UI of a Shiny app
#'
#' @importFrom shiny addResourcePath singleton tagList
#' @export
gigvisOutput <- function(id) {
  addResourcePath("gigvis", system.file("www", package = "gigvis"))

  tagList(
    singleton(tags$head(
      tags$script(src = "gigvis/lib/jquery-1.9.1.js"),
      tags$script(src = "gigvis/lib/jquery-ui-1.10.3.custom/js/jquery-ui-1.10.3.custom.js"),
      tags$script(src = "gigvis/lib/d3.js"),
      tags$script(src = "gigvis/lib/vega.js"),
      tags$script(src = "gigvis/lib/QuadTree.js"),
      tags$script(src = "gigvis/js/shiny-gigvis.js"),
      tags$link(rel = "stylesheet",
                type = "text/css",
                href = "gigvis/lib/jquery-ui-1.10.3.custom/css/smoothness/jquery-ui-1.10.3.custom.css")
    )),
    tags$div(id = id, class = "gigvis-output")
  )
}

#' Set up Shiny observers for a dynamic gigvis plot
#'
#' @export
observeGigvis <- function(gv, id, session, ...) {
  spec <- as.vega(gv, session = session, dynamic = TRUE, ...)

  observe_spec(spec, id, session)
  observe_data(attr(spec, "data_table"), id, session)
}

# Create an observer for the vega spec
observe_spec <- function(spec, id, session) {
  obs <- observe({
    session$sendCustomMessage("gigvis_vega_spec", list(
      plotId = id,
      spec = spec
    ))
  })
  session$onSessionEnded(function() {
    obs$suspend()
  })
}

# Create observers for the data objects
observe_data <- function(data_table, id, session) {
  # Send each of the data objects
  for (name in ls(data_table, all.names = TRUE)) {
    # The datasets list contains named objects. The names are synthetic IDs
    # that are present in the vega spec. The values can be a variety of things,
    # see the if/else clauses below.
    local({
      # Have to do everything in a local so that these variables are not shared
      # between the different iterations
      data_name <- name

      obs <- observe({
        data_reactive <- get(data_name, data_table)
        data <- data_reactive()

        session$sendCustomMessage("gigvis_data", list(
          plot = id,
          name = data_name,
          value = as.vega(data, data_name)
        ))
      })
      session$onSessionEnded(function() {
        obs$suspend()
      })
    })
  }
}

#' Render the controls for a gigvis object in a Shiny app
#'
#' @examples
#' \dontrun{
#' # In server.r
#' gv <- gigvis(mtcars, props(x ~ wt, y ~ mpg),
#'   mark_symbol(),
#'   branch_smooth(
#'     n = input_slider(2, 80, "Interpolation points", value = 5, step = 1),
#'     method = input_select(c("Linear" = "lm", "LOESS" = "loess"))
#'   )
#' )
#'
#' output$controls <- renderControls(gv)
#' }
#' @export
renderControls <- function(gv, session = NULL) {
  renderUI({
    controls <- controls(gv, session)
    if (empty(controls)) {
      NULL
    } else {
      tagList(controls)
    }
  })
}
