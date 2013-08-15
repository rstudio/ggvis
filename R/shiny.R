#' Add a Gigvis plot to the UI of a Shiny app
#'
#' @importFrom shiny addResourcePath singleton tagList
#' @export
gigvisOutput <- function(id) {
  addResourcePath("gigvis", system.file("www", package = "gigvis"))

  tagList(
    singleton(tags$head(
      tags$script(src = "gigvis/lib/jquery-1.9.1.js"),
      tags$script(src = "gigvis/lib/jquery-ui/js/jquery-ui-1.10.3.custom.js"),
      tags$script(src = "gigvis/lib/d3.js"),
      tags$script(src = "gigvis/lib/vega.js"),
      tags$script(src = "gigvis/lib/QuadTree.js"),
      tags$script(src = "gigvis/js/shiny-gigvis.js"),
      tags$link(rel = "stylesheet", type = "text/css",
                href = "gigvis/css/gigvis.css"),
      tags$link(rel = "stylesheet",
                type = "text/css",
                href = "gigvis/lib/jquery-ui/css/smoothness/jquery-ui-1.10.3.custom.css")
    )),
    tags$div(id = id, class = "gigvis-output")
  )
}

#' Set up Shiny observers for a dynamic gigvis plot
#'
#' @param r_gv A reactive expression which returns a gigvis object.
#' @param id The ID of the plot on the web page.
#' @param session A Shiny session object.
#' @param renderer The renderer type ("canvas" or "svg")
#' @param ... Other arguments passed to \code{as.vega}.
#'
#' @export
observeGigvis <- function(r_gv, id, session, renderer = "canvas", ...) {
  if (!is.reactive(r_gv)) {
    stop("observeGigvis requires a reactive expression that returns a gigvis object",
      call. = FALSE)
  }
  r_spec <- reactive(as.vega(r_gv(), session = session, dynamic = TRUE, ...))

  observe_spec(r_spec, id, session, renderer)
  observe_data(r_spec, id, session)
}

# Create an observer for a reactive vega spec
observe_spec <- function(r_spec, id, session, renderer) {
  obs <- observe({
    session$sendCustomMessage("gigvis_vega_spec", list(
      plotId = id,
      spec = r_spec(),
      renderer = renderer
    ))
  })
  session$onSessionEnded(function() {
    obs$suspend()
  })
}

# Create observers for the data objects attached to a reactive vega spec
observe_data <- function(r_spec, id, session) {
  observe({
    data_table <- attr(r_spec(), "data_table")

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
            plotId = id,
            name = data_name,
            value = as.vega(data, data_name)
          ))
        })
        session$onSessionEnded(function() {
          obs$suspend()
        })
      })
    }
  })
}

#' Render the controls for a gigvis object in a Shiny app
#'
#' @examples
#' \dontrun{
#' # In server.r
#' gv <- reactive({
#'   gigvis(mtcars, props(x ~ wt, y ~ mpg),
#'     mark_symbol(),
#'     branch_smooth(
#'       n = input_slider(2, 80, "Interpolation points", value = 5, step = 1),
#'       method = input_select(c("Linear" = "lm", "LOESS" = "loess"))
#'     )
#'   )
#' })
#'
#' output$controls <- renderControls(gv)
#' }
#' @export
renderControls <- function(r_gv, session = NULL) {
  renderUI({
    controls <- controls(r_gv(), session)
    if (empty(controls)) {
      NULL
    } else {
      tagList(controls)
    }
  })
}
