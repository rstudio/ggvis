#' Connect a ggvis graphic to a shiny app.
#' 
#' It's easiest to learn by example: there are two shiny apps in 
#' \code{demo/apps/} that you can learn from.
#' 
#' @details
#' There are two required components and one optional component:
#' 
#' \itemize{
#'   \item Use \code{ggvis_output} in \code{ui.r} to insert a placeholder 
#'   (a div with id) for a ggvis graphic.
#'
#'   \item Use \code{observe_ggvis} in \code{server.r} to insert a ggvis object
#'   into a shiny app and set up the observers to notify the client side 
#'   whenever the plot data or spec changes.
#' 
#'   \item If the plot uses interactive inputs, use \code{renderControls} to
#'   insert those controls into the ui.
#' }
#' @examples
#' \dontrun{
#' # In server.r
#' gv <- reactive({
#'   ggvis(mtcars, props(x = ~ wt, y = ~ mpg),
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
#' @name shiny
NULL

#' @rdname shiny
#' @importFrom shiny addResourcePath singleton tagList
#' @param id unique identifier to use for div tag containing ggvis plot
#' @export
ggvis_output <- function(id) {
  addResourcePath("ggvis", system.file("www", package = "ggvis"))

  tagList(
    singleton(tags$head(
      tags$script(src = "ggvis/lib/jquery-1.9.1.js"),
      tags$script(src = "ggvis/lib/jquery-ui/js/jquery-ui-1.10.3.custom.js"),
      tags$script(src = "ggvis/lib/d3.js"),
      tags$script(src = "ggvis/lib/vega.js"),
      tags$script(src = "ggvis/lib/QuadTree.js"),
      tags$script(src = "ggvis/js/ggvis.js"),
      tags$script(src = "ggvis/js/shiny-ggvis.js"),
      tags$link(rel = "stylesheet", type = "text/css",
                href = "ggvis/css/ggvis.css"),
      tags$link(rel = "stylesheet",
                type = "text/css",
                href = "ggvis/lib/jquery-ui/css/smoothness/jquery-ui-1.10.3.custom.css")
    )),
    tags$div(id = id, class = "ggvis-output")
  )
}

#' @rdname shiny
#' @param r_gv A reactive expression which returns a ggvis object.
#' @param session A Shiny session object.
#' @param renderer The renderer type ("canvas" or "svg")
#' @param ... Other arguments passed to \code{as.vega}.
#' @export
observe_ggvis <- function(r_gv, id, session, renderer = "canvas", ...) {
  if (!is.reactive(r_gv)) {
    stop("observe_ggvis requires a reactive expression that returns a ggvis object",
      call. = FALSE)
  }
  r_spec <- reactive(as.vega(r_gv(), session = session, dynamic = TRUE, ...))

  observe_spec(r_spec, id, session, renderer)
  observe_data(r_spec, id, session)
}

# Create an observer for a reactive vega spec
observe_spec <- function(r_spec, id, session, renderer) {
  obs <- observe({
    session$sendCustomMessage("ggvis_vega_spec", list(
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
  # A list for keeping track of each data observer
  data_observers <- list()

  obs_all <- observe({
    # If data_observers list is nonempty, that means there are old observers
    # which need to be suspended before we create new ones.
    for (obs in data_observers) obs$suspend
    data_observers <<- list()

    data_table <- attr(r_spec(), "data_table")

    # Create observers for each of the data objects
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

          session$sendCustomMessage("ggvis_data", list(
            plotId = id,
            name = data_name,
            value = as.vega(data_reactive(), data_name)
          ))
        })
        session$onSessionEnded(function() {
          obs$suspend()
        })

        # Track this data observer
        data_observers[[length(data_observers) + 1]] <<- obs
      })
    }
  })
  session$onSessionEnded(function() {
    obs_all$suspend()
  })
}

#' @rdname shiny
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
