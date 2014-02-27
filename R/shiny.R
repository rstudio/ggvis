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
#' ## In server.r
#' gv <- reactive({
#'   ggvis(mtcars, props(x = ~wt, y = ~mpg),
#'     layer_point(),
#'     layer_smooth(
#'       n = input_slider(2, 80, "Interpolation points", value = 5, step = 1),
#'       method = input_select(c("Linear" = "lm", "LOESS" = "loess"))
#'     )
#'   )
#' })
#'
#' output$controls <- renderControls(gv)
#'
#'
#' ## In ui.r
#' ggvisControlOutput("controls")
#' }
#' @name shiny
NULL

#' @rdname shiny
#' @importFrom shiny addResourcePath singleton tagList div
#' @param plot_id unique identifier to use for the div containing the ggvis plot.
#' @param shiny Should this include headers for Shiny? For dynamic and
#'   interactive plots, this should be TRUE; otherwise FALSE.
#' @param minify If \code{TRUE}, use minified version of JS and CSS files. This
#'   can be useful for debugging.
#' @export
ggvis_output <- function(plot_id, shiny = TRUE, minify = TRUE) {
  container <-
    div(id = paste0(plot_id, "-container"), class = "ggvis-output-container",
      # Div containing the plot
      div(id = plot_id, class = "ggvis-output"),

      div(class = "plot-gear-icon",
        ggvisControlGroup(plot_id)
      )
    )


  if (shiny) {
    suppressMessages(
      addResourcePath("ggvis", system.file("www", package = "ggvis"))
    )

    tagList(
      singleton(tags$head(
        html_head(prefix = "ggvis", minify = minify, shiny = TRUE)
      )),
      container
    )

  } else {
    container
  }
}

#' @rdname shiny
#' @param r_gv A reactive expression which returns a ggvis object.
#' @param session A Shiny session object.
#' @param ... Other arguments passed to \code{as.vega}.
#' @export
observe_ggvis <- function(r_gv, plot_id, session, ...) {
  if (!is.reactive(r_gv)) {
    stop("observe_ggvis requires a reactive expression that returns a ggvis object",
      call. = FALSE)
  }
  r_spec <- reactive(as.vega(r_gv(), session = session, dynamic = TRUE, ...))

  observe_spec(r_spec, plot_id, session)
  observe_data(r_spec, plot_id, session)
}

# Create an observer for a reactive vega spec
observe_spec <- function(r_spec, id, session) {
  obs <- observe({
    session$sendCustomMessage("ggvis_vega_spec", list(
      plotId = id,
      spec = r_spec()
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
    for (obs in data_observers) obs$suspend()
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
      # Wrap each control in a div, for layout purposes
      tagList(
        lapply(controls, function(x) div(x, class = "ggvis-input-container"))
      )
    }
  })
}


#' Create a page with a sidebar
#'
#' This creates a page with a sidebar, where the sidebar moves to the bottom
#' when the width goes below a particular value.
#'
#' @param sidebarPanel The \code{\link{sidebarBottomPanel}} containing input
#'   controls.
#' @param mainPanel The \code{\link{mainTopPanel}} containing the main content.
#' @param shiny_headers Should Shiny headers be embedded in the page? This
#'   should be TRUE for interactive/dynamic pages, FALSE for static pages.
#' @importFrom shiny bootstrapPage
#' @export
#' @examples
#' sidebarBottomPage
sidebarBottomPage <- function(sidebarPanel, mainPanel, shiny_headers = TRUE) {
  content <- div(
    class = "container-fluid",
    div(class = "row-fluid",
      mainPanel,
      sidebarPanel
    )
  )

  if (shiny_headers) {
    bootstrapPage(content)
  } else {
    content
  }
}

#' Create a sidebar panel which moves to the bottom
#'
#' This is to be used with \code{link{sidebarBottomPage}}.
#'
#' @param ... UI elements to include in the sidebar.
#' @export
sidebarBottomPanel <- function(...) {
  div(class = "span4 sidebar-bottom",
    tags$form(class = "well well-small",
      ...
    )
  )
}


#' Create a main panel which moves to the top
#'
#' This is to be used with \code{link{sidebarBottomPage}}.
#'
#' @param ... UI elements to include in the main panel.
#' @export
mainTopPanel <- function(...) {
  div(class = "span8 main-top",
    ...
  )
}

#' Generate Shiny tags for ggvis controls
#'
#' Controls for choosing a renderer and downloading an image.
#' @param plot_id Plot ID
#' @export
ggvisControlGroup <- function(plot_id) {
  tags$nav(class = "ggvis-control",
    tags$a(class = "dropdown-toggle", title = "Controls", "\u2699"),
    tags$ul(class = "dropdown",
      tags$li(
        "Renderer: ",
        tags$a(
          id = paste0(plot_id, "_renderer_svg"),
          class = "ggvis-renderer-button",
          `data-plot-id` = plot_id,
          `data-renderer` = "svg",
          "SVG"
        ),
        " | ",
        tags$a(
          id = paste0(plot_id, "_renderer_canvas"),
          class = "ggvis-renderer-button",
          `data-plot-id` = plot_id,
          `data-renderer` = "canvas",
          "Canvas"
        )
      ),
      tags$li(tags$a(
        id = paste0(plot_id, "_download"),
        class = "ggvis-download",
        `data-plot-id` = plot_id,
        "Download"
      ))
    )
  )
}

#' Create a ggvis control output element in UI
#'
#' This is effectively the same as \code{\link[shiny]{uiOutput}}, except that
#' on the client side it may call some plot resizing functions after new
#' controls are drawn.
#'
#' \code{ggvisControlOutput} is intended to be used with
#' \code{\link{renderControls}} on the server side.
#'
#' @param outputId The output variable to read the value from.
#' @param plotId An optional plot ID or vector of plot IDs. The plots will
#'   have their .onControlOutput functions called after the controls are drawn.
#' @examples
#' ggvisControlOutput("plot1")
#' @export
ggvisControlOutput <- function(outputId, plotId = NULL) {
  if (is.null(plotId)) {
    div(id = outputId, class = "ggvis-control-output")

  } else {
    div(
      id = outputId,
      class = "ggvis-control-output",
      `data-plot-id` = paste(plotId, collapse = " ")
    )
  }
}
