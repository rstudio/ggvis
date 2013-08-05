# This is used similarly to view_static, but view_dynamic can take functions
# or (reactive expressions) as data instead of names in an environment.

#' @import shiny
view_dynamic <- function(gv, envir = parent.frame(), controls = NULL,
                         renderer = "canvas", launch = TRUE, port = 8228) {

  if (!(renderer %in% c("canvas", "svg")))
    stop("renderer must be 'canvas' or 'svg'")

  plot_id <- "plot1"

  # Make our resources available
  script_tags <- deploy_www_resources()
  ui <- pageWithSidebar(
    headerPanel("Gigvis plot"),
    sidebarPanel(
      uiOutput("gigvis_ui")
    ),
    mainPanel(
      tags$head(deploy_www_resources()),

      # Placeholder for the plot
      tags$div(id = plot_id),

      # Add an actionButton that quits the app and closes the browser window
      tags$button(id="quit", type="button", class="btn action-button",
        onclick = "window.close()", "Quit")
    )
  )

  server <- function(input, output, session) {

    # Do the preprocessing steps for gigvis
    spec <- as.vega(gv, session = session, dynamic = TRUE)
    data_table <- attr(spec, "data_table")

    # Send the vega spec
    spec_obs <- observe({
      session$sendCustomMessage("gigvis_vega_spec", list(
        plotId = plot_id,
        spec = spec
      ))
    })
    session$onSessionEnded(function() {
      spec_obs$suspend()
    })

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
            plot = plot_id,
            name = data_name,
            value = as.vega(data, data_name)
          ))
        })
        session$onSessionEnded(function() {
          obs$suspend()
        })
      })
    }

    # Stop the app when the quit button is clicked
    observe({
      if (is.null(input$quit)) return()
      if (input$quit > 0) stopApp()
    })

    # User interface elements (in the sidebar)
    controls <- controls(gv)
    if (!empty(controls)) {
      output$gigvis_ui <- renderUI({
        tagList(controls)
      })      
    }
  }

  runApp(list(ui = ui, server = server))
}


#' @importFrom shiny addResourcePath
deploy_www_resources <- function() {
  files <- c(
    "lib/jquery-1.9.1.js",
    "lib/d3.js",
    "lib/vega.js",
    "lib/QuadTree.js",
    "js/shiny-gigvis.js"
  )

  addResourcePath("gigvis", system.file("www", package="gigvis"))
  lapply(files, function(file) {
    tags$script(src=paste("gigvis", file, sep="/"))
  })
}
