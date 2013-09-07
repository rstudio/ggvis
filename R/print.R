#' View in a ggvis plot in the browser.
#' 
#' \code{view_static} creates a static web page in a temporary directory;
#' \code{view_dynamic} generate a dynamic shiny app and launches it. Print 
#' picks between the two methods automatically.
#'
#' @param x A ggvis object.
#' @param dynamic Uses \code{view_dynamic} if \code{TRUE}, \code{view_static} if
#'   \code{FALSE}. The default picks automatically based on the presence of
#'   reactives or interactive inputs.
#' @param ... Other arguments passed on to \code{view_dynamic} and 
#'   \code{view_static}
#' @param renderer The renderer to use in the browser. Can be \code{"canvas"}
#'   (the default) or \code{"svg"}.
#' @param launch If \code{TRUE}, launch this web page in a browser.
#' @param port the port on which to start the shiny app
#' @keywords internal
#' @method print ggvis
#' @export
print.ggvis <- function(x, dynamic = NA, ...) {
  set_last_vis(x)
  
  if (is.na(dynamic)) dynamic <- is.dynamic(x) && interactive()
  
  if (dynamic) {
    view_dynamic(x, ...)
  } else {
    view_static(x, ...)
  }
}

#' @rdname print.ggvis
#' @export
#' @importFrom RJSONIO toJSON
#' @importFrom whisker whisker.render
view_static <- function(x, renderer = "canvas", launch = interactive()) {
  
  if (!(renderer %in% c("canvas", "svg")))
    stop("renderer must be 'canvas' or 'svg'")
  
  temp_dir <- tempfile(pattern = "ggvis")
  dir.create(temp_dir)
  
  copy_www_resources(temp_dir)
  
  spec <- as.vega(x, dynamic = FALSE)
  vega_json <- toJSON(spec, pretty = TRUE)
  
  template <- paste(readLines(system.file('index.html', package='ggvis')),
    collapse='\n')
  
  js <- paste0(
    '<script type="text/javascript">
      // $(function(){ //DOM Ready
      ggvis.parseSpec(', vega_json, ', "vis");
      // });
    </script>')
  
  body <- paste('<div id="vis" class="ggvis-output"></div>', js, sep ='\n')
  
  html_file <- file.path(temp_dir, "plot.html")
  writeLines(whisker.render(template, list(head = '', body = body)),
    con = html_file)
  
  if (launch) browseURL(html_file)
  invisible(html_file)
}

copy_www_resources <- function(destdir) {
  files <- c(
    "lib/jquery-1.9.1.js",
    "lib/d3.js",
    "lib/vega.js",
    "lib/QuadTree.js",
    "lib/jquery-ui/js/jquery-ui-1.10.3.custom.js",
    "lib/jquery-ui",
    "js/ggvis.js",
    "css/ggvis.css"
  )
  
  lapply(files, function(file) {
    src <- system.file("www", file, package = "ggvis")

    destfile <- file.path(destdir, file)
    parent_dir <- dirname(destfile)
    if (!dir.exists(parent_dir))
      dir.create(parent_dir, recursive = TRUE)

    if (file.info(src)$isdir) {
      file.copy(src, dirname(destfile), recursive = TRUE)
    } else {
      file.copy(src, destfile)
    }
  })
}

#' @rdname print.ggvis
#' @export
#' @importFrom RJSONIO toJSON
#' @importFrom whisker whisker.render
#' @importFrom shiny pageWithSidebar headerPanel sidebarPanel uiOutput
#'   mainPanel tags observe runApp stopApp renderUI
view_dynamic <- function(x, renderer = "canvas", launch = TRUE, port = 8228) {
  
  if (!(renderer %in% c("canvas", "svg")))
    stop("renderer must be 'canvas' or 'svg'")
  
  plot_id <- "plot1"
  
  # Make our resources available
  ui <- pageWithSidebar(
    headerPanel("ggvis plot"),
    sidebarPanel(
      uiOutput("ggvis_ui"),
      
      tags$div(
        tags$label("Renderer:", `for` ="ggvis_renderer"),
        tags$select(id = "ggvis_renderer",
          tags$option(value = "canvas", "Canvas"),
          tags$option(value = "svg", "SVG")
      ),
      tags$div(
        # Add an actionButton that quits the app and closes the browser window
        tags$button(id="quit", type="button", class="btn action-button", "Quit"),
        # Add PNG download button
        tags$a(id = "ggvis_download", class = "btn", style = "float:right;",
          `data-plot-id` = plot_id, "Download")
        )
      )
    ),
    mainPanel(
      ggvis_output(plot_id)
    )
  )
  
  server <- function(input, output, session) {
    r_gv <- reactive(x)
    # Set up observers for the spec and the data
    observe_ggvis(r_gv, plot_id, session, renderer)
    
    # User interface elements (in the sidebar)
    output$ggvis_ui <- renderControls(r_gv, session)
    
    # Stop the app when the quit button is clicked
    observe({
      if (is.null(input$quit)) return()
      if (input$quit > 0) stopApp()
    })
  }
  
  app <- list(ui = ui, server = server)
  if (launch) {
    runApp(app)
  } else {
    app
  }
}
