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

  plot_id <- "plot1"

  template <- paste(readLines(system.file('index.html', package='ggvis')),
    collapse='\n')

  head <- tagList(
    tags$script(src = "lib/jquery-1.9.1.js"),
    tags$script(src = "lib/jquery-ui/js/jquery-ui-1.10.3.custom.js"),
    tags$link(rel = "stylesheet", type = "text/css",
      href = "shared/bootstrap/css/bootstrap.min.css"),
    tags$link(rel = "stylesheet", type = "text/css",
      href = "shared/bootstrap/css/bootstrap-responsive.min.css"),
    tags$script(src = "shared/bootstrap/js/bootstrap.min.js"),
    tags$script(charset = "utf-8", src = "lib/d3.js"),
    tags$script(src = "lib/vega.js"),
    tags$script(src = "lib/QuadTree.js"),
    tags$script(src = "js/ggvis.js"),
    tags$link(rel = "stylesheet", type = "text/css",
      href = "css/ggvis.css"),
    tags$link(rel = "stylesheet", type = "text/css",
      href = "lib/jquery-ui/css/smoothness/jquery-ui-1.10.3.custom.css")
  )

  body <- divWithSidebar(
    headerPanel("ggvis plot"),
    sidebarPanel(
      ggvisControlGroup(plot_id)
    ),
    mainPanel(
      div(id = plot_id, class = "ggvis-output"),
      tags$script(type = "text/javascript",
        paste0('
          var spec = ', vega_json, ';
          var plot = ggvis.getPlot("', plot_id, '");

          ggvis.renderer = "', renderer, '";
          ggvis.setRendererChooser(ggvis.renderer);
          ggvis.updateDownloadButtonText();

          plot.parseSpec(spec, "', renderer, '");
        ')
      )
    )
  )

  body <- format(body)
  
  html_file <- file.path(temp_dir, "plot.html")
  writeLines(whisker.render(template, list(head = head, body = body)),
    con = html_file)
  
  if (launch) view_app(html_file)
  invisible(html_file)
}

copy_www_resources <- function(destdir) {
  # Copies a file/dir from an installed package to the destdir (with path)
  copy_www_file <- function(file, pkg) {
    src <- system.file("www", file, package = pkg)

    destfile <- file.path(destdir, file)
    parent_dir <- dirname(destfile)
    if (!dir.exists(parent_dir))
      dir.create(parent_dir, recursive = TRUE)

    if (file.info(src)$isdir) {
      file.copy(src, dirname(destfile), recursive = TRUE)
    } else {
      file.copy(src, destfile)
    }
  }

  shiny_files <- c(
    "shared/bootstrap/css/bootstrap.min.css",
    "shared/bootstrap/js/bootstrap.min.js",
    "shared/bootstrap/css/bootstrap-responsive.min.css"
  )
  ggvis_files <- c(
    "lib/jquery-1.9.1.js",
    "lib/d3.js",
    "lib/vega.js",
    "lib/QuadTree.js",
    "lib/jquery-ui/js/jquery-ui-1.10.3.custom.js",
    "lib/jquery-ui",
    "js/ggvis.js",
    "css/ggvis.css"
  )

  lapply(shiny_files, copy_www_file, pkg = "shiny")
  lapply(ggvis_files, copy_www_file, pkg = "ggvis")
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
      ggvisControlGroup(plot_id)
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
    runApp(app, launch.browser = view_app)
  } else {
    app
  }
}

# Check for either a user-specified ggvis.viewapp function or for one provided
# by the R front-end (RStudio >= v0.98.414 is known to do this, others may as
# well). Fallback to utils::browseURL if there is no viewapp option provided.
view_app <- function(url) {
  viewer <- getOption("ggvis.viewapp", getOption("viewapp", browseURL))
  viewer(url)
}


