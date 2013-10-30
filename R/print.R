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
view_static <- function(x, 
                        renderer = getOption("ggvis.renderer", default="canvas"), 
                        launch = interactive()) {
  
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
      href = "lib/jquery-ui/css/smoothness/jquery-ui-1.10.3.custom.css"),
    tags$link(rel = "stylesheet", type = "text/css",
      href = "css/ggvis.css")
  )

  body <- tagList(
    ggvis_output(plot_id, shiny = FALSE),
    tags$script(type = "text/javascript",
      paste0('
        var spec = ', vega_json, ';
        var plot = ggvis.getPlot("', plot_id, '");
        plot.parseSpec(spec);
      ')
    )
  )

  body <- format(body)
  
  html_file <- file.path(temp_dir, "plot.html")
  writeLines(whisker.render(template, list(head = head, body = body)),
    con = html_file)
  
  if (launch) view_plot(html_file, 350)
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
    "shared/bootstrap/img/glyphicons-halflings.png",
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
view_dynamic <- function(x, 
                         renderer = getOption("ggvis.renderer", default="canvas"), 
                         launch = TRUE, port = 8228) {
  
  if (!(renderer %in% c("canvas", "svg")))
    stop("renderer must be 'canvas' or 'svg'")
  
  plot_id <- "plot1"

  # Find number of control elements for the plot
  n_controls <- length(controls(x))

  if (n_controls == 0) {
    ui <- basicPage(ggvis_output(plot_id, shiny = TRUE))

  } else {
    ui <- sidebarBottomPage(
      sidebarBottomPanel(
        ggvisControlOutput("ggvis_controls", plot_id)
      ),
      mainTopPanel(
        ggvis_output(plot_id, shiny = TRUE)
      )
    )
  }

  server <- function(input, output, session) {
    r_gv <- reactive(x)
    # Set up observers for the spec and the data
    observe_ggvis(r_gv, plot_id, session, renderer)
    
    # User interface elements (in the sidebar). These must be added dynamically
    # (instead of being rendered statically in ui) because they are unique to
    # each session.
    output$ggvis_controls <- renderControls(r_gv, session)
  }
  
  app <- list(ui = ui, server = server)
  if (launch) {
    # Request 70 vertical pixels for each pair of control items, since there are
    # two on a row.
    height <- 350 + 70 * ceiling(n_controls / 2)

    suppressMessages(
      runApp(app, launch.browser = function(url) view_plot(url, height))
    )
  } else {
    app
  }
}

# View using either an internal viewer or fallback to utils::browseURL
view_plot <- function(url, height) {
  viewer <- getOption("viewer")
  if (!is.null(viewer) && getOption("ggvis.view_internal", TRUE))
    viewer(url, height)
  else
    utils::browseURL(url)
}


