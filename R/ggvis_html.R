#' @importFrom htmltools knit_print.shiny.tag knit_print.shiny.tag.list knit_print.html
NULL

ggvis_path <- function(x) {
  system.file(package = "ggvis", "www", x)
}

# HTML dependencies of a ggvis plot
ggvis_dependencies <- function() {

  minified <- getOption("ggvis.js_minified", TRUE)
  adjust_min <- function(x) {
    if (minified) return(x)
    gsub("\\.min", "", x)
  }

  deps <- list(
    htmltools::htmlDependency(
      name = "jquery",
      version = "1.11.0",
      src = ggvis_path("lib/jquery"),
      script = "jquery.min.js"
    ),
    htmltools::htmlDependency(
      name = "detect-resize",
      version = "0.5.3",
      src = ggvis_path("lib/detect-resize"),
      script = "jquery.resize.js"
    ),
    htmltools::htmlDependency(
      name = "jquery-ui",
      version = "1.11.4",
      src = ggvis_path("lib/jquery-ui"),
      script = adjust_min("jquery-ui.min.js"),
      stylesheet = adjust_min("jquery-ui.min.css")
    ),
    htmltools::htmlDependency(
      name = "d3",
      version = "3.5.2",
      src = ggvis_path("lib/d3"),
      script = adjust_min("d3.min.js")
    ),
    htmltools::htmlDependency(
      name = "vega",
      version = "1.4.3",
      src = ggvis_path("lib/vega"),
      script = adjust_min("vega.min.js")
    ),
    htmltools::htmlDependency(
      name = "lodash",
      version = "2.2.1",
      src = ggvis_path("lib/lodash"),
      script = adjust_min("lodash.min.js"),
      head = "<script>var lodash = _.noConflict();</script>"
    ),
    htmltools::htmlDependency(
      name = "ggvis",
      version = as.character(packageVersion("ggvis")),
      src = ggvis_path("ggvis"),
      script = "js/ggvis.js",
      stylesheet = "css/ggvis.css"
    )
  )

  deps
}

shiny_dependency <- function() {
  htmltools::htmlDependency(
    name = "shiny-ggvis",
    version = as.character(packageVersion("ggvis")),
    src = ggvis_path("ggvis"),
    script = "js/shiny-ggvis.js"
  )
}

ggvis_app <- function(x, plot_id = rand_id("plot_"),
                      ...) {

  ui <- ggvisLayout(plot_id, length(x$controls) > 0, spec = NULL, shiny = TRUE)

  server <- function(input, output, session) {
    r_gv <- reactive(x)
    bind_shiny(r_gv, session = session, plot_id = plot_id,
      controls_id = "ggvis_controls")
  }

  options <- compact(list(...))

  shiny::shinyApp(ui = ui, server = server, options = options)
}
