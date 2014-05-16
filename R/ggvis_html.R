#' @import htmltools
NULL

#' @export
format.html_dependency <- function(x, ...) {
  htmltools::renderDependencies(list(x), "file")
}

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

  deps <- compact(list(
    htmlDependency(
      name = "jquery",
      version = "1.11.0",
      src = ggvis_path("lib/jquery"),
      script = "jquery.min.js"
    ),
    htmlDependency(
      name = "jquery-ui",
      version = "1.10.4",
      src = ggvis_path("lib/jquery-ui"),
      script = adjust_min("js/jquery-ui-1.10.4.custom.min.js"),
      stylesheet = adjust_min("css/smoothness/jquery-ui-1.10.4.custom.min.css")
    ),
    htmlDependency(
      name = "d3",
      version = "3.4.1",
      src = ggvis_path("lib/d3"),
      script = adjust_min("d3.min.js")
    ),
    htmlDependency(
      name = "vega",
      version = "1.3.3",
      src = ggvis_path("lib/vega"),
      script = adjust_min("vega.min.js")
    ),
    htmlDependency(
      name = "lodash",
      version = "2.2.1",
      src = ggvis_path("lib/lodash"),
      script = adjust_min("lodash.min.js"),
      head = "<script>var lodash = _.noConflict();</script>"
    ),
    htmlDependency(
      name = "ggvis",
      version = as.character(packageVersion("ggvis")),
      src = ggvis_path("ggvis"),
      script = "js/ggvis.js",
      stylesheet = "css/ggvis.css"
    )
  ))

  deps
}

shiny_dependency <- htmlDependency(
  name = "shiny-ggvis",
  version = as.character(packageVersion("ggvis")),
  src = ggvis_path("ggvis"),
  script = "js/shiny-ggvis.js"
)

ggvis_app <- function(x, plot_id = rand_id("plot_"),
                      ...) {

  ui <- attachDependencies(
    list(ggvisPage(plot_id, length(x$controls) > 0)),
    shiny_dependency
  )

  server <- function(input, output, session) {
    r_gv <- reactive(x)
    bind_shiny(r_gv, session = session, plot_id = plot_id,
      controls_id = "ggvis_controls")
  }

  options <- compact(list(...))

  shiny::shinyApp(ui = ui, server = server, options = options)
}
