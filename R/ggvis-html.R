# embedded in shiny app
# console: static
# console: dynamic
# reactive doc: static
# reactive doc: dynamic

ggvis_dependencies <- function(minified = TRUE) {
  adjust_min <- if (minified) identity else function(x) gsub("\\.min", "", x)

  list(
    html_dependency(
      name = "jquery-ui",
      version = "1.10.4",
      path = "lib/jquery-ui",
      script = adjust_min("js/jquery-ui-1.10.4.custom.min.js"),
      stylesheet = adjust_min("css/smoothness/jquery-ui-1.10.4.custom.min.css")
    ),
    html_dependency(
      name = "d3",
      version = "3.4.1",
      path = "lib/d3",
      script = adjust_min("d3.min.js")
    ),
    html_dependency(
      name = "vega",
      version = "1.3.3",
      path = "lib/vega",
      script = adjust_min("vega.min.js")
    ),
    html_dependency(
      name = "lodash",
      version = "2.2.1",
      path = "lib/lodash",
      script = adjust_min("lodash.min.js"),
      head = "<script>var lodash = _.noConflict();</script>"
    ),
    html_dependency(
      name = "ggvis",
      version = as.character(packageVersion("ggvis")),
      path = "ggvis",
      script = "js/ggvis.js",
      stylesheet = "css/ggvis.css"
    )
  )
}

add_jquery_dep <- function(deps) {
  jquery <- html_dependency(
    name = "jquery",
    version = "1.11.0",
    path = "lib/jquery",
    script = "jquery.min.js"
  )
  c(list(jquery), deps)
}
add_shiny_ggvis_dep <- function(deps) {
  shiny_ggvis <- html_dependency(name = "shiny-ggvis",
    version = packageVersion("ggvis"),
    path = "ggvis",
    script = "js/shiny-ggvis.js")
  c(deps, list(shiny_ggvis))
}


ggvis_ui <- function(plot_id, has_controls = TRUE, spec = NULL, deps = NULL) {
  plot_div <- ggvisOutput(plot_id, spec = spec, deps = deps)

  if (!has_controls) {
    shiny::basicPage(plot_div)
  } else {
    shiny::bootstrapPage(
      sidebarBottomPage(
        sidebarBottomPanel(ggvisControlOutput("ggvis_controls", plot_id)),
        mainTopPanel(plot_div)
      )
    )
  }
}

ggvis_app <- function(x, plot_id = rand_id("plot_"), deps = ggvis_dependencies(), options = list()) {
  deps <- add_shiny_ggvis_dep(deps)

  ui <- ggvis_ui(plot_id, length(x$controls) > 0, deps = deps)
  shiny::addResourcePath("ggvis", system.file("www", "ggvis", package = "ggvis"))
  shiny::addResourcePath("lib", system.file("www", "lib", package = "ggvis"))

  server <- function(input, output, session) {
    r_gv <- reactive(x)
    bind_shiny(r_gv, session = session, plot_id = plot_id,
      controls_id = "ggvis_controls", renderer = renderer)
  }

  shiny::shinyApp(ui = ui, server = server, options = options)
}
