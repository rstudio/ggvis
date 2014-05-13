#' HTML dependencies of a ggvis plot
#'
#' There are five way to display a ggvis plot:
#' \itemize{
#'   \item static html file
#'   \item temporary shiny app
#'   \item static html embedded in Rmarkdown
#'   \item shiny app embedded in Rmarkdown
#'   \item embedded in regular shiny app
#' }
#' This function ensures that the right dependencies are specified for
#' each scenario.
#'
#' @param absolute Use absolute paths? Needed for local html files.
#' @param in_shiny Will the plot be embedded in a shiny app? Drops jquery
#'   dependency and adds shiny resource paths
#' @param dynamic Is this a dynamic plot? If so, add shiny-ggvis dependency
#' @export
#' @keywords internal
ggvis_dependencies <- function(absolute = FALSE, in_shiny = FALSE,
                               dynamic = TRUE) {

  minified <- getOption("ggvis.js_minified", TRUE)
  adjust_min <- function(x) {
    if (minified) return(x)
    gsub("\\.min", "", x)
  }
  adjust_path <- function(x) {
    if (!absolute) return(x)
    system.file(package = "ggvis", "www", x)
  }

  if (in_shiny) {
    shiny::addResourcePath("ggvis", system.file("www", "ggvis", package = "ggvis"))
    shiny::addResourcePath("lib", system.file("www", "lib", package = "ggvis"))
  }

  deps <- compact(list(
    if (!in_shiny) html_dependency(
      name = "jquery",
      version = "1.11.0",
      path = adjust_path("lib/jquery"),
      script = "jquery.min.js"
    ),
    html_dependency(
      name = "jquery-ui",
      version = "1.10.4",
      path = adjust_path("lib/jquery-ui"),
      script = adjust_min("js/jquery-ui-1.10.4.custom.min.js"),
      stylesheet = adjust_min("css/smoothness/jquery-ui-1.10.4.custom.min.css")
    ),
    html_dependency(
      name = "d3",
      version = "3.4.1",
      path = adjust_path("lib/d3"),
      script = adjust_min("d3.min.js")
    ),
    html_dependency(
      name = "vega",
      version = "1.3.3",
      path = adjust_path("lib/vega"),
      script = adjust_min("vega.min.js")
    ),
    html_dependency(
      name = "lodash",
      version = "2.2.1",
      path = adjust_path("lib/lodash"),
      script = adjust_min("lodash.min.js"),
      head = "<script>var lodash = _.noConflict();</script>"
    ),
    html_dependency(
      name = "ggvis",
      version = as.character(packageVersion("ggvis")),
      path = adjust_path("ggvis"),
      script = "js/ggvis.js",
      stylesheet = "css/ggvis.css"
    ),
    if (dynamic) html_dependency(
      name = "shiny-ggvis",
      version = as.character(packageVersion("ggvis")),
      path = adjust_path("ggvis"),
      script = "js/shiny-ggvis.js"
    )
  ))

  deps
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

ggvis_app <- function(x, plot_id = rand_id("plot_"),
                      deps = ggvis_dependencies(in_shiny = TRUE),
                      ...) {

  ui <- ggvis_ui(plot_id, length(x$controls) > 0, deps = deps)

  server <- function(input, output, session) {
    r_gv <- reactive(x)
    bind_shiny(r_gv, session = session, plot_id = plot_id,
      controls_id = "ggvis_controls")
  }

  options <- compact(list(...))

  shiny::shinyApp(ui = ui, server = server, options = options)
}
