#' @rdname shiny-ggvis
#' @param plot_id unique identifier to use for the div containing the ggvis plot.
#' @param shiny Should this include headers for Shiny? For dynamic and
#'   interactive plots, this should be TRUE; otherwise FALSE.
#' @param spec Plot specification, used internally.
#' @param deps Default dependencies, used internally.
#' @export
ggvisOutput <- function(plot_id = rand_id("plot_id"), spec = NULL,
                        deps = ggvis_dependencies()) {
  shiny::tagList(
    ggvisPlot(plot_id),
    ggvisDependencies(deps),
    ggvisSpec(plot_id, spec)
  )
}

ggvisPlot <- function(plot_id) {
  shiny::div(id = paste0(plot_id, "-container"), class = "ggvis-output-container",
    # Div containing the plot
    shiny::div(id = plot_id, class = "ggvis-output"),
    shiny::div(class = "plot-gear-icon",
      ggvisControlGroup(plot_id)
    )
  )
}

ggvisDependencies <- function(deps) {
  head <- unlist(lapply(deps, format), use.names = FALSE)
  head_html <- lapply(head, shiny::HTML)

  shiny::singleton(shiny::tags$head(head_html))
}

ggvisSpec <- function(plot_id, spec = NULL) {
  if (is.null(spec)) return()
  json <- RJSONIO::toJSON(spec, pretty = TRUE)

  shiny::tags$script(type = "text/javascript", paste0('\n',
    'var ', plot_id, '_spec = ', json, ';\n',
    'ggvis.getPlot("', plot_id, '").parseSpec(', plot_id, '_spec);\n'
  ))
}

# Controls drop down
ggvisControlGroup <- function(plot_id) {
  shiny::tags$nav(class = "ggvis-control",
    shiny::tags$a(class = "ggvis-dropdown-toggle", title = "Controls", "\u2699"),
    shiny::tags$ul(class = "ggvis-dropdown",
      shiny::tags$li(
        "Renderer: ",
        shiny::tags$a(
          id = paste0(plot_id, "_renderer_svg"),
          class = "ggvis-renderer-button",
          `data-plot-id` = plot_id,
          `data-renderer` = "svg",
          "SVG"
        ),
        " | ",
        shiny::tags$a(
          id = paste0(plot_id, "_renderer_canvas"),
          class = "ggvis-renderer-button",
          `data-plot-id` = plot_id,
          `data-renderer` = "canvas",
          "Canvas"
        )
      ),
      shiny::tags$li(shiny::tags$a(
        id = paste0(plot_id, "_download"),
        class = "ggvis-download",
        `data-plot-id` = plot_id,
        "Download"
      ))
    )
  )
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
#' @param ... Additional tags.
#' @export
#' @examples
#' sidebarBottomPage(sidebarBottomPanel(), mainTopPanel())
sidebarBottomPage <- function(sidebarPanel, mainPanel, shiny_headers = TRUE) {
  content <- shiny::div(
    class = "container-fluid",
    shiny::div(class = "row-fluid",
      mainPanel,
      sidebarPanel
    )
  )

  if (shiny_headers) {
    shiny::bootstrapPage(content)
  } else {
    content
  }
}

#' @export
#' @rdname sidebarBottomPage
sidebarBottomPanel <- function(...) {
  shiny::div(class = "span4 sidebar-bottom",
    shiny::tags$form(class = "well well-small",
      ...
    )
  )
}

#' @rdname sidebarBottomPage
#' @export
mainTopPanel <- function(...) {
  shiny::div(class = "span8 main-top",
    ...
  )
}

#' Create a ggvis control output element in UI
#'
#' This is effectively the same as \code{\link[shiny]{uiOutput}}, except that
#' on the client side it may call some plot resizing functions after new
#' controls are drawn.
#'
#' \code{ggvisControlOutput} is intended to be used with
#' \code{\link{bind_shiny}} on the server side.
#'
#' @param outputId The output variable to read the value from.
#' @param plotId An optional plot ID or vector of plot IDs. The plots will
#'   have their .onControlOutput functions called after the controls are drawn.
#' @examples
#' ggvisControlOutput("plot1")
#' @export
ggvisControlOutput <- function(outputId, plotId = NULL) {
  if (is.null(plotId)) {
    shiny::div(id = outputId, class = "ggvis-control-output")

  } else {
    shiny::div(
      id = outputId,
      class = "ggvis-control-output",
      `data-plot-id` = paste(plotId, collapse = " ")
    )
  }
}
