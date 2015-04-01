ggvisLayout <- function(plot_id, has_controls = TRUE, spec = NULL,
                        shiny = TRUE) {
  plot_div <- ggvisOutputElements(plot_id, spec = spec, shiny = shiny)

  if (!has_controls) {
    plot_div
  } else {
    shiny::bootstrapPage(
      sidebarBottomPage(
        sidebarBottomPanel(ggvisControlOutput("ggvis_controls", plot_id)),
        mainTopPanel(plot_div)
      )
    )
  }
}

# This is the user-facing wrapper for ggvisOutputElements. When the user calls
# it, it will always be used with Shiny.
#' @rdname shiny-ggvis
#' @param plot_id unique identifier to use for the div containing the ggvis plot.
#' @export
ggvisOutput <- function(plot_id = rand_id("plot_id")) {
  ggvisOutputElements(plot_id, spec = NULL, shiny = TRUE)
}

#' Create HTML elements for ggvis output
#'
#' This is an internal-facing function similar to ggvisOutput, but with more
#' options.
#'
#' @param plot_id Unique identifier to use for the div containing the ggvis plot.
#' @param spec Plot specification, used internally.
#' @param shiny Should this include headers for Shiny? For dynamic and
#'   interactive plots, this should be TRUE; otherwise FALSE.
#' @keywords internal
ggvisOutputElements <- function(plot_id = rand_id("plot_id"), spec = NULL,
                                shiny = TRUE) {

  validate_plot_id(plot_id)

  htmltools::attachDependencies(
    htmltools::tagList(
      ggvisPlot(plot_id),
      ggvisSpec(plot_id, spec)
    ),
    c(
      ggvis_dependencies(),
      if (shiny) list(shiny_dependency())
    )
  )
}

ggvisPlot <- function(plot_id) {
  htmltools::div(id = paste0(plot_id, "-container"), class = "ggvis-output-container",
    # Div containing the plot
    htmltools::div(id = plot_id, class = "ggvis-output"),
    htmltools::div(class = "plot-gear-icon",
      ggvisControlGroup(plot_id)
    )
  )
}

ggvisSpec <- function(plot_id, spec = NULL) {
  if (is.null(spec)) return()
  json <- jsonlite::toJSON(spec, pretty = TRUE, auto_unbox = TRUE, force = TRUE,
                           null = "null")

  htmltools::tags$script(type = "text/javascript", paste0('\n',
    'var ', plot_id, '_spec = ', json, ';\n',
    'ggvis.getPlot("', plot_id, '").parseSpec(', plot_id, '_spec);\n'
  ))
}

# Controls drop down
ggvisControlGroup <- function(plot_id) {
  # The <a> tags need the onclick so that they work properly in Shiny Doc iframes
  htmltools::tags$nav(class = "ggvis-control",
    htmltools::tags$a(class = "ggvis-dropdown-toggle", title = "Controls",
                      onclick = "return false;"),
    htmltools::tags$ul(class = "ggvis-dropdown",
      htmltools::tags$li(
        "Renderer: ",
        htmltools::tags$a(
          id = paste0(plot_id, "_renderer_svg"),
          class = "ggvis-renderer-button",
          onclick = "return false;",
          `data-plot-id` = plot_id,
          `data-renderer` = "svg",
          "SVG"
        ),
        " | ",
        htmltools::tags$a(
          id = paste0(plot_id, "_renderer_canvas"),
          class = "ggvis-renderer-button",
          onclick = "return false;",
          `data-plot-id` = plot_id,
          `data-renderer` = "canvas",
          "Canvas"
        )
      ),
      htmltools::tags$li(htmltools::tags$a(
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
  content <- htmltools::div(
    class = "container-fluid",
    htmltools::div(class = "row-fluid",
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
  htmltools::div(class = "col-sm-4 sidebar-bottom",
    htmltools::tags$form(class = "well well-small",
      ...
    )
  )
}

#' @rdname sidebarBottomPage
#' @export
mainTopPanel <- function(...) {
  htmltools::div(class = "col-sm-8 main-top",
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
    htmltools::div(id = outputId, class = "ggvis-control-output")

  } else {
    htmltools::div(
      id = outputId,
      class = "ggvis-control-output",
      `data-plot-id` = paste(plotId, collapse = " ")
    )
  }
}
