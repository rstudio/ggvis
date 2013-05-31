#' @export
renderGigvis <- function(expr, ..., env=parent.frame(), quoted=FALSE) {
  assert_installed("shiny")

  func <- shiny::exprToFunction(expr, env, quoted)

  function() {
    data <- func()
    if (is.null(data))
      data <- list()

    return(list(spec = data))
  }
}


#' @export
gigvisOutput <- function(outputId) {
  assert_installed("shiny")

  shiny::addResourcePath(
    prefix = "gigvis",
    directoryPath = system.file("www", package="gigvis"))

  shiny::tagList(
    shiny::singleton(shiny::tags$head(
      shiny::tags$script(src = "gigvis/lib/d3.min.js"),
      shiny::tags$script(src = "gigvis/lib/vega.min.js"),
      shiny::tags$script(src = "gigvis/js/shiny-gigvis.js")
    )),
    shiny::tags$div(id = outputId, class = "shiny-gigvis-output")
  )
}
