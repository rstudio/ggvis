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

#' @importFrom shiny addResourcePath singleton tagList
gigvisOutput <- function(id) {
  addResourcePath("gigvis", system.file("www", package = "gigvis"))

  tagList(
    singleton(tags$head(
      tags$script(src = "gigvis/lib/jquery-1.9.1.js"),
      tags$script(src = "gigvis/lib/d3.js"),
      tags$script(src = "gigvis/lib/vega.js"),
      tags$script(src = "gigvis/lib/QuadTree.js"),
      tags$script(src = "gigvis/js/shiny-gigvis.js")
    )),
    tags$div(id = id)
  )
}
