#' @include events.R
NULL

#' Event broker for hover events.
#' 
#' The hover event broker is useful if you want your shiny app to respond
#' to hover events (\code{mouse_out} and \code{mouse_over}) in a custom
#' way. 
#' 
#' @seealso \code{\link{tooltip}} for a custom wrapper that uses hover
#'   events to display tooltips.
#' @export
Hover <- setRefClass("Hover", contains = "EventBroker",
  methods = list(
    mouse_out = function() {
      "A reactive value that changes every time the mouse moves off the 
      previously selected data mark. The return should be ignored." 
    
      listen_for("mouse_out")
    },
    mouse_over = function() {
      "A reactive value that changes every time the mouse moves on a mark.
      The value is [WINSTON INSERT HERE]."
      
      listen_for("mouse_over")
    }
  )
)

#' Display tooltips.
#' 
#' @param f A function that takes a single argument as input. This argument
#'   will be a list containing the data in the mark currently under the 
#'   mouse. It should return a string containing HTML.
#' @export
tooltip <- function(f) {
  stopifnot(is.function(f))
  
  structure(list(f = f), class = c("tooltip", "input"))
}

#' @export
as.reactive.tooltip <- function(x, session = NULL, ...) {
  h <- Hover(session)
  observe({
    h$mouse_out()
    h$tell_to("hide_tooltip")
  })
  observe({
    hover <- h$mouse_over()
    html <- fun(hover$data)
    
    tell_to("show_tooltip",
      visible = TRUE,
      pagex = hover$pagex - 100,
      pagey = hover$pagey + 6,
      html = html
    )
  })
  reactive(NULL)
}
