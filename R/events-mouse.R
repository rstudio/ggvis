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
#' @importFrom methods setRefClass
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
#' @examples
#' all_values <- function(x) {
#'   paste0(names(x), ": ", format(x), collapse = "<br />")
#' } 
#' 
#' ggvis(mtcars, props(x = ~wt, y = ~mpg), mark_symbol(), tooltip(all_values))
#' ggvis(mtcars, props(x = ~wt, y = ~mpg)) + 
#'   mark_symbol() +
#'   tooltip(all_values)
tooltip <- function(f) {
  stopifnot(is.function(f))
  
  handler("tooltip", "hover", list(f = f))
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
    html <- x$control_args$f(hover$data)
    
    tell_to("show_tooltip",
      visible = TRUE,
      pagex = hover$pagex - 100,
      pagey = hover$pagey + 6,
      html = html
    )
  })
  reactive(NULL)
}
