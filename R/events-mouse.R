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

#' Event broker for click events.
#'
#' The click event broker is useful if you want your shiny app to respond
#' to click events (\code{mouse_click} in a custom way.
#'
#' @seealso \code{\link{click_tooltip}} for a custom wrapper that can use click
#'   events to display tooltips.
#' @export
#' @importFrom methods setRefClass
Click <- setRefClass("Click", contains = "EventBroker",
  methods = list(
    mouse_click = function() {
      "A reactive value that changes every time the mouse is clicked.
      The return should be ignored."

      listen_for("mouse_click")
    }
  )
)

#' @export
click_tooltip <- function(f) {
  stopifnot(is.function(f))

  handler("click_tooltip", "click", list(f = f))
}

#' @export
as.reactive.click_tooltip <- function(x, session = NULL, ...) {
  h <- Click(session, id = x$id)

  obs <- observe({
    click <- h$mouse_click()
    if (is.null(click$data)) {
      hide_tooltip(session)
      return()
    }

    html <- x$control_args$f(click$data)

    show_tooltip(session,
      pagex = click$pagex - 90,
      pagey = click$pagey - 6,
      html = html
    )
  })

  session$onSessionEnded(function() {
    obs$suspend()
  })

  reactive({ NULL })
}



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
  h <- Hover(session, id = x$id)

  obs_out <- observe({
    h$mouse_out()
    hide_tooltip(session)
  })
  obs_over <- observe({
    hover <- h$mouse_over()
    if (is.null(hover$data)) {
      hide_tooltip(session)
      return()
    }

    html <- x$control_args$f(hover$data)

    show_tooltip(session,
      pagex = hover$pagex - 90,
      pagey = hover$pagey - 6,
      html = html
    )
  })

  session$onSessionEnded(function() {
    obs_out$suspend()
    obs_over$suspend()
  })

  reactive({ NULL })
}

#' Send a message to the client to show or hide a tooltip
#'
#' @param session A Shiny session object.
#' @param pagex x position of the tooltip box on the page.
#' @param pagey y position of the tooltip box on the page.
#' @param html HTML to display in the tooltip box.
#'
#' @export
show_tooltip <- function(session, pagex = 0, pagey = 0, html = "") {
  ggvis_message(session, "show_tooltip",
    list(pagex = pagex, pagey = pagey, html = html))
}

#' @rdname show_tooltip
#' @export
hide_tooltip <- function(session) {
  ggvis_message(session, "hide_tooltip")
}
