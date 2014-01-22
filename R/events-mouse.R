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
      previously selected data mark.
      Returns a list containing:
        * plot_id: The ID of the ggvis plot."

      listen_for("mouse_out")
    },
    mouse_over = function() {
      "A reactive value that changes every time the mouse moves on a mark.
      Returns a list containing:
        * plot_id: The ID of the ggvis plot.
        * data: a list of the data underlying the mark that is hovered over.
          There is an item for each variable in the data, as well as a field
          named `key__`.
        * pagex: The x position of the mouse relative to the page.
        * pagey: The y position of the mouse relative to the page."

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
      Returns a list containing:
        * plot_id: The ID of the ggvis plot.
        * data: a list of the data underlying the mark that is hovered over.
          There is an item for each variable in the data, as well as a field
          named `key__`.
        * pagex: The x position of the mouse relative to the page.
        * pagey: The y position of the mouse relative to the page."

      listen_for("mouse_click")
    }
  )
)

#' Display tooltips
#'
#' @param f A function that takes a single argument as input. This argument
#'   will be a list containing the data in the mark currently under the
#'   mouse. It should return a string containing HTML.
#' @export
#' @examples
#' \dontrun{
#' all_values <- function(x) {
#'   if(is.null(x)) return(NULL)
#'   paste0(names(x), ": ", format(x), collapse = "<br />")
#' }
#'
#' # Display tooltip when hovering over objects
#' ggvis(mtcars, props(x = ~wt, y = ~mpg)) +
#'   mark_symbol() +
#'   tooltip(all_values)
#'
#' # Display tooltip when objects are clicked
#' ggvis(mtcars, props(x = ~wt, y = ~mpg)) +
#'   mark_symbol() +
#'   click_tooltip(all_values)
#'
#' # Grab the mean and standard deviations of brushed values
#' brushed_summary <- function(x) {
#'  if(is.null(x) || length(x) == 0) return(NULL)
#'  # Get the names of variables other than "key__"
#'  names <- setdiff(names(x[[1]]), "key__")
#'
#'  # Print out the mean and sd for each variable
#'  lines <- lapply(names, function(name) {
#'    vals <- vapply(x, `[[`, name, FUN.VALUE = numeric(1))
#'    paste0(name, ": ", round(mean(vals), 2), " (", round(sd(vals), 2), ")")
#'  })
#'
#'  paste0(lines, collapse = "<br />")
#' }
#'
#' # Display tooltip when objects are brushed
#' ggvis(mtcars, props(x = ~wt, y = ~mpg)) +
#'   mark_symbol(props(size.brush := 400)) +
#'   brush_tooltip(brushed_summary)
#' }
tooltip <- function(f) {
  stopifnot(is.function(f))

  handler("tooltip", "hover", list(f = f))
}

#' @export
as.reactive.tooltip <- function(x, session = NULL, ...) {
  if (is.null(session)) return()

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

#' @export
#' @rdname tooltip
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
