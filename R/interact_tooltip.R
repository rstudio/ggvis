#' Add tooltips to a plot.
#'
#' @param vis Visualisation to add tooltips to.
#' @param html A function that takes a single argument as input. This argument
#'   will be a list containing the data in the mark currently under the
#'   mouse. It should return a string containing HTML or \code{NULL} to
#'   hide tooltip for the current element.
#' @param on Should tooltips appear on hover, or on click?
#' @export
#' @examples
#' ## Run these examples only in interactive R sessions
#' if (interactive()) {
#'
#' all_values <- function(x) {
#'   if(is.null(x)) return(NULL)
#'   paste0(names(x), ": ", format(x), collapse = "<br />")
#' }
#'
#' base <- mtcars %>% ggvis(x = ~wt, y = ~mpg) %>%
#'   layer_points()
#' base %>% add_tooltip(all_values, "hover")
#' base %>% add_tooltip(all_values, "click")
#'
#' # The data sent from client to the server contains only the data columns that
#' # are used in the plot. If you want to get other columns of data, you should
#' # to use a key to line up the item from the plot with a row in the data.
#' mtc <- mtcars
#' mtc$id <- 1:nrow(mtc)  # Add an id column to use ask the key
#'
#' all_values <- function(x) {
#'   if(is.null(x)) return(NULL)
#'   row <- mtc[mtc$id == x$id, ]
#'   paste0(names(row), ": ", format(row), collapse = "<br />")
#' }
#'
#' mtc %>% ggvis(x = ~wt, y = ~mpg, key := ~id) %>%
#'   layer_points() %>%
#'   add_tooltip(all_values, "hover")
#'
#' }
add_tooltip <- function(vis, html, on = c("hover", "click")) {
  on <- match.arg(on)

  show_tooltip2 <- function(data, location, session, ...) {
    if (is.null(data)) {
      hide_tooltip(session)
      return()
    }

    html <- html(data)
    if (is.null(html)) {
      hide_tooltip(session)
    } else {
      show_tooltip(session, location$x + 5, location$y + 5, html)
    }
  }
  hide_tooltip2 <- function(session) {
    hide_tooltip(session)
  }

  switch(on,
    click = handle_click(vis, show_tooltip2),
    hover = handle_hover(vis, show_tooltip2, hide_tooltip2)
  )
}

#' Send a message to the client to show or hide a tooltip
#'
#' @param session A Shiny session object.
#' @param l Pixel location of left edge of tooltip (relative to page)
#' @param t Pixel location of top edge of tooltip (relative to page)
#' @param html HTML to display in the tooltip box.
#'
#' @export
show_tooltip <- function(session, l = 0, t = 0, html = "") {
  ggvis_message(session, "show_tooltip",
    list(pagex = l, pagey = t, html = html))
}

#' @rdname show_tooltip
#' @export
hide_tooltip <- function(session) {
  ggvis_message(session, "hide_tooltip")
}
