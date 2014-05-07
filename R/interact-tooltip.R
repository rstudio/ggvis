#' Add tooltips to a plot.
#'
#' @param vis Visualisation to add tooltips to.
#' @param html A function that takes a single argument as input. This argument
#'   will be a list containing the data in the mark currently under the
#'   mouse. It should return a string containing HTML.
#' @param on Should tooltips appear on hover, or on click?
#' @export
#' @examples
#' \donttest{
#' all_values <- function(x) {
#'   if(is.null(x)) return(NULL)
#'   paste0(names(x), ": ", format(x), collapse = "<br />")
#' }
#'
#' base <- mtcars %>% ggvis(x = ~wt, y = ~mpg, size.hover := 400) %>%
#'   layer_points()
#' base %>% add_tooltip(all_values, "hover")
#' base %>% add_tooltip(all_values, "click")
#' }
add_tooltip <- function(vis, html, on = c("hover", "click")) {
  on <- match.arg(on)

  show_tooltip2 <- function(value, session) {
    if (is.null(value$data)) {
      hide_tooltip(session)
      return()
    }
    html <- html(value$data)
    show_tooltip(session, value$pagex + 5, value$pagey + 5, html)
  }
  hide_tooltip2 <- function(value, session) {
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
