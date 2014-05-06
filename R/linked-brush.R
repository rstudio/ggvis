#' Create a linked brush object.
#'
#' A linked brush has two sides: input and output
#'
#' @param keys vector of all possible keys, if known.
#' @param fill brush colour
#' @export
#' @importFrom methods setRefClass
#' @examples
#' lb <- linked_brush(keys = 1:nrow(mtcars), "red")
#'
#' # Change the colour of the points
#' mtcars %>%
#'  ggvis(~disp, ~mpg) %>%
#'  layer_points(fill := lb$fill(), size.brush := 400) %>%
#'  function(vis) lb$input(vis)
#'
#' # Display one layer with all points and another layer with selected points
#' library(dplyr)
#' mtcars %>%
#'  ggvis(~disp, ~mpg) %>%
#'  layer_points(size.brush := 400) %>%
#'  filter(eval(lb$selected())) %>%
#'  layer_points(fill := "red")
linked_brush <- function(keys, fill = "red") {
  stopifnot(is.character(fill), length(fill) == 1)

  rv <- shiny::reactiveValues(under_brush = character())

  input <- function(vis) {
    save_keys <- function(value, session) {
      if (is.null(value)) return()

      keys <- sapply(value$items, "[[", "key__")
      if (is.numeric(keys)) {
        keys <- as.character(keys + 1)
      }
      rv$under_brush <- keys
    }
    handle_brush(vis, save_keys, fill = fill)
  }

  output_selected <- function() {
    create_broker(reactive(keys %in% rv$under_brush))
  }
  output_fill <- function() {
    create_broker(reactive(c("black", fill)[keys %in% rv$under_brush + 1]))
  }

  list(input = input, selected = output_selected, fill = output_fill)
}
