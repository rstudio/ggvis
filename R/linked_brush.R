#' Create a linked brush object.
#'
#' A linked brush has two sides: input and output
#'
#' @note \code{linked_brush} is very new and is likely to change substantially
#'   in the future
#' @param keys vector of all possible keys, if known.
#' @param fill brush colour
#' @return A list with components:
#'   \item{input}{A function that takes a visualisation as an argument and
#'      adds an input brush to that plot}
#'   \item{selected}{A reactive providing a logical vector that describes
#'     which points are under the brush}
#'   \item{fill}{A reactive that gives the fill colour of points under the
#'     brush}
#' @export
#' @examples
#' lb <- linked_brush(keys = 1:nrow(mtcars), "red")
#'
#' # Change the colour of the points
#' mtcars %>%
#'  ggvis(~disp, ~mpg) %>%
#'  layer_points(fill := lb$fill, size.brush := 400) %>%
#'  lb$input()
#'
#' # Display one layer with all points and another layer with selected points
#' library(shiny)
#' mtcars %>%
#'  ggvis(~disp, ~mpg) %>%
#'  layer_points(size.brush := 400) %>%
#'  lb$input() %>%
#'  layer_points(fill := "red", data = reactive(mtcars[lb$selected(), ]))
linked_brush <- function(keys, fill = "red") {
  stopifnot(is.character(fill), length(fill) == 1)

  rv <- shiny::reactiveValues(under_brush = character())

  input <- function(vis) {
    handle_brush(vis, fill = fill, on_move = function(items, ...) {
      rv$under_brush <- items$key__
    })
  }

  selected_r <- reactive(keys %in% rv$under_brush)
  fill_r <- reactive(c("black", fill)[selected_r() + 1])

  list(
    input = input,
    selected = create_broker(selected_r),
    fill = create_broker(fill_r)
  )
}
