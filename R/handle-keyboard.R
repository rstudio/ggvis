#' Interactive inputs bound to arrow keys.
#'
#' @param min A minimum value.
#' @param max A maximum value.
#' @param value The initial value before any keys are pressed. Defaults to
#'   half-way between \code{min} and \code{max}.
#' @param step How much each key press changes \code{value}. Defaults to
#'   40 steps along range
#' @export
#' @examples
#' size <- left_right(1, 801, value = 51, step = 50)
#' opacity <- up_down(0, 1, value = 0.9, step = 0.05)
#'
#' mtcars %>% ggvis(~mpg, ~wt, size := size, opacity := opacity) %>%
#'   layer_points()
left_right <- function(min, max, value = (min + max) / 2,
                       step = (max - min) / 40) {

  # Given the key_press object and current value, return the next value
  map <- function(key_press, current_value) {
    key <- key_press$value

    if (key == "left") {
      pmax(min, current_value - step)
    } else if (key == "right") {
      pmin(max, current_value + step)
    } else {
      current_value
    }
  }

  create_keyboard_event(map, value)
}

#' @export
#' @rdname left_right
up_down <- function(min, max, value = (min + max) / 2,
                    step = (max - min) / 40) {

  map <- function(key_press, current_value) {
    key <- key_press$value

    if (key == "down") {
      pmax(min, current_value - step)
    } else if (key == "up") {
      pmin(max, current_value + step)
    } else {
      current_value
    }
  }

  create_keyboard_event(map, value)
}

#' Event broker for keyboard events.
#'
#' The keyboard event broker is useful if you want your shiny app to respond
#' to keyboard events: \code{key_down}, \code{key_press} and \code{key_up}.
#' This returns a broker object, similar to \code{\link{create_input}}.
#'
#' The \code{map} function takes a \code{key_press} list object which is
#' converted from the JSON object sent from the client, and the current value
#' of this reactive. Keyboard events differ from inputs in that when a key
#' is pressed, the new value can depend on the previous value. For example,
#' pressing the right arrow might increment the value each time. The value of
#' an input contrast, in contrast, takes the value directly from the client,
#' without using the previous value. The map function for keyboard events
#' therefore take the \code{current_value} as an argument.
#'
#' The \code{map} function takes two arguments. The first is a list containing
#' an item named \code{value}, which is a string representation of the key that
#' was pressed. The second is the current value of the reactive.
#'
#' The keyboard event handling in ggvis is implemented with
#'   \href{mousetrap}{http://craig.is/killing/mice}, so you can specify keys
#'   like \code{c("C", "Shift + X", "F2", "up"))}.
#'
#' Also, unlike inputs, keyboard events add no HTML controls, but they do add
#' an object to insert into the Vega spec.
#'
#' @param map A function which takes the key_press list object (a list
#' @param default Default value before first key pressed
#' @noRd
create_keyboard_event <- function(map, default = NULL) {
  if (!is.function(map)) stop("map must be a function")

  vals <- shiny::reactiveValues()
  vals$x <- default

  # A reactive to wrap the reactive value
  res <- reactive({
    vals$x
  })

  # This function is run at render time.
  connect <- function(session, plot_id) {
    key_press_id  <- paste0(plot_id, "_key_press")

    shiny::observe({
      key_press <- session$input[[key_press_id]]

      if (!is.null(key_press)) {
        # Get the current value of the reactive, without taking a dependency
        current_value <- shiny::isolate(vals$x)

        vals$x <- map(key_press, current_value)
      }

    })
  }
  connector_label(connect) <- "key_press"

  spec <- list(type = "keyboard")
  create_broker(res, connect = connect, spec = spec)
}
