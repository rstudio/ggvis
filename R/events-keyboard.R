#' @include events.R
NULL

#' Event broker for keyboard events.
#' 
#' The hover event broker is useful if you want your shiny app to respond
#' to keyboard events: \code{key_down}, \code{key_press} and \code{key_up}.
#' 
#' @param bindings A character vector describing the keyboard bindings
#'   to listen to. The keyboard event handling in ggvis is implemented with
#'   \href{mousetrap}{http://craig.is/killing/mice} so you can specify keys
#'   like \code{c("C", "Shift + X", "F2", "up"))}
#' @seealso \code{\link{left_right}} and \code{\link{up_down}} to easily
#'   control values in a ggvis object with the arrow keys.
#' @export
Keyboard <- setRefClass("Keyboard", contains = "EventBroker",
  fields = list("bindings" = "character"),
  methods = list(
    key_down = function() {
      "A reactive value changed when any key listed in \\code{bindings} is first
      depressed. Returns a list describing the key press"
      
      listen_for("key_down")
    },
    key_press = function() {
      "A reactive value changed when any key listed in \\code{bindings} is first
      'pressed'. It is similar to \\code{key_down} but in most browsers is only
      triggered by printing keys, not modifiers (like shift or control), or 
      special keys like escape or delete."
      
      listen_for("key_press")
    },
    key_up = function() {
      "A reactive value changed when any key listed in \\code{bindings} is
      released. Returns a list describing the key press"
      
      listen_for("key_up")
    },
    as_vega = function(...) {
      c(callSuper(), list(keys = bindings))
    }
  )
)

#' An interactive input bound to the left and right arrows.
#' 
#' @inheritParams shiny::sliderInput
#' @param value The initial value before any keys are pressed. Defaults to
#'   half-way between \code{min} and \code{max}.
#' @param step How much each key press changes \code{value}. Defaults to
#'   40 steps along range
#' @export
#' @examples
#' ggvis(mtcars, props(x = ~mpg, y = ~wt, size := left_right(1, 100))) +
#'   mark_symbol()
left_right <- function(min, max, value = (min + max) / 2, 
                       step = (max - min) / 40) {
  handler("left_right", "keyboard",
    list(min = min, max = max, value = value, step = step),
    value = value
  )
}

#' @export
as.reactive.left_right <- function(x, session = NULL, ...) {
  k <- Keyboard(session, c("left", "right"))
  
  i <- x$control_args$value
  step <- x$control_args$step
  reactive({
    press <- k$key_press()
    if (is.null(press)) return(i)
    
    if (press$key == "left"  && i > x$control_args$min) i <<- i - step
    if (press$key == "right" && i < x$control_args$max) i <<- i + step
    
    i
  })
}
