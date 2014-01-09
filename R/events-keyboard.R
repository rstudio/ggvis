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
#' @seealso \code{\link{left_right}} to easily
#'   control values in a ggvis object with the arrow keys.
#' @export
#' @importFrom methods setRefClass
Keyboard <- setRefClass("Keyboard", contains = "EventBroker",
  fields = list("bindings" = "character"),
  methods = list(
    key_press = function() {
      "A reactive value changed when any key is pressed."
      
      listen_for("key_press")
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
#' ggvis(mtcars, props(x = ~mpg, y = ~wt,
#'                     size := left_right(1, 801, value = 51, step = 50))) +
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
  if (is.null(session)) return(reactive(x$control_args$value))
  
  k <- Keyboard(session, id = x$id)
  modify_value_with_keys(k, x$control_args$value, 
    left =  function(i) pmax(x$control_args$min, i - x$control_args$step),
    right = function(i) pmin(x$control_args$max, i + x$control_args$step)
  )
}

modify_value_with_keys <- function(k, val, ..., .key_funs = list()) {
  key_funs <- c(list(...), .key_funs)
  stopifnot(is(k, "Keyboard"))

  k$bindings <- names(key_funs)
  reactive({
    press <- k$key_press()
    if (is.null(press)) return(val)
    if (!(press$value %in% names(key_funs))) return(val)
    
    val <<- key_funs[[press$value]](val)
    val 
  })
}