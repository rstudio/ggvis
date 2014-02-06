#' Waggle back and forth between two numbers
#'
#' @inheritParams shiny::sliderInput
#' @param value Starting value. Defaults to half-way between \code{min} and
#'   \code{max}.
#' @param step How much value changes at each frame. Defaults to 60 steps
#'   between min and max so it takes 2 seconds to waggle once.
#' @param fps number of frames per second.
#' @export
#' @examples
#' span <- waggle(0.2, 1)
#' ggvis(mtcars, props(~mpg, ~wt)) +
#'  layer_point() +
#'  layer_smooth(span = span, method = "loess", formula = y ~ x)
waggle <- function(min, max, value = (min + max) / 2, step = (max - min) / 60,
                   fps = 30) {
  # FIXME: I need my own class!
  handler("waggle", "waggle",
    list(min = min, max = max, value = value, step = step, fps = fps),
    value = value
  )
}

#' @export
as.reactive.waggle <- function(x, session = NULL, ...) {
  if (is.null(session)) return(reactive(x$control_args$value))

  value <- x$control_args$value
  step <- x$control_args$step
  min <- x$control_args$min
  max <- x$control_args$max
  fps <- x$control_args$fps

  direction <- 1
  reactive({
    invalidateLater(1000 / fps, NULL)

    next_value <- value + direction * step
    if (next_value < min || next_value > max) {
      direction <<- -1 * direction
      next_value <- pmax(pmin(next_value, max), min)
    }

    value <<- next_value
    value
  })
}

handlers.waggle <- function(x, ...) NULL
