#' Waggle back and forth between two numbers
#'
#' @param min A minimum value.
#' @param max A maximum value.
#' @param value Starting value. Defaults to half-way between \code{min} and
#'   \code{max}.
#' @param step How much value changes at each frame. Defaults to 50 steps
#'   between min and max so it takes 5 seconds to waggle once.
#' @param fps number of frames per second.
#' @export
#' @examples
#' span <- waggle(0.2, 1)
#' mtcars %>% ggvis(~mpg, ~wt) %>%
#'  layer_points() %>%
#'  layer_smooths(span = span)
waggle <- function(min, max, value = (min + max) / 2, step = (max - min) / 50,
                   fps = 10) {

  vals <- shiny::reactiveValues()
  vals$x <- value

  connect <- function(session, plot_id) {
    direction <- 1
    shiny::observe({
      shiny::invalidateLater(1000 / fps, NULL)

      next_value <- shiny::isolate(vals$x) + direction * step
      if (next_value < min || next_value > max) {
        direction <<- -1 * direction
        next_value <- pmax(pmin(next_value, max), min)
      }

      vals$x <<- next_value
    })
  }

  create_broker(reactive(vals$x), connect = connect)
}
