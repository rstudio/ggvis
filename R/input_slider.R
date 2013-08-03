#' Create a placeholder for a slider input.
#' 
#' @importFrom shiny sliderInput
#' @inheritParams shiny::sliderInput
#' @family interactive input
#' @export
#' @examples
#' input_slider(0, 100)
#' input_slider(0, 100, label = "binwidth")
#' input_slider(0, 100, value = 50)
input_slider <- function(min, max, value = min, step = NULL, round = FALSE,
                         format = "#,##0.#####", locale = "us", ticks = TRUE,
                         animate = FALSE, label = "", id = rand_id("slider_")) {

  assert_that(is.string(label), is.string(id))

  control <- sliderInput(id, label, min = min, max = max, value = value,
    step = step, round = round, format = format, locale = locale, ticks = ticks)
  
  delayed_reactive(from_input(id, value), control, id = id)
}
