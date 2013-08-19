#' Create a placeholder for a slider input.
#'
#' @importFrom shiny sliderInput
#' @inheritParams shiny::sliderInput
#' @inheritParams input_select
#' @family interactive input
#' @export
#' @examples
#' input_slider(0, 100)
#' input_slider(0, 100, label = "binwidth")
#' input_slider(0, 100, value = 50)
input_slider <- function(min, max, value = min, step = NULL, round = FALSE,
                         format = "#,##0.#####", locale = "us", ticks = TRUE,
                         animate = FALSE, label = "", id = rand_id("slider_"),
                         map = identity) {

  assert_that(is.string(label), is.string(id))

  controls <- list(id, label, min = min, max = max, value = value, step = step,
      round = round, format = format, locale = locale, ticks = ticks)

  delayed_reactive("input_slider", from_input(id, value, map), controls, id = id)
}

#' @S3method controls input_slider
controls.input_slider <- function(x, session = NULL) {
  setNames(list(do.call(sliderInput, x$controls)), x$id)
}
