#' Create a placeholder for a select input.
#'
#' @importFrom shiny selectInput
#' @inheritParams shiny::selectInput
#' @param id a unique identifying name for this control - only one control
#'   for a given name will be displayed on a page
#' @param map a function with a singe argument that takes the value returned
#'   from the input control and converts it to an argument useful for ggvis.
#'   Defaults to \code{identity}, leaving the output unchanged.
#' @family interactive input
#' @export
#' @examples
#' input_select(c("a", "b", "c"))
#' input_select(c("a", "b", "c"), multiple = TRUE)
#' input_select(c("a", "b", "c"), selected = "c")
input_select <- function(choices, selected = NULL, multiple = FALSE,
                         label = "", id = rand_id("select_"),
                         map = identity) {
  assert_that(is.string(label), is.string(id))

  controls <- list(id, label, choices = choices, selected = selected,
      multiple = multiple)

  if (is.null(selected)) {
    if (multiple) value <- ""
    else value <- choices[1]
  } else {
    value <- choices[selected]
  }

  input("input_select", from_input(id, value, map), controls, id = id)
}

#' @S3method controls input_select
controls.input_select <- function(x, session = NULL) {
  setNames(list(do.call(selectInput, x$controls)), x$id)
}


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

  input("input_slider", from_input(id, value, map), controls, id = id)
}

#' @S3method controls input_slider
controls.input_slider <- function(x, session = NULL) {
  setNames(list(do.call(sliderInput, x$controls)), x$id)
}

#' Create a placeholder for a checkbox input.
#'
#' @importFrom shiny checkboxInput
#' @inheritParams shiny::checkboxInput
#' @inheritParams input_checkbox
#' @family interactive input
#' @export
#' @examples
#'
#' input_checkbox(label = "Confidence interval")
#' input_checkbox(label = "Confidence interval", value = TRUE)
#'
#' # Used in a branch_smooth
#' branch_smooth(se = input_checkbox(label = "Confidence interval"))
#' branch_smooth(se = input_checkbox(label = "Confidence interval", value = FALSE))
#'
#' # Used with a map function, to convert the boolean to another type of value
#' branch_smooth(
#'   method = input_checkbox(label = "LOESS (curve) model fit",
#'                           map = function(val) ifelse(val, "loess", "lm"))
#' )
input_checkbox <- function(value = FALSE, label = "", id = rand_id("checkbox_"),
                           map = identity) {

  assert_that(is.string(label), is.string(id))

  controls <- list(id, label, value = value)

  input("input_checkbox", from_input(id, value, map), controls, id = id)
}

#' @S3method controls input_checkbox
controls.input_checkbox <- function(x, session = NULL) {
  setNames(list(do.call(checkboxInput, x$controls)), x$id)
}


#' Create a placeholder for a text input.
#'
#' @importFrom shiny textInput
#' @inheritParams shiny::textInput
#' @inheritParams input_text
#' @family interactive input
#' @export
#' @examples
#' input_text(label = "Point color", value = "red")
#'
#' ggvis(mtcars, props(x ~ wt, y ~ mpg),
#'   mark_symbol(
#'     props(fill = input_text(label = "Point color", value = "red"))
#'   )
#' )
input_text <- function(value, label = "", id = rand_id("text_"),
                       map = identity) {

  assert_that(is.string(label), is.string(id), is.string(value))

  controls <- list(id, label, value = value)

  input("input_text", from_input(id, value, map), controls, id = id)
}

#' @S3method controls input_text
controls.input_text <- function(x, session = NULL) {
  setNames(list(do.call(textInput, x$controls)), x$id)
}


#' Create a placeholder for a numeric input.
#'
#' @importFrom shiny numericInput
#' @inheritParams shiny::numericInput
#' @inheritParams input_numeric
#' @family interactive input
#' @export
#' @examples
#' input_numeric(label = "Point size", value = 25)
#'
#' ggvis(mtcars, props(x ~ wt, y ~ mpg),
#'   mark_symbol(
#'     props(size = prop(input_numeric(label = "Point size", value = 25))
#'   )
#' )
input_numeric <- function(value, label = "", id = rand_id("text_"),
                          map = identity) {

  assert_that(is.string(label), is.string(id), is.numeric(value))

  controls <- list(id, label, value = value)

  input("input_numeric", from_input(id, value, map), controls, id = id)
}

#' @S3method controls input_numeric
controls.input_numeric <- function(x, session = NULL) {
  setNames(list(do.call(numericInput, x$controls)), x$id)
}


#' Create a placeholder for a radio button input.
#'
#' @importFrom shiny radioButtons
#' @inheritParams shiny::radioButtons
#' @inheritParams input_radiobuttons
#' @family interactive input
#' @export
#' @examples
#'
#' input_radiobuttons(choices = c("Linear" = "lm", "LOESS" = "loess"),
#'                    label = "Model type")
#' input_radiobuttons(choices = c("Linear" = "lm", "LOESS" = "loess"),
#'                    selected = "LOESS",
#'                    label = "Model type")
#'
#' # Used in a branch_smooth
#' branch_smooth(model = input_radiobuttons(
#'   choices = c("Linear" = "lm", "LOESS" = "loess"),
#'               selected = "LOESS",
#'               label = "Model type"))
input_radiobuttons <- function(choices, selected = NULL, label = "",
                               id = rand_id("radio_"), map = identity) {

  assert_that(is.string(label), is.string(id))

  controls <- list(id, label, choices = choices, selected = selected)

  if (is.null(selected)) {
    value <- choices[1]
  } else {
    value <- choices[selected]
  }

  input("input_radiobuttons", from_input(id, value, map), controls, id = id)
}

#' @S3method controls input_radiobuttons
controls.input_radiobuttons <- function(x, session = NULL) {
  setNames(list(do.call(radioButtons, x$controls)), x$id)
}


#' Create a placeholder for a checkbox group input.
#'
#' @importFrom shiny checkboxGroupInput
#' @inheritParams shiny::checkboxGroupInput
#' @inheritParams input_checkboxgroup
#' @family interactive input
#' @export
input_checkboxgroup <- function(choices, selected = NULL, label = "",
                                id = rand_id("radio_"), map = identity) {

  assert_that(is.string(label), is.string(id))

  controls <- list(id, label, choices = choices, selected = selected)

  if (is.null(selected)) {
    value <- character(0)
  } else {
    value <- choices[selected]
  }

  input("input_checkboxgroup", from_input(id, value, map), controls, id = id)
}

#' @S3method controls input_checkboxgroup
controls.input_checkboxgroup <- function(x, session = NULL) {
  setNames(list(do.call(checkboxGroupInput, x$controls)), x$id)
}
