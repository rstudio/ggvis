#' Create a placeholder for a select input.
#'
#' @importFrom shiny selectInput
#' @inheritParams shiny::selectInput
#' @inheritParams input
#' @family interactive input
#' @export
#' @examples
#' input_select(c("a", "b", "c"))
#' input_select(c("a", "b", "c"), multiple = TRUE)
#' input_select(c("a", "b", "c"), selected = "c")
input_select <- function(choices, selected = NULL, multiple = FALSE,
                         label = "", id = rand_id("select_"),
                         map = identity, scale = FALSE) {
  assert_that(is.string(label), is.string(id))

  args <- list(id, label, choices = choices, selected = selected,
      multiple = multiple)

  if (is.null(selected)) {
    if (multiple) value <- ""
    else value <- choices[1]
  } else {
    value <- choices[selected]
  }
  
  input("select", args, value, map, id, scale)
}

#' Create a placeholder for a slider input.
#'
#' @importFrom shiny sliderInput
#' @inheritParams shiny::sliderInput
#' @inheritParams input
#' @family interactive input
#' @export
#' @examples
#' input_slider(0, 100)
#' input_slider(0, 100, label = "binwidth")
#' input_slider(0, 100, value = 50)
input_slider <- function(min, max, value = min, step = NULL, round = FALSE,
                         format = "#,##0.#####", locale = "us", ticks = TRUE,
                         animate = FALSE, label = "", id = rand_id("slider_"),
                         map = identity, scale = FALSE) {

  assert_that(is.string(label), is.string(id))

  args <- list(id, label, min = min, max = max, value = value, step = step,
      round = round, format = format, locale = locale, ticks = ticks)

  input("slider", args, value, map, id, scale)
}

#' Create a placeholder for a checkbox input.
#'
#' @importFrom shiny checkboxInput
#' @inheritParams shiny::checkboxInput
#' @inheritParams input
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
input_checkbox <- function(value = FALSE, label = "", 
                           id = rand_id("checkbox_"), map = identity, 
                           scale = FALSE) {

  assert_that(is.string(label), is.string(id))

  args <- list(id, label, value = value)
  input("checkbox", args, value, map, id)
}

#' Create a placeholder for a text input.
#'
#' @importFrom shiny textInput
#' @inheritParams shiny::textInput
#' @inheritParams input
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
                       map = identity, scale = FALSE) {

  assert_that(is.string(label), is.string(id), is.string(value))

  args <- list(id, label, value = value)
  input("text", args, value, map, id, scale)
}


#' Create a placeholder for a numeric input.
#'
#' @importFrom shiny numericInput
#' @inheritParams shiny::numericInput
#' @inheritParams input
#' @family interactive input
#' @export
#' @examples
#' input_numeric(label = "Point size", value = 25)
#'
#' ggvis(mtcars, props(x ~ wt, y ~ mpg),
#'   mark_symbol(
#'     props(size = input_numeric(label = "Point size", value = 25)
#'   )
#' )
input_numeric <- function(value, label = "", id = rand_id("text_"),
                          map = identity, scale = FALSE) {

  assert_that(is.string(label), is.string(id), is.numeric(value))

  args <- list(id, label, value = value)

  input("numeric", args, value, map, id, scale)
}

#' Create a placeholder for a radio button input.
#'
#' @importFrom shiny radioButtons
#' @inheritParams shiny::radioButtons
#' @inheritParams input
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
                               id = rand_id("radio_"), map = identity, 
                               scale = FALSE) {

  assert_that(is.string(label), is.string(id))

  args <- list(id, label, choices = choices, selected = selected)

  if (is.null(selected)) {
    value <- choices[1]
  } else {
    value <- choices[selected]
  }

  input("radio_buttons", args, value, map, id, scale, 
    control_f = "radioButtons")
}

#' Create a placeholder for a checkbox group input.
#'
#' @importFrom shiny checkboxGroupInput
#' @inheritParams shiny::checkboxGroupInput
#' @inheritParams input
#' @family interactive input
#' @export
input_checkboxgroup <- function(choices, selected = NULL, label = "",
                                id = rand_id("radio_"), map = identity,
                                scale = FALSE) {

  assert_that(is.string(label), is.string(id))

  args <- list(id, label, choices = choices, selected = selected)

  if (is.null(selected)) {
    value <- character(0)
  } else {
    value <- choices[selected]
  }

  input("checkbox_group", args, value, map, id, scale)
}
