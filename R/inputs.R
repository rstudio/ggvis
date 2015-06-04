#' Create an interactive slider.
#'
#' @inheritParams shiny::sliderInput
#' @param id A unique identifier for this input. Usually generated
#'   automatically.
#' @param map A function with single argument \code{x}, the value of the
#'   control on the client. Returns a modified value.
#' @family interactive input
#' @export
#' @examples
#' input_slider(0, 100)
#' input_slider(0, 100, label = "binwidth")
#' input_slider(0, 100, value = 50)
#'
#' # Supply two values to value to make a double-ended sliders
#' input_slider(0, 100, c(25, 75))
#'
#' # You can use map to transform the outputs
#' input_slider(-5, 5, label = "Log scale", map = function(x) 10 ^ x)
input_slider <- function(min, max, value = (min + max) / 2, step = NULL,
                         round = FALSE, format = NULL, locale = "us",
                         ticks = TRUE, animate = FALSE,
                         sep = ",", pre = NULL, post = NULL, label = "",
                         id = rand_id("slider_"), map = identity) {

  assert_that(is.string(label), is.string(id))

  if (!is.null(step)) {
    value <- round_any(value - min, step) + min
  }

  # Older versions of shiny use `format` and `locale`; newer versions use
  # `sep`, `pre`, and `post`.
  if (packageVersion("shiny") >= "0.11") {
    control <- shiny::sliderInput(id, label, min = min, max = max,
      value = value, step = step, round = round,
      ticks = ticks, animate = animate, sep = sep, pre = pre, post = post)
  } else {
    control <- shiny::sliderInput(id, label, min = min, max = max,
      value = value, step = step, round = round, format = format, locale = locale,
      ticks = ticks, animate = animate)
  }

  create_input(id, value, map, control)
}


#' Create an interactive checkbox.
#'
#' @inheritParams shiny::checkboxInput
#' @inheritParams input_slider
#' @family interactive input
#' @export
#' @examples
#'
#' input_checkbox(label = "Confidence interval")
#' input_checkbox(label = "Confidence interval", value = TRUE)
#'
#' # Used in layer_smooths
#' mtcars %>% ggvis(~wt, ~mpg) %>%
#'   layer_smooths(se = input_checkbox(label = "Confidence interval"))
#'
#' # Used with a map function, to convert the boolean to another type of value
#' model_type <- input_checkbox(label = "Use flexible curve",
#'   map = function(val) if(val) "loess" else "lm")
#' mtcars %>% ggvis(~wt, ~mpg) %>%
#'   layer_model_predictions(model = model_type)
input_checkbox <- function(value = FALSE, label = "",
                           id = rand_id("checkbox_"), map = identity) {

  assert_that(is.string(label), is.string(id))

  create_input(id, value, map, shiny::checkboxInput(id, label, value))
}

#' Create an interactive text or numeric input box.
#'
#' \code{input_numeric} only allows numbers and comes with a spin box control.
#' \code{input_text} allows any type of input.
#'
#' @inheritParams shiny::textInput
#' @inheritParams input_slider
#' @family interactive input
#' @export
#' @examples
#' fill_text <- input_text(label = "Point color", value = "red")
#' mtcars %>% ggvis(~wt, ~mpg, fill := fill_text) %>% layer_bars()
#'
#' size_num <- input_numeric(label = "Point size", value = 25)
#' mtcars %>% ggvis(~wt, ~mpg, size := size_num) %>% layer_points()
input_text <- function(value, label = "", id = rand_id("text_"),
                       map = identity) {

  assert_that(is.string(label), is.string(id), is.string(value))

  create_input(id, value, map, shiny::textInput(id, label, value))
}

#' @rdname input_text
#' @export
input_numeric <- function(value, label = "", id = rand_id("numeric_"),
                          map = identity) {

  assert_that(is.string(label), is.string(id), is.numeric(value))

  create_input(id, value, map, shiny::numericInput(id, label, value))
}

#' Create interactive control to select one (or more options) from a list.
#'
#' \itemize{
#'  \item \code{input_radiobuttons} only ever selects one value
#'  \item \code{input_checkboxgroup} can alway select multiple values
#'  \item \code{input_select} can select only one if \code{multiple = FALSE},
#'    otherwise the user can select multiple by using modifier keys
#' }
#'
#' @inheritParams shiny::selectInput
#' @inheritParams input_slider
#' @family interactive input
#' @export
#' @examples
#' # Dropdown
#' input_select(c("a", "b", "c"))
#' input_select(c("a", "b", "c"), multiple = TRUE)
#' input_select(c("a", "b", "c"), selected = "c")
#'
#' # If you want to select variable names, you need to convert
#' # the output of the input to a name with map so that they get
#' # computed correctly
#' input_select(names(mtcars), map = as.name)
#'
#' # Radio buttons
#' input_radiobuttons(choices = c("Linear" = "lm", "LOESS" = "loess"),
#'                    label = "Model type")
#' input_radiobuttons(choices = c("Linear" = "lm", "LOESS" = "loess"),
#'                    selected = "loess",
#'                    label = "Model type")
#'
#' # Used in layer_model_predictions
#' mtcars %>% ggvis(~wt, ~mpg) %>%
#'   layer_model_predictions(model = input_radiobuttons(
#'     choices = c("Linear" = "lm", "LOESS" = "loess"),
#'     selected = "loess",
#'     label = "Model type"))
#'
#' # Checkbox group
#' mtcars %>% ggvis(x = ~wt, y = ~mpg) %>%
#'   layer_points(
#'     fill := input_checkboxgroup(
#'       choices = c("Red" = "r", "Green" = "g", "Blue" = "b"),
#'       label = "Point color components",
#'       map = function(val) {
#'         rgb(0.8 * "r" %in% val, 0.8 * "g" %in% val, 0.8 * "b" %in% val)
#'       }
#'     )
#'   )
input_select <- function(choices, selected = NULL, multiple = FALSE,
                         label = "", id = rand_id("select_"), map = identity,
                         selectize = FALSE) {

  assert_that(is.string(label), is.string(id))

  control <- shiny::selectInput(id, label, choices = choices,
                                selected = selected, multiple = multiple,
                                selectize = selectize)

  if (is.null(selected)) {
    if (multiple) value <- ""
    else value <- choices[1]
  } else {
    value <- selected
  }

  create_input(id, value, map, control)
}

#' @rdname input_select
#' @export
input_radiobuttons <- function(choices, selected = NULL, label = "",
                               id = rand_id("radio_"), map = identity) {

  assert_that(is.string(label), is.string(id))

  control <- shiny::radioButtons(id, label, choices = choices,
                                 selected = selected)

  if (is.null(selected)) {
    value <- choices[1]
  } else {
    value <- selected
  }

  create_input(id, value, map, control)
}

#' @rdname input_select
#' @export
input_checkboxgroup <- function(choices, selected = NULL, label = "",
                                id = rand_id("radio_"), map = identity) {

  assert_that(is.string(label), is.string(id))

  control <- shiny::checkboxGroupInput(id, label, choices = choices,
                                       selected = selected)

  if (is.null(selected)) {
    value <- character(0)
  } else {
    value <- selected
  }

  create_input(id, value, map, control)
}
