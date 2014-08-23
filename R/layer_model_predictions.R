#' Overlay model predictions or a smooth curve.
#'
#' \code{layer_model_predictions} fits a model to the data and draw it with
#' \code{layer_paths} and, optionally, \code{layer_ribbons}.
#' \code{layer_smooths} is a special case of layering model predictions where
#' the model is a smooth loess curve whose smoothness is controlled by the
#' \code{span} parameter.
#'
#' @param vis Visualisation to modify
#' @param ... Visual properties. Stroke properties control only affect line,
#'   fill properties only affect standard error band.
#' @param model Name of the model as a string, e.g. \code{"loess"}, \code{"lm"},
#'   or \code{"MASS::rlm"}. Must be the name of a function that produces a
#'   standard model object with a \code{\link{predict}} method. For
#'   \code{layer_smooth} this is always "loess".
#' @param formula Model formula. If not supplied, guessed from the visual
#'   properties, constructing \code{y ~ x}.
#' @param model_args A list of additional arguments passed on to the
#'   \code{model} function.
#' @param se Also display a point-wise standard error band? Defaults to
#'   \code{FALSE} because interpretation is non-trivial.
#' @param span For \code{layer_smooth}, the span of the loess smoother.
#' @param domain If \code{NULL} (the default), the domain of the predicted
#'   values will be the same as the domain of the prediction variable in the
#'   data. It can also be a two-element numeric vector specifying the min and
#'   max.
#' @export
#' @examples
#' mtcars %>% ggvis(~wt, ~mpg) %>% layer_smooths()
#' mtcars %>% ggvis(~wt, ~mpg) %>% layer_smooths(se = TRUE)
#'
#' # Use group by to display multiple smoothes
#' mtcars %>% ggvis(~wt, ~mpg) %>% group_by(cyl) %>% layer_smooths()
#'
#' # Control appearance with props
#' mtcars %>% ggvis(~wt, ~mpg) %>%
#'   layer_smooths(se = TRUE, stroke := "red", fill := "red", strokeWidth := 5)
#'
#' # Control the wiggliness with span. Default is 0.75
#' mtcars %>% ggvis(~wt, ~mpg) %>% layer_points() %>%
#'   layer_smooths(span = 0.2)
#' mtcars %>% ggvis(~wt, ~mpg) %>% layer_points() %>%
#'   layer_smooths(span = 1)
#' # Map to an input to modify interactively
#' mtcars %>% ggvis(~wt, ~mpg) %>% layer_points() %>%
#'   layer_smooths(span = input_slider(0.2, 1))
#'
#' # Use other modelling functions with layer_model_predictions
#' mtcars %>% ggvis(~wt, ~mpg) %>%
#'   layer_points() %>%
#'   layer_model_predictions(model = "lm") %>%
#'   layer_model_predictions(model = "MASS::rlm", stroke := "red")
#'
#' # Custom domain for predictions
#' mtcars %>% ggvis(~wt, ~mpg) %>% layer_points() %>%
#'   layer_model_predictions(model = "lm", domain = c(0, 8))
#' mtcars %>% ggvis(~wt, ~mpg) %>% layer_points() %>%
#'   layer_model_predictions(model = "lm",
#'     domain = input_slider(0, 10, value = c(1, 4)))
#'
#' # layer_smooths() is just compute_smooth() + layer_paths()
#' # Run loess or other model outside of a visualisation to see what variables
#' # you get.
#' mtcars %>% compute_smooth(mpg ~ wt)
#' mtcars %>% compute_model_prediction(mpg ~ wt, model = "lm")
#'
#' mtcars %>%
#'   ggvis(~wt, ~mpg) %>%
#'   layer_points() %>%
#'   compute_smooth(mpg ~ wt) %>%
#'   layer_paths(~pred_, ~resp_, strokeWidth := 2)
layer_model_predictions <- function(vis, ..., model, formula = NULL,
                                    model_args = NULL, se = FALSE,
                                    domain = NULL) {

  vis <- set_scale_label(vis, "x", prop_label(cur_props(vis)$x.update))
  vis <- set_scale_label(vis, "y", prop_label(cur_props(vis)$y.update))

  formula <- formula %||% guess_formula(vis$cur_props, model)
  props <- stroke_fill_defaults(props(...),
    stroke = props(~pred_, ~resp_, strokeWidth := 2),
    fill =   props(~pred_, ~resp_lwr_, y2 = ~resp_upr_, fillOpacity := 0.2)
  )

  pipeline <- function(x) {
    x <- do_call(compute_model_prediction, quote(x), formula = formula,
                 model = model, se = se, domain = domain, .args = model_args)

    if (identical(se, TRUE)) {
      x <- emit_ribbons(x, props$fill)
    }
    x <- emit_paths(x, props$stroke)
    x
  }
  layer_f(vis, pipeline)
}

#' @export
#' @rdname layer_model_predictions
layer_smooths <- function(vis, ..., span = 0.75, se = FALSE) {
  formula <- guess_formula(vis$cur_props, "loess", quiet = TRUE)
  layer_model_predictions(vis, ..., model = "loess", formula = formula,
    model_args = list(span = span), se = se)
}

guess_formula <- function(props, model, quiet = FALSE) {
  vars <- list(
    x = find_prop_var(props, "x.update")[[2]],
    y = find_prop_var(props, "y.update")[[2]]
  )

  if (identical(model, "gam")) {
    f <- substitute(y ~ s(x), vars)
  } else {
    f <- substitute(y ~ x, vars)
  }
  formula <- eval(f, parent.frame())
  if (!quiet) notify_guess(formula)
  formula
}


# Helper function to determin maximum number of rows ---------------------------

max_rows <- function(x) UseMethod("max_rows")
#' @export
max_rows.data.frame <- function(x) nrow(x)
#' @export
max_rows.grouped_df <- function(x) max(dplyr::group_size(x))
