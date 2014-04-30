#' Overlay model predictions or a smooth curve
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
#' @export
#' @examples
#' mtcars %>% ggvis(~wt, ~mpg) %>% layer_smooths()
#' mtcars %>% ggvis(~wt, ~mpg) %>% layer_smooths(se = T)
#'
#' # Control appearance with props
#' mtcars %>% ggvis(~wt, ~mpg) %>%
#'   layer_smooths(se = T, stroke := "red", fill := "red", strokeWidth := 5)
#'
#' # Control the wiggliness with span. Default is 0.75
#' mtcars %>% ggvis(~wt, ~mpg) %>% layer_points() %>%
#'   layer_smooths(span = 0.2)
#' mtcars %>% ggvis(~wt, ~mpg) %>% layer_points() %>%
#'   layer_smooths(span = 1)
#' # Map to an input to modify interactively
#' mtcars %>% ggvis(~wt, ~mpg) %>% layer_point() %>%
#'   layer_smooths(span = input_slider(0.2, 1))
#'
#' # Use other modelling functions with layer_model_predictions
#' mtcars %>% ggvis(~wt, ~mpg) %>%
#'   layer_point() %>%
#'   layer_model_predictions(model = "lm") %>%
#'   layer_model_predictions(model = "MASS::rlm", stroke := "red")
#'
#' # layer_smooths() is just smooth() + layer_paths()
#' # Run smooth outside of a visualisation to see what variables you get
#' mtcars %>% smooth(mpg ~ wt)
#'
#' mtcars %>%
#'   ggvis(~wt, ~mpg) %>%
#'   layer_points() %>%
#'   smooth(mpg ~ wt) %>%
#'   layer_paths(~pred_, ~resp_, strokeWidth := 2)
layer_model_predictions <- function(vis, ..., model, formula = NULL,
                                    model_args = NULL, se = FALSE) {

  formula <- formula %||% guess_formula(vis$cur_props, model)

  # Construct default properties. Line shouldn't get fill-related props,
  # and se shouldn't get stroke-related props.
  props <- props(...)
  line_props <- merge_props(props(x = ~pred_, y = ~resp_, strokeWidth := 2),
    props)
  line_props <- drop_props(line_props, c("fill", "fillOpacity"))

  se_props <- merge_props(props(x = ~pred_, y = ~resp_lwr_, y2 = ~resp_upr_,
    fillOpacity := 0.2), props)
  se_props <- drop_props(se_props, c("stroke", "strokeOpacity"))

  pipeline <- function(x) {
    smooth_args <- c(list(x = x, formula = formula, method = model, se = se),
      model_args)
    x <- do.call("smooth", smooth_args)

    if (identical(se, TRUE)) {
      x <- emit_ribbons(x, se_props)
    }
    x <- emit_paths(x, line_props)
    x
  }
  branch_f(vis, pipeline)
}

#' @export
#' @rdname layer_model_predictions
layer_smooths <- function(vis, ..., span = 0.75, se = FALSE) {
  formula <- guess_formula(vis$cur_props, "loess", quiet = TRUE)
  layer_model_predictions(vis, ..., model = "loess", formula = formula,
    model_args = list(span = span), se = se)
}

guess_formula <- function(props, method, quiet = FALSE) {
  # FIXME: what's the right way to do this in general?
  vars <- list(
    x = props$x.update$value,
    y = props$y.update$value
  )

  if (identical(method, "gam")) {
    f <- substitute(y ~ s(x), vars)
  } else {
    f <- substitute(y ~ x, vars)
  }
  formula <- eval(f, parent.frame())
  if (!quiet) notify_guess(formula)
  formula
}

#' Smooth data with a model.
#'
#' Fit a 1d model, then compute predictions and (optionally) standard errors
#' over an evenly spaced grid.
#'
#' @param method Model fitting function to use - it must support R's standard
#'   modelling interface, taking a formula and data frame as input, and
#'   returning predictions with \code{\link{predict}}. If not supplied, will
#'   use \code{\link{loess}} for <= 1000 points, otherwise it will use
#'   \code{\link[mgcv]{gam}}. Other modelling functions that will work include
#'   \code{\link{lm}}, \code{\link{glm}} and \code{\link[MASS]{rlm}}.
#' @param formula Formula passed to modelling function. Can use any variables
#'   from data.
#' @param se include standard errors in output? Requires appropriate method of
#'   \code{predict_grid}, since the interface for returning predictions with
#'   standard errors is not consistent acrossing modelling frameworks.
#' @param level the confidence level of the standard errors.
#' @param n the number of grid points to use in the prediction
#' @param ... arguments passed on to \code{method} function
#' @return A data frame with columns:
#'  \item{\code{resp_}}{regularly spaced grid of \code{n} locations}
#'  \item{\code{pred_}}{predicted value from smooth}
#'  \item{\code{pred_lwr_} and \code{pred_upr_}}{upper and lower bounds of
#'    confidence interval (if \code{se = TRUE})}
#'  \item{\code{pred_se_}}{the standard error (width of the confidence interval)
#'    (if \code{se = TRUE})}
#' @export
#' @examples
#' mtcars %>% smooth(mpg ~ wt, n = 10)
#' mtcars %>% smooth(mpg ~ wt, n = 10, se = TRUE)
#' mtcars %>% group_by(cyl) %>% smooth(mpg ~ wt, n = 10)
#'
#' # Override method to suppress message or change approach
#' mtcars %>% smooth(mpg ~ wt, n = 10, method = "loess")
#' mtcars %>% smooth(mpg ~ wt, n = 10, method = "lm")
#'
#' # Plot the results
#' mtcars %>% smooth(mpg ~ wt) %>% ggvis(~pred_, ~resp_) %>% layer_line()
#' mtcars %>% ggvis() %>% smooth(mpg ~ wt) %>% layer_line(props(~pred_, ~resp_))
smooth <- function(x, formula, ..., method = NULL, se = FALSE,
                   level = 0.95, n = 80L) {
  UseMethod("smooth")
}

#' @export
smooth.data.frame <- function(x, formula, ..., method = NULL, se = FALSE,
                              level = 0.95, n = 80L) {
  assert_that(is.formula(formula))
  method <- method %||% guess_method(x)
  assert_that(is.string(method))

  if (is.character(method)) {
    # This allows the use of e.g. MASS::rlm
    method <- parse(text = method)[[1]]
  }

  # Create model environment & model call, then evaluate
  env <- new.env(parent = environment(formula))
  env$data <- x
  model_call <- make_call(method, formula, data = quote(data), ...)
  model <- eval(model_call, env)

  # Make prediction
  pred_grid(model, x, se = se, level = level, n = n)
}

#' @export
smooth.grouped_df <- function(x, formula, ..., method = NULL, se = FALSE,
                              level = 0.95, n = 80L) {
  dplyr::do(x, smooth(., formula = formula, method = method, se = se,
    level = level, n = n, ...))
}

#' @export
smooth.ggvis <- function(x, formula, ..., method = NULL, se = FALSE,
                         level = 0.95, n = 80L) {
  args <- list(formula = formula, method = method, se = se, level = level,
    n = n, ...)
  x <- register_reactives(x, args)

  new_data <- reactive({
    data <- x$cur_data()
    output <- do.call("smooth", c(list(x = data), lapply(args, value)))
    preserve_constants(data, output)
  })

  # FIXME: modify register_data so this creates informative name.
  # Maybe use slashes to make the data hierarchy more clear?
  register_data(x, new_data, "_transform_smooth")
}

guess_method <- function(data) {
  method <- if (max_rows(data) > 1000) "gam" else "loess"
  notify_guess(method)
  method
}

notify_guess <- function(x) {
  message("Guessing ", deparse(substitute(x)), " = ", format(x))
}

# Helper function to create data frame of predictions -------------------------

pred_grid <- function(model, data, n = 80, se = FALSE, level = 0.95) {
  assert_that(is.flag(se))
  assert_that(is.numeric(level), length(level) == 1, level >= 0, level <= 1)
  assert_that(length(n) == 1, n >= 0)

  UseMethod("pred_grid")
}

#' @export
pred_grid.loess <- function(model, data, n = 80, se = FALSE, level = 0.95) {
  if (length(model$xnames) > 1) {
    stop("Only know how to make grid for one variable", call. = FALSE)
  }

  x_rng <- range(model$x, na.rm = TRUE)
  x_grid <- seq(x_rng[1], x_rng[2], length = n)
  grid <- setNames(data.frame(x_grid), model$xnames)
  resp <- predict(model, newdata = grid, se = se)

  if (!se) {
    data.frame(
      pred_ = x_grid,
      resp_ = as.vector(resp)
    )
  } else {
    ci <- resp$se.fit * qt(level / 2 + .5, resp$df)
    data.frame(
      pred_ = x_grid,
      resp_ = resp$fit,
      resp_lwr_ = resp$fit - ci,
      resp_upr_ = resp$fit + ci,
      resp_se_ = resp$se.fit
    )
  }
}

pred_grid.lm <- function(model, data, n = 80, se = FALSE, level = 0.95) {
  x_var <- attr(terms(model), "term.labels")
  if (length(x_var) > 1) {
    stop("Only know how to make grid for one variable", call. = FALSE)
  }

  x_rng <- range(data[[x_var]], na.rm = TRUE)
  x_grid <- seq(x_rng[1], x_rng[2], length = n)
  grid <- setNames(data.frame(x_grid), x_var)

  resp <- predict(model, newdata = grid, se = se,
    level = level, interval = if(se) "confidence" else "none")

  if (!se) {
    data.frame(
      pred_ = x_grid,
      resp_ = as.vector(resp)
    )
  } else {
    data.frame(
      pred_ = x_grid,
      resp_ = resp$fit[, 1],
      resp_lwr_ = resp$fit[, 2],
      resp_upr_ = resp$fit[, 3],
      resp_se_ = resp$se.fit
    )
  }
}

# Helper function to determin maximum number of rows ---------------------------

max_rows <- function(x) UseMethod("max_rows")
#' @export
max_rows.data.frame <- function(x) nrow(x)
#' @export
max_rows.grouped_df <- function(x) max(group_size(x))
