#' Overlay a smooth curve
#'
#' \code{layer_smooth} combines \code{\link{smooth}} with \code{mark_path} and
#' \code{mark_area} to display a smooth line and its standard error.
#'
#' @export
#' @examples
#' # Or do it directly with layer_smooth()
#' mtcars %>% ggvis(~wt, ~mpg) %>% layer_smooth()
#' mtcars %>% ggvis(~wt, ~mpg) %>% layer_point() %>%
#'   layer_smooth(method = "lm", se = FALSE)
#'
#' mtcars %>% ggvis(~wt, ~mpg) %>% layer_point() %>%
#'   layer_smooth(span = input_slider(0.2, 1))
#'
#' # These are equivalent to combining transform_smooth with mark_path and
#' # mark_area
#' mtcars %>%
#'   ggvis(~wt, ~mpg) %>%
#'   layer_point() %>%
#'   transform_smooth() %>%
#'   mark_area(props(x = ~x, y = ~y_lower__, y2 = ~y_upper__,
#'    fillOpacity := 0.2)) %>%
#'   mark_path(props(x = ~x, y = ~y))
#'
#' # You can also combine other data transformations like splitting
#' mtcars %>% ggvis(~wt, ~mpg, stroke = ~cyl) %>% group_by(cyl) %>%
#'   layer_smooth(method = "lm")
layer_smooth <- function(vis, props = NULL, formula = NULL, ..., se = FALSE) {

  # If formula not supplied, guess it from the x and y props.
  if (is.null(formula)) {
    cur_props <- vis$cur_props
    formula <- eval(substitute(x ~ y,
      list(x = cur_props$x.update$value, y = cur_props$y.update$value)),
      parent.frame())
    notify_guess(formula)
  }

  # Line shouldn't get fill-related props, and se area shouldn't get
  # stroke-related props.
  line_props <-  merge_props(props(x = ~pred_, y = ~resp_, strokeWidth := 2), props)
  line_props <- drop_props(line_props, c("fill", "fillOpacity"))

  se_props <- merge_props(props(x = ~pred_, y = ~resp_lwr_, y2 = ~resp_upr_,
    fillOpacity := 0.2), props)
  se_props <- drop_props(se_props, c("stroke", "strokeOpacity"))

  pipeline <- function(x) {
    x <- smooth(x, formula, ..., se = se)
    if (identical(se, TRUE)) {
      x <- mark_area(x, se_props)
    }
    x <- mark_path(x, line_props)
    x
  }
  branch_f(vis, pipeline)
}

#' Smooth data with a model.
#'
#' Fits a 1d model computes predictions and (optionaly) standard errors.
#'
#' @section Ouput:
#'
#' \code{smooth} creates a data frame with columns:
#'
#' \itemize{
#'  \item \code{resp_}: regularly spaced grid of \code{n} locations
#'  \item \code{pred_}: predicted value from smooth
#' }
#'
#' If standard errors are requested, it will also contain:
#'
#' \itemize{
#'  \item \code{pred_lwr_} and \code{pred_upr_}: upper and lower bounds of
#'    confidence interval
#'  \item \code{pred_se_}: the standard error (width of the confidence interval)
#' }
#'
#' @param method Model fitting function to use - it must support R's standard
#'   modelling interface, taking a formula and data frame as input, and
#'   returning predictions with \code{\link{predict}}. If not supplied, will
#'   use \code{\link{loess}} for <= 1000 points, otherwise it will use
#'   \code{\link[mgcv]{gam}}. Other modelling functions that will work include
#'   \code{\link{lm}}, \code{\link{glm}} and \code{\link[MASS]{rlm}}
#' @param formula Formula passed to modelling function. Can only any variables
#'   from data.
#' @param se include standard errors in output? Requires appropriate method of
#'   \code{predict_grid}, since the interface for returning predictions with
#'   standard errors is not consistent acrossing modelling frameworks.
#' @param level the confidence level of the standard errors.
#' @param n the number of grid points to use in the prediction
#' @param na.rm If \code{TRUE} missing values will be silently removed,
#'   otherwise they will be removed with a warning.
#' @param ... named arguments passed on to \code{method} function, unnamed
#'   arguments ignored.
#' @export
#' @examples
#' # Use smooth directly on data
#' mtcars %>% smooth(mpg ~ wt, n = 10)
#' mtcars %>% group_by(cyl) %>% smooth(mpg ~ wt, n = 10)
#' mtcars %>% smooth(mpg ~ wt, n = 10, method = "lm")
#'
#' # Plot the results
#' mtcars %>% smooth(mpg ~ wt) %>% ggvis(~pred_, ~resp_) %>% layer_line()
#' mtcars %>% ggvis() %>% smooth(mpg ~ wt) %>% layer_line(props(~pred_, ~resp_))
smooth <- function(x, formula, ..., method = NULL, se = TRUE,
                   level = 0.95, n = 80L) {
  UseMethod("smooth")
}

#' @export
smooth.data.frame <- function(x, formula, ..., method = NULL, se = TRUE,
                              level = 0.95, n = 80L) {
  assert_that(is.formula(formula))
  method <- method %||% guess_method(x)
  assert_that(is.string(method))

  # Create model environment & model call, then evaluate
  env <- new.env(parent = environment(formula))
  env$data <- x
  model_call <- make_call(method, formula, data = quote(data), ...)
  model <- eval(model_call, env)

  # Make prediction
  pred_grid(model, x, se = se, level = level, n = n)
}

#' @export
smooth.grouped_df <- function(x, formula, ..., method = NULL, se = TRUE,
                              level = 0.95, n = 80L) {
  dplyr::do(x, smooth(., formula = formula, method = method, se = se,
    level = level, n = n, ...))
}

#' @export
smooth.ggvis <- function(x, formula, ..., method = NULL, se = TRUE,
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

pred_grid <- function(model, data, n = 80, se = TRUE, level = 0.95) {
  assert_that(is.flag(se))
  assert_that(is.numeric(level), length(level) == 1, level >= 0, level <= 1)
  assert_that(length(n) == 1, n >= 0)

  UseMethod("pred_grid")
}

#' @export
pred_grid.loess <- function(model, data, n = 80, se = TRUE, level = 0.95) {
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

pred_grid.lm <- function(model, data, n = 80, se = TRUE, level = 0.95) {
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
      resp_ = as.vector(pred)
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
max_rows.grouped_df <- function(x) {
  x %>% group_size() %>% max()
}
