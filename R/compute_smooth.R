#' Smooth data with a model.
#'
#' Fit a 1d model, then compute predictions and (optionally) standard errors
#' over an evenly spaced grid.
#'
#' @param x Dataset-like object to smooth. Built-in methods for data frames,
#'   grouped data frames and ggvis visualisations.
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
#' mtcars %>% compute_smooth(mpg ~ wt, n = 10)
#' mtcars %>% compute_smooth(mpg ~ wt, n = 10, se = TRUE)
#' mtcars %>% group_by(cyl) %>% compute_smooth(mpg ~ wt, n = 10)
#'
#' # Override method to suppress message or change approach
#' mtcars %>% compute_smooth(mpg ~ wt, n = 10, method = "loess")
#' mtcars %>% compute_smooth(mpg ~ wt, n = 10, method = "lm")
#'
#' # Plot the results
#' mtcars %>% compute_smooth(mpg ~ wt) %>% ggvis(~pred_, ~resp_) %>% layer_paths()
#' mtcars %>% ggvis() %>% compute_smooth(mpg ~ wt) %>% layer_paths(~pred_, ~resp_)
compute_smooth <- function(x, formula, ..., method = NULL, se = FALSE,
                           level = 0.95, n = 80L) {
  UseMethod("compute_smooth")
}

#' @export
compute_smooth.data.frame <- function(x, formula, ..., method = NULL,
                                      se = FALSE, level = 0.95, n = 80L) {
  assert_that(is.formula(formula))
  method <- method %||% guess_method(x)
  assert_that(is.string(method))

  restore <- identity

  if (is.character(method)) {
    # This allows the use of e.g. MASS::rlm
    method <- parse(text = method)[[1]]
  }

  if (method == quote(loess)) {
    # loess can't handle POSIXct, so convert to numeric. Fortunately we know
    # that loess only has a single predictor to extract.
    pred_var <- formula[[3]]
    pred_vals <- eval(pred_var, x)
    if (inherits(pred_vals, "POSIXct")) {
      x[[as.character(pred_var)]] <- as.numeric(pred_vals)
      tz <- attr(pred_vals, "tzone", TRUE)

      restore <- function(data) {
        data$pred_ <- as.POSIXct(data$pred_, origin = "1970-01-01", tz = tz)
        data
      }
    } else if (inherits(pred_vals, "Date")) {
      x[[as.character(pred_var)]] <- as.numeric(pred_vals)

      restore <- function(data) {
        data$pred_ <- structure(data$pred_, class = "Date")
        data
      }
    }
  }

  # Create model environment & model call, then evaluate
  env <- new.env(parent = environment(formula))
  env$data <- x
  model_call <- make_call(method, formula, data = quote(data), ...)
  model <- eval(model_call, env)

  # Make prediction
  res <- pred_grid(model, x, se = se, level = level, n = n)
  restore(res)
}

#' @export
compute_smooth.grouped_df <- function(x, formula, ..., method = NULL,
                                      se = FALSE, level = 0.95, n = 80L) {
  dplyr::do(x, compute_smooth(., formula = formula, method = method, se = se,
    level = level, n = n, ...))
}

globalVariables(".")

#' @export
compute_smooth.ggvis <- function(x, formula, ..., method = NULL, se = FALSE,
                                 level = 0.95, n = 80L) {
  args <- list(formula = formula, method = method, se = se, level = level,
    n = n, ...)

  register_computation(x, args, "smooth", function(data, args) {
    output <- do_call(compute_smooth, quote(data), .args = args)
    preserve_constants(data, output)
  })
}

guess_method <- function(data) {
  method <- if (max_rows(data) > 1000) "gam" else "loess"
  notify_guess(method)
  method
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

#' @export
pred_grid.lm <- function(model, data, n = 80, se = FALSE, level = 0.95) {
  x_var <- attr(terms(model), "term.labels", TRUE)
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
