#' Create a model of a data set and compute predictions.
#'
#' Fit a 1d model, then compute predictions and (optionally) standard errors
#' over an evenly spaced grid.
#'
#' \code{compute_model_prediction} fits a model to the data and makes
#' predictions with it. \code{compute_smooth} is a special case of model
#' predictions where the model is a smooth loess curve whose smoothness is
#' controlled by the \code{span} parameter.
#'
#' @param x Dataset-like object to model and predict. Built-in methods for data
#'   frames, grouped data frames and ggvis visualisations.
#' @param model Model fitting function to use - it must support R's standard
#'   modelling interface, taking a formula and data frame as input, and
#'   returning predictions with \code{\link{predict}}. If not supplied, will use
#'   \code{\link{loess}} for <= 1000 points, otherwise it will use
#'   \code{\link[mgcv]{gam}}. Other modelling functions that will work include
#'   \code{\link{lm}}, \code{\link{glm}} and \code{\link[MASS]{rlm}}.
#' @param formula Formula passed to modelling function. Can use any variables
#'   from data.
#' @param se include standard errors in output? Requires appropriate method of
#'   \code{predict_grid}, since the interface for returning predictions with
#'   standard errors is not consistent acrossing modelling frameworks.
#' @param level the confidence level of the standard errors.
#' @param n the number of grid points to use in the prediction
#' @param domain If \code{NULL} (the default), the domain of the predicted
#'   values will be the same as the domain of the prediction variable in the
#'   data. It can also be a two-element numeric vector specifying the min and
#'   max.
#' @param ... arguments passed on to \code{model} function
#' @param method Deprecated. Please use \code{model} instead.
#' @param span Smoothing span used for loess model.
#' @return A data frame with columns: \item{\code{resp_}}{regularly spaced grid
#'   of \code{n} locations} \item{\code{pred_}}{predicted value from model}
#'   \item{\code{pred_lwr_} and \code{pred_upr_}}{upper and lower bounds of
#'   confidence interval (if \code{se = TRUE})} \item{\code{pred_se_}}{the
#'   standard error (width of the confidence interval) (if \code{se = TRUE})}
#' @export
#' @examples
#' # Use a small value of n for these examples
#' mtcars %>% compute_model_prediction(mpg ~ wt, n = 10)
#' mtcars %>% compute_model_prediction(mpg ~ wt, n = 10, se = TRUE)
#' mtcars %>% group_by(cyl) %>% compute_model_prediction(mpg ~ wt, n = 10)
#'
#' # compute_smooth defaults to loess
#' mtcars %>% compute_smooth(mpg ~ wt)
#'
#' # Override model to suppress message or change approach
#' mtcars %>% compute_model_prediction(mpg ~ wt, n = 10, model = "loess")
#' mtcars %>% compute_model_prediction(mpg ~ wt, n = 10, model = "lm")
#'
#' # Set the domain manually
#' mtcars %>%
#'   compute_model_prediction(mpg ~ wt, n = 20, model = "lm", domain = c(0, 8))
#'
#' # Plot the results
#' mtcars %>% compute_model_prediction(mpg ~ wt) %>%
#'   ggvis(~pred_, ~resp_) %>%
#'   layer_paths()
#' mtcars %>% ggvis() %>%
#'   compute_model_prediction(mpg ~ wt) %>%
#'   layer_paths(~pred_, ~resp_)
compute_model_prediction <- function(x, formula, ..., model = NULL, se = FALSE,
                              level = 0.95, n = 80L, domain = NULL, method) {
  if (!missing(method)) {
    deprecated("method", version = "0.3")
    model <- method
  }
  UseMethod("compute_model_prediction")
}

#' @export
compute_model_prediction.data.frame <- function(x, formula, ..., model = NULL,
                                         se = FALSE, level = 0.95, n = 80L,
                                         domain = NULL, method) {
  assert_that(is.formula(formula))
  model <- model %||% guess_model(x)
  assert_that(is.string(model))

  if (nrow(x) == 0) {
    return(empty_smooth(se))
  }

  restore <- identity

  if (is.character(model)) {
    # This allows the use of e.g. MASS::rlm
    model <- parse(text = model)[[1]]
  }

  if (model == quote(loess)) {
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

  # Create model environment & model call
  env <- new.env(parent = environment(formula))
  env$data <- x
  model_call <- make_call(model, formula, data = quote(data), ...)

  # Fit model and make predictions
  model <- eval(model_call, env)
  res <- pred_grid(model, x, se = se, level = level, n = n, domain = domain)
  restore(res)
}

empty_smooth <- function(se = FALSE) {
  res <- data.frame(resp_ = numeric(0), pred_ = numeric(0))
  if (se) {
    res$pred_lwr_ <- numeric(0)
    res$pred_upr_ <- numeric(0)
    res$pred_se_ <- numeric(0)
  }
  res
}

#' @export
compute_model_prediction.grouped_df <- function(x, formula, ..., model = NULL,
                                         se = FALSE, level = 0.95, n = 80L,
                                         domain = NULL, method) {
  dplyr::do(x, compute_model_prediction(., formula = formula, model = model,
    se = se, level = level, n = n, domain = domain, ...))
}

globalVariables(".")

#' @export
compute_model_prediction.ggvis <- function(x, formula, ..., model = NULL,
                                           se = FALSE, level = 0.95, n = 80L,
                                           domain = NULL, method) {
  args <- list(formula = formula, model = model, se = se, level = level,
               n = n, domain = domain, ...)

  register_computation(x, args, "model_prediction", function(data, args) {
    output <- do_call(compute_model_prediction, quote(data), .args = args)
    preserve_constants(data, output)
  })
}


#' @rdname compute_model_prediction
#' @export
compute_smooth <- function(x, formula, ..., span = 0.75, se = FALSE) {
  compute_model_prediction(x, formula, ..., model = "loess", span = span,
                           se = se)
}

guess_model <- function(data) {
  model <- if (max_rows(data) > 1000) "gam" else "loess"
  notify_guess(model)
  model
}

# Helper function to create data frame of predictions -------------------------

pred_grid <- function(model, data, domain = NULL, n = 80, se = FALSE,
                      level = 0.95) {
  assert_that(is.flag(se))
  assert_that(is.numeric(level), length(level) == 1, level >= 0, level <= 1)
  assert_that(length(n) == 1, n >= 0)
  assert_that(is.null(domain) || length(domain) == 2)

  UseMethod("pred_grid")
}

#' @export
pred_grid.loess <- function(model, data, domain = NULL, n = 80, se = FALSE,
                            level = 0.95) {
  if (length(model$xnames) > 1) {
    stop("Only know how to make grid for one variable", call. = FALSE)
  }

  x_rng <- domain %||% range(model$x, na.rm = TRUE)
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
pred_grid.lm <- function(model, data, domain = NULL, n = 80, se = FALSE,
                         level = 0.95) {
  x_var <- get_predict_vars(terms(model))
  if (length(x_var) > 1) {
    stop("Only know how to make grid for one variable", call. = FALSE)
  }

  x_rng <- domain %||% range(data[[x_var]], na.rm = TRUE)
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


# Given a formula object, return a character vector of predictor variables
get_predict_vars <- function(f) {
  if (!is.formula(f))
    stop("f must be a formula object")
  if (length(f) != 3)
    stop("Formula must have components on both sides of `~`")

  all.vars(f[[3]])
}
