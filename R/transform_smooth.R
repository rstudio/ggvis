#' Transformation: smooth the data
#' 
#' @export
#' @examples
#' transform_smooth()
#' flow(transform_smooth(), props(x ~ mpg, y ~ disp), mtcars)
#' # You can see the results of a transformation by creating your own pipeline
#' # and flowing data through it
#' flow(transform_smooth(n = 5L), props(x ~ disp, y ~ mpg), mtcars)
#' # Or
#' pl <- pipeline("mtcars", by_group("cyl"), transform_smooth(n = 5L))
#' flow(pl, props(x ~ disp, y ~ mpg))
transform_smooth <- function(method = guess(), formula = guess(), se = TRUE,
                             level = 0.95, n = 80L, na.rm = FALSE, ...) {
  assert_that(is.guess(method) || is.string(method))
  assert_that(is.guess(formula) || is.formula(formula))
  assert_that(is.flag(se))
  assert_that(is.numeric(level), length(level) == 1, level >= 0, level <= 1)
  assert_that(is.integer(n), length(n) == 1, n >= 0)
  assert_that(is.flag(na.rm))

  transform("smooth", method = method, formula = formula, se = se,
    level = level, n = n, na.rm = na.rm, dots = dots(...))
}

#' @S3method format transform_smooth
format.transform_smooth <- function(x, ...) {
  paste0(" -> smooth()", param_string(x[c("method", "formula")]))
}

#' @S3method flow transform_smooth
flow.transform_smooth <- function(x, props, data) {
  if (is.null(props$x)) {
    stop("transform_smooth needs x variable", call. = FALSE)
  }
  type <- prop_type(data, props$x)
  if (!(type %in% c("double", "integer"))) {
    stop("transform_bin needs numeric x", call. = FALSE)
  }
    
  if (is.null(props$y)) {
    stop("transform_smooth needs y variable", call. = FALSE)
  }
  type <- prop_type(data, props$y)
  if (!(type %in% c("double", "integer"))) {
    stop("transform_bin needs numeric y", call. = FALSE)
  }
  
  if (is.guess(x$method)) {
    x$method <- if (max_rows(data) > 1000) "gam" else "loess"
    message("Guess transform_smooth(method = '", x$method, "')")
  }
  if (is.guess(x$formula)) {
    x$formula <- if (x$method == "gam") y ~ s(x) else y ~ x
    message("Guess transform_smooth(formula = '", x$formula, "')")
  }
  x$method <- as.name(x$method)

  smooth(data, x, x_var = props$x, y_var = props$y)
}

smooth <- function(data, trans, x_var, y_var) UseMethod("smooth")

#' @S3method smooth split_df
smooth.split_df <- function(data, trans, x_var, y_var) {
  structure(
    lapply(data, smooth, trans = trans, x_var = x_var, y_var = y_var),
    class = "split_df"
  )
}

#' @S3method smooth data.frame
smooth.data.frame <- function(data, trans, x_var, y_var) {
  env <- new.env(parent = globalenv())
  env$data <- data.frame(
    x = prop_value(x_var, data), 
    y = prop_value(y_var, data)
  )
  
  # Create model call and combine with ... captured earlier, evaluating in
  call <- substitute(method(formula, data = data), trans)
  call <- modify_call(call, trans$dots)
  mod <- eval(call, env)
  
  # Make prediction
  x_grid <- seq(min(env$data$x), max(env$data$x), length = trans$n)
  predict_df(mod, x_grid, trans$se, trans$level)
}

# Helper function to create data frame of predictions -------------------------

predict_df <- function(model, x_grid, se, level) UseMethod("predict_df")

#' @S3method predict_df lm
predict_df.lm <- function(model, x_grid, se, level) {
  dat <- data.frame(x = x_grid)
  pred <- predict(model, newdata = dat, se = se,
    level = level, interval = if(se) "confidence" else "none")

  if (se) {
    fit <- as.data.frame(pred$fit)
    names(fit) <- c("y", "y_min", "y_max")
    dat <- cbind(dat, fit, se = pred$se)
  } else {
    dat$y <- as.vector(pred)
  }
  dat
}

#' @S3method predict_df loess
predict_df.loess <- function(model, x_grid, se, level) {
  dat <- data.frame(x = x_grid)
  pred <- predict(model, newdata = dat, se = se)

  if (se) {
    dat$y <- pred$fit
    ci <- pred$se.fit * qt(level / 2 + .5, pred$df)
    dat$y_min <- dat$y - ci
    dat$y_max <- dat$y + ci
    dat$se <- pred$se.fit
  } else {
    dat$y <- as.vector(pred)
  }
  dat
}

# Helper function to determin maximum number of rows ---------------------------

max_rows <- function(x) UseMethod("max_rows")
#' @S3method max_rows data.frame
max_rows.data.frame <- function(x) nrow(x)
#' @S3method max_rows split_df
max_rows.split_df <- function(x) {
  rows <- vapply(x, nrow, integer(1))
  max(rows)
}
