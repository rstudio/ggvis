#' Transformation: smooth the data
#'
#' \code{transform_smooth} is a data transformation that can be passed into a
#' node. \code{branch_smooth} creates a node that transforms the data and then
#' displays it with \code{\link{mark_line}}.
#'
#' @section Ouput:
#'
#' \code{transform_smooth} creates a data frame with columns:
#'
#' \itemize{
#'  \item \code{x}
#'  \item \code{y}
#' }
#'
#' If standard errors are requested, it will also contain:
#'
#' \itemize{
#'  \item \code{y_lower}
#'  \item \code{y_upper}
#'  \item \code{se}
#' }
#'
#' @param method Model fitting function to use - it must support R's standard
#'   modelling interface, taking a formula and data frame as input, and
#'   returning predictions with \code{\link{predict}}. If not supplied, will
#'   use \code{\link{loess}} for <= 1000 points, otherwise it will use
#'   \code{\link[mgcv]{gam}}. Other modelling functions that will work include
#'   \code{\link{lm}}, \code{\link{glm}} and \code{\link[MASS]{rlm}}
#' @param formula Formula passed to modelling function. Can only use \code{y}
#'   and \code{x} variables.  If not specified, defaults to \code{y ~ s(x)}
#'   for \code{method = gam}, \code{y ~ x} otherwise.
#' @param se include standard errors in output? Requires approprate method of
#'   \code{predictdf}, since the interface for returning predictions with
#'   standard errors is not consistent acrossing modelling frameworks.
#' @param level the confidence level of the standard errors.
#' @param n the number of grid points to use in the prediction
#' @param na.rm If \code{TRUE} missing values will be silently removed,
#'   otherwise they will be removed with a warning.
#' @export
#' @examples
#' transform_smooth()
#' sluice(transform_smooth(), props(x ~ mpg, y ~ disp), mtcars)
#' # You can see the results of a transformation by creating your own pipeline
#' # and flowing data through it
#' sluice(transform_smooth(n = 5L), props(x ~ disp, y ~ mpg), mtcars)
#' # Or
#' pl <- pipeline(
#'   mtcars,
#'   by_group(quote(cyl)),
#'   transform_smooth(n = 5L, method = "lm")
#' )
#' sluice(pl, props(x ~ disp, y ~ mpg))
transform_smooth <- function(method = guess(), formula = guess(), se = TRUE,
                             level = 0.95, n = 80L, na.rm = FALSE, ...) {
  transform("smooth", method = method, formula = formula, se = se,
    level = level, n = n, na.rm = na.rm, dots = list(...))
}

#' @rdname transform_smooth
#' @export
#' @inheritParams branch_histogram
branch_smooth <- function(props = NULL, ...) {
  if (is.null(props)) props <- props()
  node(
    data = transform_smooth(...),
    mark_line(props)
  )
}

#' @S3method format transform_smooth
format.transform_smooth <- function(x, ...) {
  paste0(" -> smooth()", param_string(x[c("method", "formula")]))
}

#' @S3method compute transform_smooth
compute.transform_smooth <- function(x, props, data) {
  check_prop(x, props, data, "x", "numeric")
  check_prop(x, props, data, "y", "numeric")

  if (is.guess(x$method)) {
    x$method <- if (max_rows(data) > 1000) "gam" else "loess"
    message("Guess transform_smooth(method = '", x$method, "')")
  }
  if (is.guess(x$formula)) {
    if (x$method == "gam")
      formula <- sprintf("%s ~ s(%s)", props$y, props$x)
    else
      formula <- sprintf("%s ~ %s", props$y, props$x)

    x$formula <- as.formula(formula)
    message("Guess transform_smooth(formula = ", formula, ")")
  }

  output <- smooth(data, x, x_var = props$x, y_var = props$y)
  preserve_constants(data, output)
}

smooth <- function(data, trans, x_var, y_var) UseMethod("smooth")

#' @S3method smooth split_df
smooth.split_df <- function(data, trans, x_var, y_var) {
  data[] <- lapply(data, smooth, trans = trans, x_var = x_var, y_var = y_var)
  data
}

#' @S3method smooth data.frame
smooth.data.frame <- function(data, trans, x_var, y_var) {
  assert_that(is.formula(trans$formula))
  assert_that(is.flag(trans$se))
  assert_that(is.numeric(trans$level), length(trans$level) == 1,
              trans$level >= 0, trans$level <= 1)
  assert_that(length(trans$n) == 1, trans$n >= 0)
  assert_that(is.flag(trans$na.rm))

  x_name <- prop_name(x_var)
  y_name <- prop_name(y_var)

  env <- new.env(parent = globalenv())
  env$data <- data.frame(
    prop_value(x_var, data),
    prop_value(y_var, data)
  )
  names(env$data) <- c(x_name, y_name)

  # Create model call and combine with ... captured earlier, evaluating in
  args <- c(list(trans$formula, data = quote(data)), trans$dots)
  mod <- do.call(trans$method, args)

  # Make prediction
  x_grid <- seq(min(env$data[[x_name]]), max(env$data[[x_name]]), length = trans$n)
  predict_df(mod, x_name, y_name, x_grid, trans$se, trans$level)
}

# Helper function to create data frame of predictions -------------------------

predict_df <- function(model, x_name, y_name, x_grid, se, level) UseMethod("predict_df")

#' @S3method predict_df lm
predict_df.lm <- function(model, x_name, y_name, x_grid, se, level) {
  dat <- data.frame(x_grid)
  names(dat) <- x_name
  pred <- predict(model, newdata = dat, se = se,
    level = level, interval = if(se) "confidence" else "none")

  if (se) {
    fit <- as.data.frame(pred$fit)
    names(fit) <- c(y_name, "y_min", "y_max")
    dat <- cbind(dat, fit, se = pred$se)
  } else {
    dat[[y_name]] <- as.vector(pred)
  }
  dat
}

#' @S3method predict_df loess
predict_df.loess <- function(model, x_name, y_name, x_grid, se, level) {
  dat <- data.frame(x_grid)
  names(dat) <- x_name
  pred <- predict(model, newdata = dat, se = se)

  if (se) {
    dat[[y_name]] <- pred$fit
    ci <- pred$se.fit * qt(level / 2 + .5, pred$df)
    dat$y_min <- dat[[y_name]] - ci
    dat$y_max <- dat[[y_name]] + ci
    dat$se <- pred$se.fit
  } else {
    dat[[y_name]] <- as.vector(pred)
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
