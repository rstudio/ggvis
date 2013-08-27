#' Transformation: smooth the data
#'
#' \code{transform_smooth} is a data transformation that fits a model to a 2d
#' dataset and computes predictions and standard errors. \code{branch_smooth}
#' combines \code{transform_smooth} with \code{mark_line} and \code{mark_area} 
#' to display a smooth line and its standard errror.
#'
#' @section Input:
#' Currently \code{transform_bin} only works with data frames and requires the
#' following properties:
#' 
#' \itemize{
#'   \item \code{x}, numeric, horizontal position
#'   \item \code{y}, numeric, vertical position
#' }
#' 
#' @section Ouput:
#'
#' \code{transform_smooth} creates a data frame with columns:
#'
#' \itemize{
#'  \item \code{x}: regularly spaced grid of \code{n} locations
#'  \item \code{y}: predicted value from smooth
#' }
#'
#' If standard errors are requested, it will also contain:
#'
#' \itemize{
#'  \item \code{y_lower__} and \code{y_upper__}: upper and lower bounds of 
#'    confidence interval
#'  \item \code{se__}: the standard error (width of the confidence interval)
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
#' ggvis(mtcars, props(x ~ wt, y ~ mpg), mark_symbol(), branch_smooth())
#' ggvis(mtcars, props(x ~ wt, y ~ mpg), mark_symbol(), 
#'   branch_smooth(method = "lm", se = F))
#' 
#' # These are equivalent to combining transform_smooth with mark_line and
#' # mark_area
#' ggvis(mtcars, props(x ~ wt, y ~ mpg), 
#'   mark_symbol(), 
#'   node(transform_smooth(), 
#'     mark_area(props(x ~ x, y ~ y_lower__, y2 ~ y_upper__, fillOpacity = 0.2)),
#'     mark_line(props(x ~ x, y ~ y))
#'   )
#' )
#'
#' # You can also combine other data transformations like splitting
#' ggvis(mtcars, props(x ~ wt, y ~ mpg, stroke ~ cyl), by_group(cyl),
#'    branch_smooth(method = lm))
#' 
#' # You can see the results of a transformation by creating your own pipeline
#' # and sluicing data through it
#' sluice(pipeline(mtcars, transform_smooth(n = 5L)), props(x ~ disp, y ~ mpg)
transform_smooth <- function(method = guess(), formula = guess(), se = TRUE,
                             level = 0.95, n = 80L, na.rm = FALSE, ...) {  
  
  transform("smooth", method = method, formula = formula, se = se,
    level = level, n = n, na.rm = na.rm, dots = list(...))
}

#' @rdname transform_smooth
#' @export
#' @inheritParams branch_histogram
branch_smooth <- function(props = NULL, se = TRUE, ...) {
  line_props <- props(x ~ x, y ~ y)
  se_props <- props(x ~ x, y ~ y_lower__, y2 ~ y_upper__, fillOpacity = 0.2)
  
  node(
    transform_smooth(..., se = se),
    if (se) mark_area(se_props, props),
    mark_line(line_props, props)
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
    x$formula <- if (x$method == "gam") y ~ s(x) else y ~ x
    environment(x$formula) <- globalenv()
    message("Guess transform_smooth(formula = ", deparse(x$formula), ")")
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

  env <- new.env(parent = environment(trans$formula))
  env$data <- data.frame(
    x = prop_value(x_var, data),
    y = prop_value(y_var, data)
  )

  # Create model call and combine with ... captured earlier
  call <- c(list(as.name(trans$method), trans$formula, data = quote(data)),
    trans$dots)
  model <- eval(as.call(call), env)

  # Make prediction
  x_grid <- seq(min(env$data$x), max(env$data$x), length = trans$n)
  predict_df(model, x_grid, trans$se, trans$level)
}

# Helper function to create data frame of predictions -------------------------

predict_df <- function(model, x_grid, se = se, level = level) UseMethod("predict_df")

#' @S3method predict_df lm
predict_df.lm <- function(model, x_grid, se, level) {
  grid <- data.frame(x = x_grid)
  pred <- predict(model, newdata = grid, se = se,
    level = level, interval = if(se) "confidence" else "none")
  
  if (se) {
    fit <- as.data.frame(pred$fit)
    names(fit) <- c("y__", "y_lower__", "y_upper__")
    grid <- cbind(grid, fit, se = pred$se)
  } else {
    grid$y <- as.vector(pred)
  }
  grid
}

#' @S3method predict_df loess
predict_df.loess <- function(model, x_grid, se, level) {
  grid <- data.frame(x = x_grid)
  pred <- predict(model, newdata = grid, se = se)

  if (se) {
    grid$y <- pred$fit
    ci <- pred$se.fit * qt(level / 2 + .5, pred$df)
    grid$y_lower__ <- grid$y - ci
    grid$y_upper__ <- grid$y + ci
    grid$se__ <- pred$se.fit
  } else {
    grid$y <- as.vector(pred)
  }
  grid
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
