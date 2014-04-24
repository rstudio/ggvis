#' Transformation: smooth the data
#'
#' \code{transform_smooth} is a data transformation that fits a model to a 2d
#' dataset and computes predictions and standard errors. \code{layer_smooth}
#' combines \code{transform_smooth} with \code{mark_path} and \code{mark_area}
#' to display a smooth line and its standard errror.
#'
#' @section Input:
#' Currently \code{transform_smooth} only works with data frames and requires the
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
#' @param ... \code{transform_smooth}: named arguments passed on to
#'  \code{method} function, unnamed arguments ignored.
#'  \code{layer_smooth}: named arguments are passed on to the transform,
#'  (and hence to \code{method}), unnamed arguments are passed on to
#'  \code{\link{layer}}.
#' @export
#' @examples
#' mtcars %>% ggvis(~wt, ~mpg) %>% layer_point() %>% layer_smooth()
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
transform_smooth <- function(vis, ..., method = NULL, formula = NULL,
                             se = TRUE, level = 0.95, n = 80L, na.rm = FALSE) {
  assert_that(is.ggvis(vis))

  # Register reactive arguments
  dots <- list(...)
  dots <- dots[named(dots)]
  vis <- register_reactives(vis, c(dots, method, formula, se, level, n, na.rm))

  # Get the current data and props from the parent
  parent_data <- vis$cur_data
  parent_props <- vis$cur_props

  # Compute default values
  if (is.null(method)) {
    data <- shiny::isolate(parent_data())
    method <- if (max_rows(data) > 1000) "gam" else "loess"
    notify_guess(method)
  }
  if (method == "gam") try_require("mgcv")

  if (is.null(formula)) {
    formula <- if (method == "gam") y ~ s(x) else y ~ x
    environment(formula) <- globalenv()
    notify_guess(formula)
  }

  # Generate reactive transformation
  new_data <- reactive({
    data <- parent_data()

    check_prop("transform_smooth", parent_props, data, "x.update",
      c("numeric", "datetime"))
    check_prop("transform_smooth", parent_props, data, "y.update", "numeric")

    output <- compute_smooth(data, parent_props$x.update, parent_props$y.update,
      value(method), value(formula), value(se), value(level), value(n),
      value(na.rm), lapply(dots, value))

    preserve_constants(data, output)
  })

  # Save data in the vis object, updating current data.
  # FIXME: modify register_data so this creates informative name.
  # Maybe use slashes to make the data hierarchy more clear?
  register_data(vis, new_data, "_transform_smooth")
}

notify_guess <- function(x) {
  message("Guessing ", deparse(substitute(x)), " = ", format(x))
}

#' @rdname transform_smooth
#' @export
layer_smooth <- function(vis, ..., se = FALSE) {
  comps <- parse_components(..., drop_named = TRUE)

  line_props <-  merge_props(props(x = ~x, y = ~y, strokeWidth := 2), comps$props)
  se_props <- merge_props(props(x = ~x, y = ~y_lower__, y2 = ~y_upper__,
    fillOpacity := 0.2), comps$props)

  # Line shouldn't get fill-related props, and se area shouldn't get
  # stroke-related props.
  line_props <- drop_props(line_props, c("fill", "fillOpacity"))
  se_props <- drop_props(se_props, c("stroke", "strokeOpacity"))

  vis %>%
    branch(
      transform_smooth(..., se = se) %>%
      function(vis) {
        if (!identical(se, FALSE)) vis %>% mark_area(se_props)
        else vis
      } %>%
      mark_path(line_props)
    )
}


compute_smooth <- function(data, x_var, y_var, method, formula, se, level, n,
                   na.rm, dots) {
  UseMethod("compute_smooth")
}

#' @export
compute_smooth.grouped_df <- function(data, x_var, y_var, method, formula, se,
                              level, n, na.rm, dots) {
  dplyr::do(data, compute_smooth(., x_var, y_var, method, formula, se,
                                 level, n, na.rm, dots))
}

#' @export
compute_smooth.data.frame <- function(data, x_var, y_var, method, formula, se,
                              level, n, na.rm, dots) {
  assert_that(is.formula(formula))
  assert_that(is.flag(se))
  assert_that(is.numeric(level), length(level) == 1, level >= 0, level <= 1)
  assert_that(length(n) == 1, n >= 0)
  assert_that(is.flag(na.rm))


  env <- new.env(parent = environment(formula))
  old_x_val <- prop_value(x_var, data)
  env$data <- remove_missing(data.frame(
    x = as.numeric(old_x_val),
    y = prop_value(y_var, data)
  ))

  # Create model call and combine with ... captured earlier
  call <- c(list(as.name(method), formula, data = quote(data)), dots)
  model <- eval(as.call(call), env)

  # Make prediction
  x_grid <- seq(min(env$data$x), max(env$data$x), length = n)
  pred <- predict_df(model, x_grid, se, level)

  # Coerce x value back to the original type, if applicable.
  # This may need to be abstracted out later, so other transforms can use it.
  if (inherits(old_x_val, "Date")) {
    pred$x <- as.Date(pred$x, origin = "1970-01-01")
  } else if (inherits(old_x_val, "POSIXct")) {
    pred$x <- as.POSIXct(pred$x, origin = "1970-01-01 00:00.00 UTC",
      tz = attr(old_x_val, "tzone"))
  }

  pred
}

# Helper function to create data frame of predictions -------------------------

predict_df <- function(model, x_grid, se = se, level = level) UseMethod("predict_df")

#' @export
predict_df.lm <- function(model, x_grid, se, level) {
  grid <- data.frame(x = x_grid)
  pred <- predict(model, newdata = grid, se = se,
    level = level, interval = if(se) "confidence" else "none")

  if (se) {
    fit <- as.data.frame(pred$fit)
    names(fit) <- c("y", "y_lower__", "y_upper__")
    grid <- cbind(grid, fit, se = pred$se)
  } else {
    grid$y <- as.vector(pred)
  }
  grid
}

#' @export
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
#' @export
max_rows.data.frame <- function(x) nrow(x)
#' @export
max_rows.grouped_df <- function(x) {
  mtcars %>% group_by(cyl) %>% group_size() %>% max()
}
