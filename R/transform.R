transform <- function(type, ...) {
  structure(list(...), class = c(paste0("transform_", type), "transform"))
}
print.transform <- function(x, ...) str(x)


transform_smooth <- function(method = "auto", formula = "auto", se = TRUE,
                             level = 0.95, n = 80L, na.rm = FALSE, ...) {
  assert_that(is.string(method))
  # assert_that(is.formula(formula) || identical(formula, "auto"))
  assert_that(is.flag(se))
  assert_that(is.numeric(level), length(level) == 1, level >= 0, level <= 1)
  assert_that(is.integer(n), length(n) == 1, n >= 0)
  assert_that(is.flag(na.rm))

  transform("smooth", method = method, formula = formula, se = se,
    level = level, n = n, na.rm = na.rm, dots = dots(...))
}

transform_bin <- function(binwidth = 1) {
  transform("bin", binwidth = binwidth)
}


# Returns a string representing the transform type. For example, if it has
# class "transform_smooth", then this returns "smooth".
transform_type <- function(transform) {
  classes <- class(transform)
  type <- classes[grep("^transform_", classes)]
  sub("^transform_", "", type)
}


compute <- function(transform, data, mapping) UseMethod("compute")

#' @S3method compute transform_smooth
compute.transform_smooth <- function(transform, data, mapping) {
  if (transform$method == "auto") {
    transform$method <- if (nrow(data) > 1000) "gam" else "loess"
  }
  if (transform$formula == "auto") {
    transform$formula <- if(transform$method == "gam") y ~ s(x) else y ~ x
  }
  transform$method <- as.name(transform$method)


  # Rename the columns specified in mapping. For example, if mapping is
  # c(x="wt", y="mpg"), then this renames data's 'wt' column to 'x', and
  # 'mpg' to 'y'.
  names(data)[match(mapping, names(data))] <- names(mapping)

  call <- substitute(method(transform$formula, data = data), transform["method"])
  call <- modify_call(call, transform$dots)
  mod <- eval(call)

  xseq <- seq(min(data$x), max(data$x), length = transform$n)

  # Make prediction
  pred_data <- predictdf(mod, xseq, transform$se, transform$level)

  # Rename the columns back to original names
  names(pred_data)[match(names(mapping), names(pred_data))] <- mapping

  pred_data
}



predictdf <- function(model, xseq, se, level) UseMethod("predictdf")

#' @S3method predictdf default
predictdf.default <- function(model, xseq, se, level) {
  pred <- stats::predict(model, newdata = data.frame(x = xseq), se = se,
    level = level, interval = if(se) "confidence" else "none")

  if (se) {
    fit <- as.data.frame(pred$fit)
    names(fit) <- c("y", "ymin", "ymax")
    data.frame(x = xseq, fit, se = pred$se)
  } else {
    data.frame(x = xseq, y = as.vector(pred))
  }
}

#' @S3method predictdf loess
predictdf.loess <- function(model, xseq, se, level) {
  pred <- stats::predict(model, newdata = data.frame(x = xseq), se = se)

  if (se) {
    y = pred$fit
    ci <- pred$se.fit * qt(level / 2 + .5, pred$df)
    ymin = y - ci
    ymax = y + ci
    data.frame(x = xseq, y, ymin, ymax, se = pred$se.fit)
  } else {
    data.frame(x = xseq, y = as.vector(pred))
  }
}
