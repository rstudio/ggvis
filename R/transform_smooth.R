#' @export
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


#' @S3method apply_transform transform_bin
apply_transform.transform_smooth <- function(transform, data, mapping) {
  # We've dispatched on transform type, now dispatch on data type
  compute_transform_smooth(data, transform, mapping)
}


compute_transform_smooth <- function(data, transform, mapping)
  UseMethod("compute_transform_smooth")

#' @S3method compute_transform_smooth split_df
compute_transform_smooth.split_df <- function(data, transform, mapping) {
  # Run compute_transform_smooth on each data frame in the list
  data <- structure(
    lapply(data, compute_transform_smooth, transform = transform, mapping = mapping),
    class = "split_df"
  )
}

#' @S3method compute_transform_smooth data.frame
compute_transform_smooth.data.frame <- function(data, transform, mapping) {
  xvar <- mapping["x"]
  yvar <- mapping["y"]

  if (transform$method == "auto") {
    transform$method <- if (nrow(data) > 1000) "gam" else "loess"
  }
  if (transform$formula == "auto") {
    if (transform$method == "gam")
      transform$formula <- as.formula(paste(yvar, "~ s(", xvar, ")"))
    else
      transform$formula <- as.formula(paste(yvar, "~", xvar))
  }
  transform$method <- as.name(transform$method)

  # Identify constant variables, extract and add back in
  constant_vars <- vapply(data, all_same, logical(1))

  call <- substitute(method(transform$formula, data = data), transform["method"])
  call <- modify_call(call, transform$dots)
  mod <- eval(call)

  xseq <- seq(min(data[[xvar]]), max(data[[xvar]]), length = transform$n)

  # Make prediction
  transformed <- predictdf(mod, xseq, xvar, yvar, transform$se, transform$level)

  # Add back the constant variables
  carry_over <- data[1, constant_vars, drop = FALSE]
  rownames(carry_over) <- NULL
  cbind(transformed, carry_over)
}

#' @S3method compute_transform_smooth default
compute_transform_smooth.default <- function(data, transform, mapping) {
  stop("Don't know how to compute_transform_smooth for data structure with class ",
    paste(class(data), sep = ", "))
}


predictdf <- function(model, xseq, xvar, yvar, se, level) UseMethod("predictdf")

#' @S3method predictdf default
predictdf.default <- function(model, xseq, xvar, yvar, se, level) {
  dat <- data.frame(v1 = xseq)
  names(dat) <- xvar
  pred <- stats::predict(model, newdata = dat, se = se,
    level = level, interval = if(se) "confidence" else "none")

  if (se) {
    fit <- as.data.frame(pred$fit)
    names(fit) <- c(yvar, paste0("_", yvar, "_min"), paste0("_", yvar, "_max"))
    dat <- cbind(dat, fit, se = pred$se)
  } else {
    dat[[yvar]] <- as.vector(pred)
  }
  dat
}

#' @S3method predictdf loess
predictdf.loess <- function(model, xseq, xvar, yvar, se, level) {
  dat <- data.frame(v1 = xseq)
  names(dat) <- xvar
  pred <- stats::predict(model, newdata = dat, se = se)

  if (se) {
    dat[[yvar]] <- pred$fit
    ci <- pred$se.fit * qt(level / 2 + .5, pred$df)
    dat[[paste0("_", yvar, "_min")]] <- y - ci
    dat[[paste0("_", yvar, "_max")]] <- y + ci
    dat$se <- pred$se.fit
  } else {
    dat[[yvar]] <- as.vector(pred)
  }
  dat
}
