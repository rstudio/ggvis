transform <- function(type, ...) {
  structure(list(...), class = c(paste0("transform_", type), "transform"))
}
print.transform <- function(x, ...) str(x)


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

#' @export
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


# Apply transformation to a data object, dispatching on data type
apply_transform <- function(data, transform, mapping) UseMethod("apply_transform")

#' @S3method apply_transform data.frame
apply_transform.data.frame <- function(data, transform, mapping) {
  compute(transform, data, mapping)
}

#' @S3method apply_transform split_data_dflist
apply_transform.split_data_dflist <- function(data, transform, mapping) {
  structure(
    lapply(data, function(x) compute(transform, x, mapping)),
    class = c("split_data_dflist", "split_data")
  )
}


# Compute transformation
compute <- function(transform, data, mapping) UseMethod("compute")

#' @S3method compute transform_smooth
compute.transform_smooth <- function(transform, data, mapping) {
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

  # Find columns where all entries have the same value
  same_cols <- vapply(data, all_same, FUN.VALUE = logical(1), USE.NAMES = TRUE)
  same_col_names <- names(data)[same_cols]

  call <- substitute(method(transform$formula, data = data), transform["method"])
  call <- modify_call(call, transform$dots)
  mod <- eval(call)

  xseq <- seq(min(data[[xvar]]), max(data[[xvar]]), length = transform$n)

  # Make prediction
  pred_data <- predictdf(mod, xseq, xvar, yvar, transform$se, transform$level)

  # Add back columns that all had the same value
  pred_data[same_col_names] <- data[1, same_col_names, drop = FALSE]

  pred_data
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
