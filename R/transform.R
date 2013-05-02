transform_smooth <- function(method = "auto", formula = "auto", se = TRUE,
                             level = 0.95, n = 80L, na.rm = FALSE, ...) {
  assert_that(is.string(method))
  # assert_that(is.formula(formula) || identical(formula, "auto"))
  assert_that(is.flag(se))
  assert_that(is.numeric(level), length(level) == 1, level >= 0, level <= 1)
  assert_that(is.integer(n), length(n) == 1, n >= 0)
  assert_that(is.flag(na.rm))

  list(method = method, formula = formula, se = se, level = level, n = n,
    na.rm = na.rm, dots = dots(...))
}
compute.transform_smooth <- function(x, data) {
  if (x$method == "auto") {
    x$method <- if (nrow(data) > 1000) "gam" else "loess"
  }
  if (x$formula == "auto") {
    x$formula <- if(x$method == "gam") y ~ s(x) else y ~ x
  }
  x$method <- as.name(x$method)

  call <- substitute(method(formula, data = data), x["method"])
  call <- modify_call(call, x$dots)
  mod <- eval(call)

  predictdf(mod, xseq, x$se, x$level)
}


transform_bin <- function(...) {

}
