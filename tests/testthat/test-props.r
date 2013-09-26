context("props")

test_that("property names for variables", {
  # pname(wt) is equivalent to prop_name(prop(quote(wt)))
  pname <- function(x) prop_name(prop(substitute(x)))

  # Regular variable names are simply converted to string
  expect_identical(pname(wt), "wt")

  # Variable names with special characters are made vega-safe
  expect_identical(pname(`wt/mpg`), "wt/mpg")
  expect_identical(pname(`wt.mpg`), "wt\\.mpg")

  # Expressions are kept as is
  expect_identical(pname(wt/mpg), "wt/mpg")

  # Expressions and weird column names will produce same name
  # (but this is very unlikely)
  expect_true(pname(wt/mpg) == pname(`wt/mpg`))

  # Constants have no variable name
  expect_identical(pname(10), "")
  expect_identical(pname("wt"), "")
})

test_that("property values for variables", {
  # Create a data frame with columns 'a', 'b', and 'b-a'
  dat <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  names(dat)[3] <- "b-a"

  # Column named `b-a`
  expect_identical(prop_value(prop(quote(`b-a`)), dat), 7:9)

  # Expression b-a
  expect_equal(prop_value(prop(quote(b-a)), dat), c(3, 3, 3))

  # Constant prop
  expect_equal(prop_value(prop(10), dat), rep(10, 3))

  # Expression which returns a single value; value should be replicated
  expect_equal(prop_value(prop(quote(min(a))), dat), c(1, 1, 1))

  # Expression which returns # of values that doesn't divide into # of rows
  expect_error(prop_value(prop(quote(range(a))), dat))
})

test_that("prop captures environment for evaluation", {
  dat <- data.frame(a = 1:2, b = 3:4, c = 5:6)
  b <- 11:12
  d <- 13:14

  # Column from data
  expect_identical(prop_value(prop(quote(a)), dat), 1:2)

  # Column from data, even though a var in the calling env has same name
  expect_identical(prop_value(prop(quote(b)), dat), 3:4)

  # Variable in calling environment
  expect_identical(prop_value(prop(quote(d)), dat), 13:14)

  # Column from data with variable in calling environment
  expect_identical(prop_value(prop(quote(b+d)), dat), 3:4 + 13:14)

  # Column from data with variable in calling environment
  expect_identical(prop_value(prop(quote(b+d)), dat), 3:4 + 13:14)


  # Create new environment for storing variables
  env <- new.env()
  env$d <- 23:24

  # Variable in env
  expect_identical(prop_value(prop(quote(d), env = env), dat), 23:24)
  # Column from data, even though a var in env has same name
  expect_identical(prop_value(prop(quote(b), env = env), dat), 3:4)
})

test_that("props uses environment in formulas", {
  dat <- data.frame(a = 1:2, b = 3:4)
  val <- 11:12

  # Create a formula that captures the function environment
  gen_formula <- function() {
    val <- 21:22
    ~ val
  }

  p <- props(w = ~val, x := gen_formula(), y := 5, z := 6)

  # Should get val from this environment, where w~val was defined
  expect_identical(prop_value(p$w, dat), 11:12)

  # Should get val from gen_formula environment, where x~val was defined
  expect_identical(prop_value(p$x, dat), 21:22)
})

test_that("merging props", {
  # Utility function: sort props by name
  sortp <- function(p) p[sort(names(p))]

  p_i  <- props(x=~a, z:="red")
  q_i  <- props(y=~b, z:="blue")
  q_ni <- props(y=~b, z:="blue", inherit=FALSE)

  expect_equal(sortp(merge_props(p_i, q_i)), props(x=~a, y=~b, z:="blue"))
  expect_equal(sortp(merge_props(p_i, q_ni)), q_ni)
  expect_equal(sortp(merge_props(q_ni, p_i)),
    props(x=~a, y=~b, z:="red", inherit = FALSE))
})

test_that("prop_sets splits up props properly", {
  p <- props(x.update = ~wt, x.enter = 1, y = ~mpg, fill.hover := "red")
  ps <- prop_sets(p)

  expect_equal(names(ps), c("enter", "update", "hover"))
  expect_equal(names(ps$enter), "x")
  expect_equal(names(ps$update), c("x", "y"))
  expect_equal(names(ps$hover), "fill")

  expect_identical(ps$enter$x, prop(1, scale = TRUE))
  expect_identical(ps$update$x, prop(quote(wt)))
  expect_identical(ps$update$y, prop(quote(mpg)))
  expect_identical(ps$hover$fill, prop("red"))

  expect_true(attr(ps$enter, "inherit"))
  expect_true(attr(ps$update, "inherit"))
  expect_true(attr(ps$hover, "inherit"))

  # value of inherit is passed along
  p <- props(x = ~wt, x.enter = 1, inherit = FALSE)
  ps <- prop_sets(p)
  expect_false(attr(ps$enter, "inherit"))
  expect_false(attr(ps$update, "inherit"))
})

test_that("drop_props", {
  p <- props(x = ~wt, x.enter = 0, stroke.enter := "black", stroke.hover := "red")

  expect_identical(
    drop_props(p, c("stroke", "strokeOpacity")),
    props(x = ~wt, x.enter = 0))

  expect_identical(
    drop_props(p, c("x", "stroke")),
    props())
})
