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
    x ~ val
  }

  p <- props(w ~ val, gen_formula(), y ~ 5, z = 6)

  # Should get val from this environment, where w~val was defined
  expect_identical(prop_value(p$w, dat), 11:12)

  # Should get val from gen_formula environment, where x~val was defined
  expect_identical(prop_value(p$x, dat), 21:22)
})
