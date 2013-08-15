context("props")

test_that("property names for variables", {
  # pname(wt) is equivalent to prop_name(prop_var(quote(wt)))
  pname <- function(x) prop_name(prop_var(substitute(x)))

  # Regular variable names are simply converted to string
  expect_identical(pname(wt), "wt")

  # Variable names with special characters are made vega-safe
  expect_identical(pname(`wt/mpg`), paste0("wt/mpg"))
  expect_identical(pname(`wt.mpg`), paste0("wt\\.mpg"))

  # Expressions are made js-safe, with a hash
  expect_identical(pname(wt/mpg), paste0("[e]wt/mpg"))

  # Expressions and weird column names don't result in same name
  expect_false(pname(wt/mpg) == pname(`wt/mpg`))

  # Constants have no variable name
  expect_identical(pname(10), "")
  expect_identical(pname("wt"), "")
})

test_that("property values for variables", {
  # Create a data frame with columns 'a', 'b', and 'b-a'
  dat <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  names(dat)[3] <- "b-a"

  # Column named `b-a`
  expect_identical(prop_value(prop_var(quote(`b-a`)), dat), 7:9)

  # Expression b-a
  expect_equal(prop_value(prop_var(quote(b-a)), dat), c(3, 3, 3))

  # Constant value in a prop_var()
  expect_equal(prop_value(prop_var(10), dat), 10)
})
