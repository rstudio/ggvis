context("props")

test_that("property names for variables", {
  # pname(wt) is equivalent to prop_name(variable(quote(wt)))
  pname <- function(x) prop_name(variable(substitute(x)))

  # Regular variable names are simply converted to string
  expect_identical(pname(wt), "wt")

  # Variable names with special characters are made js-safe, with a hash
  expect_identical(pname(`wt/mpg`),
                   paste0("wtmpg_", digest::digest("wt/mpg", algo="crc32")))

  # Expressions are made js-safe, with a hash
  expect_identical(pname(wt/mpg),
                   paste0("ewtmpg_", digest::digest("[e] wt/mpg", algo="crc32")))

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
  expect_identical(prop_value(variable(quote(`b-a`)), dat), 7:9)

  # Expression b-a
  expect_equal(prop_value(variable(quote(b-a)), dat), c(3, 3, 3))

  # Constant value in a variable()
  expect_equal(prop_value(variable(10), dat), 10)
})
