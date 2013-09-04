context("transform_sort")

test_that("transform_sort works as expected", {
  # Default var, x
  dat <- sluice(pipeline(mtcars, transform_sort()), props(x = ~mpg))
  expect_identical(dat, mtcars[order(mtcars$mpg), ])

  # Specified var, y
  dat <- sluice(pipeline(mtcars, transform_sort(var = "y")), props(y = ~mpg))
  expect_identical(dat, mtcars[order(mtcars$mpg), ])

  # Multiple vars, x and y
  dat <- sluice(pipeline(mtcars, transform_sort(var = c("y", "x"))),
    props(x = ~mpg, y = ~hp, z = ~qsec))
  expect_identical(dat, mtcars[order(mtcars$hp, mtcars$mpg), ])

  # Pass args to order()
  dat <- sluice(pipeline(mtcars, transform_sort(decreasing = TRUE)),
    props(x = ~mpg))
  expect_identical(dat, mtcars[order(mtcars$mpg, decreasing = TRUE), ])
})

test_that("transform_sort objects are not considered empty", {
  expect_false(empty(transform_sort()))
})
