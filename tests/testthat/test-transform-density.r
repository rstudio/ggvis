context("transform_density")

test_that("transform_density works as expected", {
  # Uses n
  res <- sluice(pipeline(mtcars, transform_density(n = 10)), props(x = ~mpg))
  expect_equal(10, nrow(res))

  # When trim = FALSE (default), result goes past bounds of original data
  expect_true(min(res$x) < min(mtcars$mpg) && max(res$x) > max(mtcars$mpg))

  # When trim = TRUE, bounds of result match bounds of original data
  res <- sluice(pipeline(mtcars, transform_density(trim = TRUE)), props(x = ~mpg))
  expect_true(all(range(res$x) == range(mtcars$mpg)))


  # Warnings for NA; suppressed when na.rm=TRUE
  mtc <- mtcars
  mtc$mpg[5:10] <- NA
  expect_warning(sluice(pipeline(mtc, transform_density()), props(x = ~mpg)))
  expect_that(
    sluice(pipeline(mtc, transform_density(na.rm =TRUE)), props(x = ~mpg)),
    not(gives_warning())
  )
})
