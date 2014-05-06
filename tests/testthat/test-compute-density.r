context("compute_density")

test_that("compute_density respects arguments", {
  # Uses n
  res <- mtcars %>% compute_density(~mpg, n = 10)
  expect_equal(10, nrow(res))

  # When trim = FALSE (default), result goes past bounds of original data
  expect_true(min(res$pred_) < min(mtcars$mpg) && max(res$pred_) > max(mtcars$mpg))

  # When trim = TRUE, bounds of result match bounds of original data
  res <- mtcars %>% compute_density(~mpg, n = 10, trim = TRUE)
  expect_true(all(range(res$pred_) == range(mtcars$mpg)))
})
