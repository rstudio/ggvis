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


test_that("Zero-row inputs", {
  res <- mtcars[0,] %>% compute_density(~mpg)
  expect_equal(nrow(res), 0)
  expect_true(setequal(names(res), c("pred_", "resp_")))

  # Grouped
  res <- mtcars %>% group_by(cyl) %>% dplyr::filter(FALSE) %>% compute_density(~mpg)
  expect_equal(nrow(res), 0)
  expect_true(setequal(names(res), c("cyl", "pred_", "resp_")))
})
