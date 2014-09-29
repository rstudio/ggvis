context("compute_boxplot")

test_that("Zero-row inputs", {
  res <- mtcars[0,] %>% compute_boxplot(~mpg)
  expect_equal(nrow(res), 0)
  expect_true(setequal(
    names(res),
    c("min_", "lower_", "median_", "upper_", "max_", "outliers_")
  ))
  expect_identical(res$outliers_, list())

  # Grouped
  res <- mtcars %>% group_by(cyl) %>% dplyr::filter(FALSE) %>% compute_boxplot(~mpg)
  expect_equal(nrow(res), 0)
  expect_true(setequal(
    names(res),
    c("cyl", "min_", "lower_", "median_", "upper_", "max_", "outliers_")
  ))
  expect_identical(res$outliers_, list())
})
