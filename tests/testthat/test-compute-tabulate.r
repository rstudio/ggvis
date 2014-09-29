context("compute_tabulate")

test_that("Zero-row inputs", {
  res <- mtcars[0,] %>% compute_tabulate(~factor(cyl))
  expect_equal(nrow(res), 0)
  expect_true(setequal(names(res), c("count_", "x_")))

  # Grouped
  res <- mtcars %>% group_by(cyl) %>% dplyr::filter(FALSE) %>% compute_tabulate(~factor(cyl))
  expect_equal(nrow(res), 0)
  expect_true(setequal(names(res), c("cyl", "count_", "x_")))
})
