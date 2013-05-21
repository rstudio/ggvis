context("utils")


test_that("merge_vectors behaves as expected", {
  expect_identical(merge_vectors(c(a=1, b=2), c(c=3, d=4)), c(a=1, b=2, c=3, d=4))

  # b appears twice; keep last version
  expect_identical(merge_vectors(c(a=1, b=2), c(b=3, c=4)), c(a=1, b=3, c=4))
  expect_identical(merge_vectors(c(a=1), c(b=2, b=3, c=4)), c(a=1, b=3, c=4))

  # NULL value
  expect_identical(merge_vectors(NULL, c(a=1, b=2)), c(a=1, b=2))
  expect_identical(merge_vectors(c(a=1, b=2), NULL), c(a=1, b=2))

  expect_error(merge_vectors(c(1, 2), c(1, 2)))
  expect_error(merge_vectors(c(1, 2), c(a=1, b=2)))
  expect_error(merge_vectors(c(a=1, b=2), c(1, 2)))
})


test_that("all_same behaves as expected", {
  expect_identical(all_same(1:2), FALSE)
  expect_identical(all_same(1), TRUE)
  expect_identical(all_same(c(1, 1)), TRUE)
  expect_identical(all_same(NA), TRUE)
  expect_identical(all_same(c(NA, NA)), TRUE)
  expect_identical(all_same(c(NA, 1)), FALSE)

  # Zero-length vector
  expect_identical(all_same(character()), TRUE)
})
