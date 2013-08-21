context("utils")


test_that("merge_vectors behaves as expected", {
  expect_identical(merge_vectors(c(a=1, b=2), c(c=3, d=4)), c(a=1, b=2, c=3, d=4))

  # b appears twice; keep last version
  expect_identical(merge_vectors(c(a=1, b=2), c(b=3, c=4)), c(a=1, b=3, c=4))
  expect_identical(merge_vectors(c(a=1), c(b=2, b=3, c=4)), c(a=1, b=3, c=4))

  # NULL value
  expect_identical(merge_vectors(NULL, c(a=1, b=2)), c(a=1, b=2))
  expect_identical(merge_vectors(c(a=1, b=2), NULL), c(a=1, b=2))

  # Lists and empty lists
  expect_identical(merge_vectors(list(a=1, b=2), list(b=3, c=4)),
                   list(a=1, b=3, c=4))
  expect_identical(merge_vectors(list(a=1, b=2), list()),
                   list(a=1, b=2))
  expect_identical(merge_vectors(list(), list(a=1, b=2)),
                   list(a=1, b=2))

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


test_that("empty and compact behave as expected", {
  expect_true(empty(character(0)))
  expect_true(empty(logical(0)))
  expect_true(empty(list()))
  expect_true(empty(new.env()))

  expect_false(empty(100))
  expect_false(empty(list(40)))
  e <- new.env()
  e$x <- 10
  expect_false(empty(e))


  # Compact drops empty vectors and lists, but keeps environments even if empty
  ne <- new.env()
  x <- list(a = 1, b = list(), c = character(0), e = e, ne = ne)
  expect_identical(compact(x), list(a = 1, e = e, ne = ne))
})
