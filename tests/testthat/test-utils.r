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
  expect_false(all_same(1:2))
  expect_true(all_same(1))
  expect_true(all_same(c(1, 1)))
  expect_true(all_same(NA))
  expect_true(all_same(c(NA, NA)))
  expect_false(all_same(c(NA, 1)))

  # Zero-length vector
  expect_true(all_same(character()))

  # List (instead of atomic vector)
  expect_false(all_same(list()))
  expect_false(all_same(list(numeric(0))))
})


test_that("empty behaves as expected", {
  expect_true(empty(character(0)))
  expect_true(empty(logical(0)))
  expect_true(empty(list()))
  expect_true(empty(new.env()))

  # Named list with no elements
  x <- list(a=1)
  x$a <- NULL
  expect_true(empty(x))

  expect_false(empty(100))
  expect_false(empty(list(40)))
  e <- new.env()
  e$x <- 10
  expect_false(empty(e))
})


test_that("compact behaves as expected", {
  # Compact drops empty vectors and lists, but keeps environments even if empty
  e <- new.env()
  e$x <- 10
  ne <- new.env()
  x <- list(a = 1, b = list(), c = character(0), e = e, ne = ne)
  expect_identical(compact(x), list(a = 1, e = e, ne = ne))
})
