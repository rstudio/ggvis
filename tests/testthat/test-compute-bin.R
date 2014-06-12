context("compute_bin")

test_that("tilelayer algorithm is working", {
  x <- rnorm(100)
  width <- runif(1, 0, max(x)/2)
  origin <- tilelayer_origin(range(x), width)
  expect_true( origin <= min(x) - 1e-8 )
  expect_true( abs(origin - min(x)) <= width / 2 - 1e-8)
  x <- round(rnorm(100) * 100)
  width <- runif(1, 0, max(x)/2)
  origin <- tilelayer_origin(range(x), width)
  expect_true( origin <= min(x) - 1e-8 )
  expect_true( abs(origin - min(x)) <= width / 2  - 1e-8)
})

