context("pipeline")

test_that("creating and concatenating pipelines", {
  x <- pipeline(mtcars, transform_bin())
  expect_equal(length(x$pipes), 2)
  expect_true(inherits(x$pipes[[1]], "source_eager"))
  expect_true(inherits(x$pipes[[2]], "transform_bin"))

  expect_identical(
    x,
    c(pipeline(mtcars), pipeline(transform_bin()))
  )
})

test_that("pipeline objects are trimmed to sources", {

  x <- c(pipeline(mtcars), pipeline(transform_bin()),
         pipeline(pressure, transform_smooth()))
  expect_equal(length(x$pipes), 2)
  expect_true(inherits(x$pipes[[1]], "source_eager"))
  expect_equal(names(x$pipes[[1]]$data), c("temperature", "pressure"))
  expect_true(inherits(x$pipes[[2]], "transform_smooth"))

  # Constructing pipelines with c is the same as creating them
  expect_identical(
    x,
    pipeline(mtcars, transform_bin(), pressure, transform_smooth())
  )
})
