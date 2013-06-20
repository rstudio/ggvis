context("pipeline")

test_that("creating and concatenating pipelines", {
  x <- pipeline(mtcars, transform_bin())
  expect_equal(length(x), 2)
  expect_true(inherits(x[[1]], "source_eager"))
  expect_true(inherits(x[[2]], "transform_bin"))

  # Creating and concatenating have the same effect
  expect_identical(
    x,
    c(pipeline(mtcars), pipeline(transform_bin()))
  )

  # Adding pipe items directly (not wrapped in pipeline()) works
  expect_identical(
    x,
    c(pipeline(mtcars), transform_bin())
  )

  # Pipelines with NULL are dropped from c()
  expect_identical(
    c(as.pipeline(NULL), transform_bin(), pipeline(NULL)),
    pipeline(transform_bin())
  )
})

test_that("pipeline objects are trimmed to sources", {

  x <- c(pipeline(mtcars), pipeline(transform_bin()),
         pipeline(pressure, transform_smooth()))
  expect_equal(length(x), 2)
  expect_true(inherits(x[[1]], "source_eager"))
  expect_equal(names(x[[1]]$data), c("temperature", "pressure"))
  expect_true(inherits(x[[2]], "transform_smooth"))

  # Constructing pipelines with c is the same as creating them
  expect_identical(
    x,
    pipeline(mtcars, transform_bin(), pressure, transform_smooth())
  )
})

test_that("pipeline id", {
  p <- pipeline("mtcars")
  expect_identical(pipeline_id(p), "mtcars")

  p <- pipeline(source_eager(mtcars))
  expect_identical(pipeline_id(p), "mtcars")

  p <- pipeline(NULL)
  expect_identical(pipeline_id(p), NULL)

  p <- pipeline("mtcars", transform_bin())
  expect_true(grepl("^mtcars_bin_", pipeline_id(p)))
})
