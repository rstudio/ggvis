context("pipeline")

test_that("creating and concatenating pipelines", {
  x <- pipeline(mtcars, transform_bin())
  expect_equal(length(x), 2)
  expect_true(inherits(x[[1]], "datasource"))
  expect_true(inherits(x[[2]], "transform_bin"))

  # Creating and concatenating have the same effect
  expect_equal(
    x,
    c(pipeline(mtcars), pipeline(transform_bin()))
  )

  # Adding pipe items directly (not wrapped in pipeline()) works
  expect_equal(
    x,
    c(pipeline(mtcars), transform_bin()),
    check.attributes = FALSE
  )

  # Pipelines with NULL are dropped from c()
  expect_equal(
    c(as.pipeline(NULL), transform_bin(), pipeline(NULL)),
    pipeline(transform_bin()),
    check.attributes = FALSE
  )
})

test_that("pipeline objects are trimmed to sources", {

  x <- c(pipeline(mtcars), pipeline(transform_bin()),
         pipeline(pressure, transform_smooth()))
  expect_equal(length(x), 2)
  expect_true(inherits(x[[1]], "datasource"))
  expect_equal(names(x[[1]]$env$data), c("temperature", "pressure"))
  expect_true(inherits(x[[2]], "transform_smooth"))

  # Constructing pipelines with c is the same as creating them
  x2 <- pipeline(mtcars, transform_bin(), pressure, transform_smooth())
  expect_equal(x[[1]], x2[[1]])
  expect_equal(x[[2]], x2[[2]])
})

test_that("pipeline id", {
  props <- props(x = ~wt, y = ~mpg)

  p <- pipeline(mtcars)
  expect_identical(pipeline_id(p, props), "mtcars_a63c70e73b58d0823ab3bcbd3b543d6f")

  p <- pipeline(NULL)
  expect_identical(pipeline_id(p, props), NULL)

  p <- pipeline(mtcars, transform_bin())
  expect_true(grepl("^mtcars_.*_bin_", pipeline_id(p, props)))
})
