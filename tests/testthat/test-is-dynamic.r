context("is.dynamic")
library(shiny)

test_that("regular plot is not dynamic", {
  p <- ggvis(mtcars, props(x ~ wt, y ~ cyl), mark_symbol())
  expect_false(is.dynamic(p))
})

test_that("plot with reactive data source is dynamic", {
  p <- ggvis(reactive(mtcars), props(x ~ wt, y ~ cyl), mark_symbol())
  expect_true(is.dynamic(p))
})

test_that("plot with reactive transform param is dynamic", {
  bin <- transform_bin(binwidth = reactive(x))
  p <- ggvis(pipeline(mtcars, bin), props(x ~ wt, y ~ cyl), mark_symbol())
  expect_true(is.dynamic(p))
})

test_that("all reactive parts of transform_dynamic found", {
  t1 <- transform_smooth(n = input_slider(10, 100))
  t2 <- transform_smooth(span = input_slider(0.1, 1))
  t3 <- transform_smooth(span = input_slider(0.1, 1), n = input_slider(10, 100))

  expect_true(is.dynamic(t1))
  expect_true(is.dynamic(t2))
  expect_true(is.dynamic(t3))
})
