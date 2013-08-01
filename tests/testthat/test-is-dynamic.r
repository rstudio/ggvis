context("is.dynamic")
library(shiny)

test_that("regular plot is not dynamic", {
  p <- gigvis(mtcars, props(x ~ wt, y ~ cyl), mark_symbol())
  expect_false(is.dynamic(p))
})

test_that("plot with reactive data source is dynamic", {
  p <- gigvis(reactive(mtcars), props(x ~ wt, y ~ cyl), mark_symbol())
  expect_true(is.dynamic(p))
})

test_that("plot with reactive transform param is dynamic", {
  bin <- transform_bin(binwidth = reactive(x))
  p <- gigvis(pipeline(mtcars, bin), props(x ~ wt, y ~ cyl), mark_symbol())
  expect_true(is.dynamic(p))
})