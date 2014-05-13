context("is.dynamic")
library(shiny)

test_that("regular plot is not dynamic", {
  p <- mtcars %>% ggvis(x = ~wt, y = ~cyl) %>% layer_points()
  expect_false(is.dynamic(p))
})

test_that("plot with reactive data source is dynamic", {
  p <- shiny::reactive(mtcars) %>% ggvis(x = ~wt, y = ~cyl) %>% layer_points()
  expect_true(is.dynamic(p))
})

test_that("plot with reactive transform param is dynamic", {
  p <- mtcars %>% ggvis(~mpg, ~wt) %>% layer_smooths(span = input_slider(0, 1))
  expect_true(is.dynamic(p))
})
