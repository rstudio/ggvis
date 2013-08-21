context("ggvis_node")

test_that("multiple pipes concatenated into a pipline", {
  p <- ggvis_node(mtcars, transform_bin())$data
  expect_is(p, "pipeline")
  expect_equal(length(p), 2)
  
  expect_equal(p[[1]]$env$data, mtcars)
  expect_equal(p[[2]], transform_bin())
})

test_that("only last data set kept", {
  p <- ggvis_node(mtcars, sleep)$data
  expect_is(p, "pipeline")
  expect_equal(length(p), 1)
  expect_equal(p[[1]]$env$data, sleep)
  
  p <- ggvis_node(mtcars, transform_bin(), sleep)$data
  expect_is(p, "pipeline")
  expect_equal(length(p), 1)
  expect_equal(p[[1]]$env$data, sleep)
})

test_that("single pipeline preserved", {
  pl <- pipeline(mtcars, transform_bin())
  p <- ggvis_node(pl)$data
  
  expect_equal(p, pl)
})

test_that("multiple pipelines concatenated", {
  pl <- pipeline(mtcars, transform_bin())
  p <- ggvis_node(pl[1], pl[2])$data
  
  expect_equal(p, pl)
})