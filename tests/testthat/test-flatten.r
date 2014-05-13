context("Flatten")

test_that("props inherited from parent", {
  p <- data.frame() %>% ggvis(x := 1) %>%
    add_props(y := 2) %>%
    layer_paths(x := 3)

  props <- p$marks[[1]]$props

  expect_equal(sort(names(props)), c("stroke.update", "x.update", "y.update"))
  expect_equal(props$x.update$value, 3)
  expect_equal(props$y.update$value, 2)
})

test_that("no data is an error", {
  base <- NULL %>% ggvis(x = ~x, y = ~y)
  expect_error(base %>% layer_paths(), "No data supplied to mark")
})

test_that("reactive source data only run once", {
  runs <- 0
  df <- data.frame(x = 1, y = 2)
  rdf <- shiny::reactive({
    runs <<- runs + 1
    df
  })

  p <- rdf %>% ggvis(~x, ~y) %>% layer_paths() %>% layer_points()
  expect_equal(length(p$data), 1)

  out_df <- shiny::isolate(p$data[[1]]())
  expect_equal(out_df, df)
  expect_equal(runs, 1)
})
