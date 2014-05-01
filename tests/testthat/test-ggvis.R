context("ggvis")

test_that("plot the same regardless of order data/props added", {
  df <- data.frame(x = 1:10, y = 10:1)
  plots <- list(
    df %>% ggvis(~x, ~y) %>% layer_points(),
    NULL %>% ggvis(~x, ~y) %>% add_data(df) %>% layer_points(),
    NULL %>% ggvis(~x, ~y) %>% layer_points(data = df),
    ggvis() %>% add_data(df) %>% add_props(~x, ~y) %>% layer_points(),
    ggvis() %>% add_data(df) %>% add_props(~x) %>% layer_points(y = ~y)
  )
  marks <- lapply(plots, function(x) x$marks[[1]])
  props <- lapply(marks, "[[", "props")
  data  <- lapply(marks, function(x) x$data())

  for (i in seq_along(marks)) {
    expect_equal(props[[i]][c("x.update", "y.update")], props(~x, ~y))
    expect_equal(data[[i]], df)
  }
})
