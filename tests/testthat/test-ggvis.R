context("ggvis")
# Recursively crawl a list and replace any environments with a special
# environment. This is a workaround for a change in behavior in R 3.2.0
# for all.equal when given two environments.
blank_envs <- function(x) {
  replacement_env <- new.env(parent = emptyenv())

  # Use `x[]<-` to preserve any attributes on x
  x[] <- lapply(x, function(val) {
    if (is.environment(val)) replacement_env
    else if (is.list(val)) blank_envs(val)
    else val
  })

  x
}

test_that("plot the same regardless of order data/props added", {

  df <- data.frame(x = 1:10, y = 10:1)
  plots <- list(
    df %>% ggvis(~x, ~y) %>% layer_points(),
    NULL %>% ggvis(~x, ~y) %>% add_data(df) %>% layer_points(),
    NULL %>% ggvis(~x, ~y) %>% layer_points(data = df),
    ggvis() %>% add_data(df) %>% add_props(~x, ~y) %>% layer_points(),
    ggvis() %>% add_data(df) %>% add_props(~x) %>% layer_points(y = ~y)
  )
  plots <- blank_envs(plots)

  marks <- lapply(plots, function(x) x$marks[[1]])
  props <- lapply(marks, "[[", "props")
  data  <- lapply(marks, function(x) x$data())

  for (i in seq_along(marks)) {
    expect_equal(
      props[[i]][c("x.update", "y.update")],
      blank_envs(props(~x, ~y))
    )
    expect_equal(data[[i]], df)
  }
})
