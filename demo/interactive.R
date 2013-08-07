library(gigvis)

# Slider and select input in a transform
gigvis(mtcars, props(x ~ wt, y ~ mpg),
  mark_symbol(),
  branch_smooth(
    n = input_slider(2, 80, value = 5, step = 1, label = "Interpolation points"),
    method = input_select(c("Linear" = "lm", "LOESS" = "loess"), label = "Method")
  )
)

gigvis(mtcars, props(x ~ wt, y ~ mpg),
  mark_symbol(),
  branch_smooth(
    method = "loess", formula = mpg ~ wt,
    span = input_slider(0.2, 1, label = "span"))
)
