library(gigvis)
library(shiny)

# Slider and select input in a transform
gigvis(mtcars, props(x ~ wt, y ~ mpg),
  mark_symbol(),
  branch_smooth(
    n = input_slider(2, 80, "Interpolation points", value = 5, step = 1),
    method = input_select(c("Linear" = "lm", "LOESS" = "loess"))
  )
)
