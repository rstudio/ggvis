library(gigvis)
library(shiny)

# Slider
gigvis(mtcars, props(x ~ wt, y ~ mpg),
  mark_symbol(),
  branch_smooth(n = input_slider(label = "Number of interpolation points",
                                 value=5, min=2, max=80, step=1))
)
