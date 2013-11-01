library(ggvis)

# Scatter plot with brushing
ggvis(mtcars, props(x = ~wt, y = ~mpg),
  mark_symbol(props(size.brush := 400)),
  branch_brush()
)

# Bar graph with brushing
ggvis(
  pressure,
  props(x = ~temperature, y = ~pressure),
  dscale("x", "nominal", range = "width", padding = 0, points = FALSE),
  mark_rect(props(y2 = 0, width = band(), fill.brush := "red")),
  branch_brush()
)
