library(ggvis)

# Scatter plot with brushing
ggvis(mtcars, props(x = ~wt, y = ~mpg)) +
  mark_symbol(props(size.brush := 400)) +
  branch_brush()

# Bar graph with brushing
ggvis(pressure, props(x = ~temperature, y = ~pressure)) +
  dscale("x", "nominal", range = "width", padding = 0, points = FALSE) +
  mark_rect(props(y2 = 0, width = band(), fill.brush := "red")) +
  branch_brush()

# Brushing with 10000 points
data("diamonds", package="ggplot2")
d <- diamonds[sample(nrow(diamonds), 10000), ]
ggvis(d, props(x = ~carat, y = ~price)) +
  mark_symbol(props(size := 40, fillOpacity := 0.02, fillOpacity.brush := 0.4)) +
  branch_brush()
