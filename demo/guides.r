library(ggvis)

ggvis(mtcars, props(x = ~wt, y = ~mpg, fill = ~cyl)) +
  layer_point()

ggvis(mtcars, props(x = ~wt, y = ~mpg, fill = ~cyl)) +
  layer_point() +
  guide_axis("x", title = "Weight")

ggvis(mtcars, props(x = ~wt, y = ~mpg, fill = ~cyl)) +
  layer_point() +
  guide_legend(fill = "fill", title = "Cylinders")
