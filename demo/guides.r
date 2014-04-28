library(ggvis)

ggvis(mtcars, props(x = ~wt, y = ~mpg, fill = ~cyl)) +
  layer_point()

mtcars %>% ggvis(x = ~wt, y = ~mpg, fill = ~cyl) %>%
  layer_point() %>%
  set_guide_axis("x", title = "Weight")

mtcars %>% ggvis(x = ~wt, y = ~mpg, fill = ~cyl) %>%
  layer_point() %>%
  set_guide_legend(fill = "fill", title = "Cylinders")
